//! Extern value semantics through a test host
//!
//! Each case extends the host specification, which declares `extern syntax state`,
//! and runs through AL, SL, and PL with nat 1 as the input to R.
//! The host makes `state` values from a key and a note;
//! payloads compare and hash by key alone,
//! so equal values may still carry distinct notes.
//! A second definition of `state`, made by the same host,
//! is never the one the specification's values are read through.
//! Each stage renders R's outputs or failure codes;
//! stages with equal transcripts share one block.

use std::{
    cmp::Ordering,
    hash::{Hash, Hasher},
    path::{Path, PathBuf},
};

use expect_test::expect_file;
use serde::Deserialize;

use p4spectec::{
    diagnostic::{Report, ReportKind},
    interface::p4::P4Interface,
    lang::{
        common::source::Span,
        data::{
            arena::Arena,
            value::{
                ValueError,
                extern_value::TypDef,
                flat::{self as value, Value},
            },
        },
        il::ast::Typ,
        traits::print::Print,
    },
    runner::{
        self, Config, Eval, Extern, ExternError, Interface, Interpreter, InterpreterError, Runner,
        RunnerContext, SideEffect,
    },
};

use crate::{Error, Result};

// = Registration

/// The host specification and the cases extending it.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Suite {
    pub host: PathBuf,
    pub cases: Vec<Case>,
}

/// A specification and the transcript expected from running it.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Case {
    pub name: String,
    pub input: PathBuf,
    pub expected: PathBuf,
    /// Type-check values at call boundaries.
    #[serde(default)]
    pub guard: bool,
}

fn failure(name: &str, message: impl std::fmt::Display) -> Error {
    Error::Invalid(format!("{name}: {message}"))
}

// = Host payloads

/// A `state` payload: a key that decides equality, and a note that does not.
#[derive(Clone, Debug)]
struct Tagged {
    key: String,
    note: String,
}

impl PartialEq for Tagged {
    fn eq(&self, tagged_other: &Self) -> bool {
        self.key == tagged_other.key
    }
}

impl Eq for Tagged {}

impl PartialOrd for Tagged {
    fn partial_cmp(&self, tagged_other: &Self) -> Option<Ordering> {
        Some(self.cmp(tagged_other))
    }
}

impl Ord for Tagged {
    fn cmp(&self, tagged_other: &Self) -> Ordering {
        self.key.cmp(&tagged_other.key)
    }
}

impl Hash for Tagged {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.key.hash(hasher);
    }
}

// = Test host

/// Makes and reads `state` values, counting reads.
struct Host {
    /// The definition the specification's values are read through.
    typdef_state: TypDef<Tagged>,
    /// Another definition with the same name and payload type.
    typdef_state_rogue: TypDef<Tagged>,
    /// Calls of `$note_state` so far.
    calls_note: u64,
}

impl Host {
    /// Defines both `state` definitions in `arena`.
    fn new(arena: &mut Arena) -> std::result::Result<Self, ValueError> {
        Ok(Self {
            typdef_state: TypDef::new(arena, "state")?,
            typdef_state_rogue: TypDef::new(arena, "state")?,
            calls_note: 0,
        })
    }
}

/// Reads a key and a note from two text arguments.
fn read_tagged(arena: &Arena, values: &[Value]) -> std::result::Result<Tagged, ValueError> {
    let (value_key, value_note) = value::get::two(values)?;
    let key = value::get::text(arena, value_key)?.to_owned();
    let note = value::get::text(arena, value_note)?.to_owned();
    Ok(Tagged { key, note })
}

impl Extern for Host {
    fn eval_rel<Interp, Iface>(
        _ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        _name: &str,
        _values: &[Value],
    ) -> Eval<Vec<Value>, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        Eval::new(Err(ExternError::diagnostic_unconfigured()), SideEffect::Impure)
    }

    fn eval_func<Interp, Iface>(
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        name: &str,
        _targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        match name {
            // Make a value through the definition the specification reads
            "make_state" => {
                let typdef = ctx.external().typdef_state;
                let result = read_tagged(ctx.arena(), values)
                    .and_then(|tagged| typdef.make(ctx.arena_mut(), tagged, Span::default()));
                Eval::new(result.map_err(ExternError::from), SideEffect::Pure)
            }
            // Make a value through the other definition
            "make_rogue_state" => {
                let typdef = ctx.external().typdef_state_rogue;
                let result = read_tagged(ctx.arena(), values)
                    .and_then(|tagged| typdef.make(ctx.arena_mut(), tagged, Span::default()));
                Eval::new(result.map_err(ExternError::from), SideEffect::Pure)
            }
            // Read the note; reported pure so callers may be cached
            "note_state" => {
                ctx.external_mut().calls_note += 1;
                let typdef = ctx.external().typdef_state;
                let result = value::get::one(values)
                    .and_then(|value| typdef.get(ctx.arena(), value))
                    .map(|tagged| tagged.note.clone())
                    .and_then(|note| value::make::text(ctx.arena_mut(), note, Span::default()));
                Eval::new(result.map_err(ExternError::from), SideEffect::Pure)
            }
            // Read the call count, which changes between calls
            "calls_note" => {
                let calls = ctx.external().calls_note;
                let result = value::make::nat(ctx.arena_mut(), calls.into(), Span::default());
                Eval::new(result.map_err(ExternError::from), SideEffect::Impure)
            }
            _ => Eval::new(Err(ExternError::diagnostic_unconfigured()), SideEffect::Impure),
        }
    }

    fn clear(&mut self) {
        self.calls_note = 0;
    }
}

// = Execution

/// Runs every registered case and compares its transcript.
pub fn run(path: &Path) -> Result<()> {
    let suite: Suite = crate::suite::load(path)?;
    if suite.cases.is_empty() {
        return Err(failure("extern", "no cases registered"));
    }
    let path_host = crate::suite::expected_path(&suite.host);
    for case in &suite.cases {
        let text = run_case(&path_host, case)?;
        expect_file![crate::suite::expected_path(&case.expected)].assert_eq(&text);
    }
    eprintln!("extern: {} cases passed", suite.cases.len());
    Ok(())
}

/// Renders one block per distinct stage outcome, in stage order.
fn run_case(path_host: &Path, case: &Case) -> Result<String> {
    let paths = [path_host.to_owned(), crate::suite::expected_path(&case.input)];
    let config = Config::new(true, true, case.guard);
    let mut blocks: Vec<(Vec<&str>, String)> = Vec::new();
    for stage in ["al", "sl", "pl"] {
        let mut arena = Arena::new();
        let host = Host::new(&mut arena).map_err(|error| failure(&case.name, error))?;
        // Specification and runner failures are setup errors, not outcomes
        let build = |result: std::result::Result<Runner<P4Interface, Host>, runner::BuildError>| {
            result.map_err(|error| failure(&case.name, error))
        };
        let runner = match stage {
            "al" => {
                let spec = p4spectec::algo(&paths).map_err(|error| failure(&case.name, error))?;
                build(runner::build_al(&mut arena, spec, config, host))?
            }
            "sl" => {
                let spec = p4spectec::structure(&paths, true)
                    .map_err(|error| failure(&case.name, error))?;
                build(runner::build_sl(&mut arena, spec, config, host))?
            }
            _ => {
                let spec =
                    p4spectec::prosify(&paths).map_err(|error| failure(&case.name, error))?;
                build(runner::build_pl(&mut arena, spec, config, host))?
            }
        };
        let outcome = eval(&case.name, &mut arena, runner)?;
        // Stages with the same outcome share a block
        match blocks.iter_mut().find(|(_, text)| *text == outcome) {
            Some((stages, _)) => stages.push(stage),
            None => blocks.push((vec![stage], outcome)),
        }
    }
    Ok(blocks
        .into_iter()
        .map(|(stages, text)| format!("== {}\n{text}", stages.join(", ")))
        .collect())
}

/// Calls R with nat 1, then renders its outputs or failure codes.
fn eval(name: &str, arena: &mut Arena, mut runner: Runner<P4Interface, Host>) -> Result<String> {
    let value = value::make::nat(arena, 1u64.into(), Span::default())
        .map_err(|error| failure(name, error))?;
    let result = runner.eval_rel(arena, "R", &[value]).result;
    let text = match result {
        Ok(values) => values
            .iter()
            .map(|value| format!("{}\n", value.view(arena).to_string()))
            .collect::<String>(),
        Err(error) => {
            let kind = match &error {
                InterpreterError::Fatal(_) => "fatal",
                InterpreterError::Mismatch(_) => "mismatch",
            };
            format!("{kind} {}\n", codes(&error.into_report()).join(" "))
        }
    };
    Ok(text)
}

/// Collects diagnostic codes beneath every report frame, outermost first.
fn codes(report: &Report) -> Vec<String> {
    let mut codes_found = Vec::new();
    let mut pending = vec![report];
    while let Some(report) = pending.pop() {
        if let ReportKind::Cause(diagnostic) = &report.kind
            && let Some(code) = &diagnostic.code
        {
            codes_found.push(code.clone());
        }
        pending.extend(report.children.iter().rev());
    }
    codes_found
}
