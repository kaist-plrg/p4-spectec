//! Extern value semantics through a test host
//!
//! Each case extends the host specification, which declares `extern syntax state`,
//! and runs through AL, SL, and PL with nat 1 as the input to R.
//! The host makes `state` values from a key and a note;
//! payloads compare and hash by key alone,
//! so equal values may still carry distinct notes.
//! A second definition of `state`, made by the same host,
//! is never the one the specification's values are read through.
//! The host also makes `holder` values that hold other values,
//! through a definition with a codec.
//! Cases choose which definitions the host registers,
//! `state` and `holder` by default,
//! and whether the runner type-checks values at call boundaries.
//! Roundtrip cases then encode each output arena-relatively into the same arena,
//! and arena-independently, directly and as a tree,
//! into a fresh arena whose host has its own definitions.
//! Each stage renders R's outputs or failure codes;
//! stages with equal transcripts share one block.

use std::{
    cmp::Ordering,
    hash::{Hash, Hasher},
    path::{Path, PathBuf},
    rc::Rc,
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
            encoding::Encoding,
            value::{
                ValueError,
                extern_value::{TypDef, TypDefId, TypDefs},
                flat::{
                    self as value, Value, ValueField, ValueKind,
                    external::{decode_with, encode_with},
                },
                tree as value_tree,
            },
        },
        il::ast::Typ,
        traits::eq::SyntaxEq,
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
    /// Definitions the host registers: `state`, `holder`, `rogue`, or `unused`.
    #[serde(default = "register_default")]
    pub register: Vec<String>,
    /// Encode and decode each output of R.
    #[serde(default)]
    pub roundtrip: bool,
    /// Definitions a fresh roundtrip receiver registers, if not `register`.
    #[serde(default)]
    pub register_receiver: Option<Vec<String>>,
}

fn register_default() -> Vec<String> {
    vec!["state".to_owned(), "holder".to_owned()]
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
    /// The definition of `holder`, whose payloads encode with their values.
    typdef_holder: TypDef<Vec<Value>>,
    /// The definitions this host registers.
    typdefs: Vec<TypDefId>,
    /// Calls of `$note_state` so far.
    calls_note: u64,
}

impl Host {
    /// Defines both `state` definitions, `holder`, and `unused` in `arena`.
    fn new(arena: &mut Arena, register: &[String]) -> std::result::Result<Self, String> {
        let typdef_state = TypDef::new(arena, "state").map_err(|error| error.to_string())?;
        let typdef_state_rogue = TypDef::new(arena, "state").map_err(|error| error.to_string())?;
        let typdef_holder =
            TypDef::with_codec(arena, "holder").map_err(|error| error.to_string())?;
        let typdef_unused =
            TypDef::<u8>::new(arena, "unused").map_err(|error| error.to_string())?;
        // Register the chosen definitions in the order given
        let typdefs = register
            .iter()
            .map(|name| match name.as_str() {
                "state" => Ok(typdef_state.id()),
                "rogue" => Ok(typdef_state_rogue.id()),
                "holder" => Ok(typdef_holder.id()),
                "unused" => Ok(typdef_unused.id()),
                _ => Err(format!("unknown definition {name}")),
            })
            .collect::<std::result::Result<_, _>>()?;
        Ok(Self { typdef_state, typdef_state_rogue, typdef_holder, typdefs, calls_note: 0 })
    }
}

/// The definitions a case's host made, kept after the host moves into its runner.
#[derive(Clone)]
struct HostDefs {
    typdef_holder: TypDef<Vec<Value>>,
    typdefs: Vec<TypDefId>,
}

impl Host {
    /// The definitions roundtrips decode through.
    fn defs(&self) -> HostDefs {
        HostDefs { typdef_holder: self.typdef_holder, typdefs: self.typdefs.clone() }
    }
}

/// Rebuilds `value` with every `state` value made by the rogue definition.
///
/// Struct, case, tuple, option, and list structure is kept as is.
fn poison(
    arena: &mut Arena,
    typdef_state: TypDef<Tagged>,
    typdef_state_rogue: TypDef<Tagged>,
    value: Value,
) -> std::result::Result<Value, ValueError> {
    let poison_inner =
        |arena: &mut Arena, value| poison(arena, typdef_state, typdef_state_rogue, value);
    let typ = Rc::clone(arena.value.typ(value.note));
    let span = *arena.value.span(value.span);
    match arena.value.kind(value.node) {
        // Move the payload to the other definition
        ValueKind::Extern(_) => {
            let tagged = typdef_state.get(arena, &value)?.clone();
            typdef_state_rogue.make(arena, tagged, span)
        }
        // Rebuild each field
        ValueKind::Struct(value_fields) => {
            let value_fields = value_fields.clone();
            let value_fields = value_fields
                .into_iter()
                .map(|ValueField { atom, value }| {
                    Ok(ValueField { atom, value: poison_inner(arena, value)? })
                })
                .collect::<std::result::Result<_, ValueError>>()?;
            value::make::structure(arena, typ, value_fields, span)
        }
        // Rebuild each argument under the same mixop
        ValueKind::Case(value_case) => {
            let mut value_case = value_case.clone();
            for value in value_case.args_mut() {
                *value = poison_inner(arena, *value)?;
            }
            value::make::case_flat(arena, typ, value_case, span)
        }
        // Rebuild each element
        ValueKind::Tuple(values) => {
            let values = values.clone();
            let values = values
                .into_iter()
                .map(|value| poison_inner(arena, value))
                .collect::<std::result::Result<_, _>>()?;
            value::make::tuple(arena, typ, values, span)
        }
        ValueKind::List(values) => {
            let values = values.clone();
            let values = values
                .into_iter()
                .map(|value| poison_inner(arena, value))
                .collect::<std::result::Result<_, _>>()?;
            value::make::list(arena, typ, values, span)
        }
        // Rebuild a present payload
        ValueKind::Opt(value_opt) => {
            let value_opt = *value_opt;
            let value_opt = value_opt
                .map(|value| poison_inner(arena, value))
                .transpose()?;
            value::make::opt(arena, typ, value_opt, span)
        }
        // Other values hold no extern values
        _ => Ok(value),
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
    fn typdefs(&self) -> Vec<TypDefId> {
        self.typdefs.clone()
    }

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
            // Hold the list's values
            "hold" => {
                let typdef = ctx.external().typdef_holder;
                let result = value::get::one(values)
                    .and_then(|value| value::get::list(ctx.arena(), value))
                    .map(<[Value]>::to_vec)
                    .and_then(|values| typdef.make(ctx.arena_mut(), values, Span::default()));
                Eval::new(result.map_err(ExternError::from), SideEffect::Pure)
            }
            // Return the argument unchanged
            "keep" => Eval::new(
                value::get::one(values).copied().map_err(ExternError::from),
                SideEffect::Pure,
            ),
            // Return the argument with its `state` values made by the rogue definition
            "poison" => {
                let (typdef_state, typdef_state_rogue) =
                    (ctx.external().typdef_state, ctx.external().typdef_state_rogue);
                let result = value::get::one(values).copied().and_then(|value| {
                    poison(ctx.arena_mut(), typdef_state, typdef_state_rogue, value)
                });
                Eval::new(result.map_err(ExternError::from), SideEffect::Pure)
            }
            // Pass a rogue value to relation S and return its output
            "reenter" => {
                let typdef = ctx.external().typdef_state_rogue;
                let value = match read_tagged(ctx.arena(), values)
                    .and_then(|tagged| typdef.make(ctx.arena_mut(), tagged, Span::default()))
                {
                    Ok(value) => value,
                    Err(error) => return Eval::new(Err(error.into()), SideEffect::Impure),
                };
                let eval = ctx.call_rel("S", &[value]);
                let result = eval
                    .result
                    .map_err(ExternError::from)
                    .and_then(|values| Ok(*value::get::one(&values)?));
                Eval::new(result, eval.effect)
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
        let host =
            Host::new(&mut arena, &case.register).map_err(|error| failure(&case.name, error))?;
        let defs = host.defs();
        // Specification failures are setup errors; runner build failures are outcomes
        let built = match stage {
            "al" => {
                let spec = p4spectec::algo(&paths).map_err(|error| failure(&case.name, error))?;
                runner::build_al(&mut arena, spec, config, host)
            }
            "sl" => {
                let spec = p4spectec::structure(&paths, true)
                    .map_err(|error| failure(&case.name, error))?;
                runner::build_sl(&mut arena, spec, config, host)
            }
            _ => {
                let spec =
                    p4spectec::prosify(&paths).map_err(|error| failure(&case.name, error))?;
                runner::build_pl(&mut arena, spec, config, host)
            }
        };
        let outcome = match built {
            Ok(runner) => eval(case, &mut arena, runner, &defs)?,
            Err(report) => format!("build {}\n", codes(&report).join(" ")),
        };
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
fn eval(
    case: &Case,
    arena: &mut Arena,
    mut runner: Runner<P4Interface, Host>,
    defs: &HostDefs,
) -> Result<String> {
    let name = case.name.as_str();
    let value = value::make::nat(arena, 1u64.into(), Span::default())
        .map_err(|error| failure(name, error))?;
    let result = runner.eval_rel(arena, "R", &[value]).result;
    let text = match result {
        Ok(values) => {
            let mut text = String::new();
            for value in values {
                text.push_str(&format!("{}\n", value.view(arena).to_string()));
                if case.roundtrip {
                    text.push_str(&roundtrip(case, arena, defs, value)?);
                }
            }
            text
        }
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

// = Encoding

/// Encodes and decodes `value` in each mode, describing what comes back.
///
/// Arena-relative decoding reuses this arena and the case's definitions;
/// arena-independent decoding, directly and through a tree value,
/// uses a fresh arena whose host defines and registers its own.
fn roundtrip(case: &Case, arena: &mut Arena, defs: &HostDefs, value: Value) -> Result<String> {
    let name = case.name.as_str();
    let mut text = String::new();
    let typdefs =
        TypDefs::new(&arena.typdef, &defs.typdefs).map_err(|error| failure(name, error))?;
    let typdef_holder = defs.typdef_holder;

    // Relative: the body's handles decode into the same arena
    let result = encode_with(arena, Encoding::ArenaRelative, arena.value.kind(value.node))
        .map_err(|error| error.to_string())
        .and_then(|json| {
            let kind = decode_with::<ValueKind>(arena, Encoding::ArenaRelative, &typdefs, &json)
                .map_err(|error| error.to_string())?;
            let typ = std::rc::Rc::clone(arena.value.typ(value.note));
            let span = *arena.value.span(value.span);
            Value::new(arena, kind, typ, span).map_err(|error| error.to_string())
        });
    text.push_str(&match result {
        Ok(value_decoded) => {
            let equal = value_decoded.view(arena).syntax_eq(&value.view(arena));
            let kept = match (
                typdef_holder.get(arena, &value),
                typdef_holder.get(arena, &value_decoded),
            ) {
                (Ok(values), Ok(values_decoded)) => {
                    format!(", held handles kept: {}", values == values_decoded)
                }
                _ => String::new(),
            };
            format!("relative: equal: {equal}{kept}\n")
        }
        Err(message) => format!("relative: {message}\n"),
    });

    // Independent: contents decode into a fresh arena through its own definitions
    let json = encode_with(arena, Encoding::ArenaIndependent, &value);
    text.push_str(&format!(
        "independent: {}\n",
        decode_fresh(
            case,
            arena,
            typdef_holder,
            value,
            json.map_err(|error| error.to_string()),
            |arena, typdefs, json| {
                decode_with::<Value>(arena, Encoding::ArenaIndependent, typdefs, json)
                    .map_err(|error| error.to_string())
            }
        )?
    ));

    // Tree: the owned form converts into a fresh arena the same way
    let tree = value.into_tree(arena).map_err(|error| error.to_string());
    let tree = tree.and_then(|tree| serde_json::to_value(tree).map_err(|error| error.to_string()));
    text.push_str(&format!(
        "tree: {}\n",
        decode_fresh(case, arena, typdef_holder, value, tree, |arena, typdefs, json| {
            let tree = serde_json::from_value::<value_tree::Value>(json.clone())
                .map_err(|error| error.to_string())?;
            tree.into_flat(arena, typdefs)
                .map_err(|error| error.to_string())
        })?
    ));
    Ok(text)
}

/// Decodes an encoded `value` into a fresh arena, describing the result.
///
/// Reports whether the printed value and every held value's annotations survive,
/// and whether the receiver's own `holder` definition reads the result.
fn decode_fresh(
    case: &Case,
    arena: &Arena,
    typdef_holder: TypDef<Vec<Value>>,
    value: Value,
    json: std::result::Result<serde_json::Value, String>,
    decode: impl FnOnce(&mut Arena, &TypDefs, &serde_json::Value) -> std::result::Result<Value, String>,
) -> Result<String> {
    let name = case.name.as_str();
    let json = match json {
        Ok(json) => json,
        Err(message) => return Ok(message),
    };
    // A fresh host defines new definitions in a fresh arena
    let mut arena_fresh = Arena::new();
    let register = case.register_receiver.as_ref().unwrap_or(&case.register);
    let host_fresh = Host::new(&mut arena_fresh, register).map_err(|error| failure(name, error))?;
    let typdefs_fresh = TypDefs::new(&arena_fresh.typdef, &host_fresh.typdefs)
        .map_err(|error| failure(name, error))?;
    let value_fresh = match decode(&mut arena_fresh, &typdefs_fresh, &json) {
        Ok(value_fresh) => value_fresh,
        Err(message) => return Ok(message),
    };
    let equal = value.view(arena).to_string() == value_fresh.view(&arena_fresh).to_string();
    // Held values keep their contents, types, and spans
    let kept = match (
        host_fresh.typdef_holder.get(&arena_fresh, &value_fresh),
        typdef_holder.get(arena, &value),
    ) {
        (Ok(values_fresh), Ok(values)) => {
            let kept = values.len() == values_fresh.len()
                && values.iter().zip(values_fresh).all(|(value, value_fresh)| {
                    value.view(arena).to_string() == value_fresh.view(&arena_fresh).to_string()
                        && arena.value.typ(value.note) == arena_fresh.value.typ(value_fresh.note)
                        && arena.value.span(value.span) == arena_fresh.value.span(value_fresh.span)
                });
            format!(", read through the receiver's holder, held annotations kept: {kept}")
        }
        _ => String::new(),
    };
    Ok(format!("printed equal: {equal}{kept}"))
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
