//! KORE frontend and runner interface for the K specification (`spec-k`)
//!
//! `parse` reads KORE text into `ast`, and `load` turns it into values
//! of the specification as it is.
//! `KoreInterface` loads a kompiled definition (`definition.kore`)
//! and then parses initial terms against its symbols.
//! `krun` runs the specification's `Krun` relation from an initial term.
//!
//! Hooks on domain values are computed by the K runtime of the definition
//! (`llvm`): `$call_hook` writes its arguments, domain values, as KORE
//! (`term`), has the runtime evaluate the hooked symbol, and reads the
//! resulting domain value back (`binary`). Hooks on K terms are specified.
//! `builtins` has the text builtins for reading domain values.

use std::{collections::HashSet, path::Path, path::PathBuf};

use num_bigint::BigInt;

use crate::lang::{
    common::{prim::num::Natural, source::Span},
    data::{
        arena::Arena,
        typ,
        value::flat::{self as value, Value},
    },
};

use crate::lang::il::ast::{Id, Typ};

use crate::diagnostic::{Diagnostic, Report, Severity};

use crate::runner::{
    Eval, Extern, Interface, InterfaceError, Interpreter, RunnerContext, SideEffect,
};

use super::builtin::call::Builtins;

use self::{
    ast::{Pattern, Sort},
    info::Info,
    llvm::Runtime,
    term::Shapes,
};

pub mod ast;
pub mod binary;
pub mod builtins;
pub mod check;
pub mod info;
pub mod krun;
pub mod llvm;
pub mod load;
pub mod normal;
pub mod parse;
pub mod term;

// == Errors

const KORE_READ: &str = "kore/read";
const KORE_PARSE: &str = "kore/parse";
const KORE_LOAD: &str = "kore/load";
const KORE_INPUT: &str = "kore/input";
const KORE_RUNTIME: &str = "kore/runtime";

fn diagnostic(code: &str, message: impl Into<String>) -> InterfaceError {
    let report: Report =
        Diagnostic::new("kore", Severity::Error, Some(code.to_owned()), message, vec![], vec![])
            .into();
    InterfaceError::from(Box::new(report))
}

fn read(path: &Path) -> Result<Vec<u8>, InterfaceError> {
    std::fs::read(path)
        .map_err(|error| diagnostic(KORE_READ, format!("{}: {error}", path.display())))
}

fn parse_error(path: &Path, error: parse::ParseError) -> InterfaceError {
    let message = format!("{}:{}: {}", path.display(), error.line, error.message);
    diagnostic(KORE_PARSE, message)
}

fn load_error(path: &Path, error: load::LoadError) -> InterfaceError {
    let message = match error {
        load::LoadError::SymbolUndeclared(name) => format!("undeclared symbol {name}"),
        load::LoadError::Malformed(what) => format!("malformed {what}"),
        load::LoadError::Value(error) => error.to_string(),
    };
    diagnostic(KORE_LOAD, format!("{}: {message}", path.display()))
}

// == Runner interface

/// Reads KORE for the K specification, and provides its builtins.
pub struct KoreInterface {
    builtins: Builtins,
    /// The symbols and aliases of the loaded definition.
    known: Option<HashSet<String>>,
    /// What converting terms needs of the loaded definition.
    info: Option<(Info, Shapes)>,
    /// The K runtime of the definition.
    runtime: Option<Runtime>,
    /// The next `$fresh_nat`.
    fresh: u64,
}

impl KoreInterface {
    /// Assembles the KORE interface with its builtin table.
    pub(crate) fn new(builtins: Builtins) -> Self {
        Self { builtins, known: None, info: None, runtime: None, fresh: 0 }
    }

    /// Opens the K runtime of the definition, a library made by
    /// `llvm-kompile definition.kore dt c`.
    pub fn open_runtime(&mut self, path: &Path) -> Result<(), InterfaceError> {
        self.runtime =
            Some(Runtime::open(path).map_err(|message| diagnostic(KORE_RUNTIME, message))?);
        Ok(())
    }

    /// Loads a definition as a `kdefinition` value, with its number of axioms.
    ///
    /// Initial terms parsed afterwards may use only its symbols.
    pub fn load_definition(
        &mut self,
        arena: &mut Arena,
        path: &Path,
    ) -> Result<(Value, usize), InterfaceError> {
        let src = read(path)?;
        let definition = parse::parse_definition(&src).map_err(|error| parse_error(path, error))?;
        let known = load::known_symbols(&definition);
        let loaded = load::load_definition(arena, &known, &definition)
            .map_err(|error| load_error(path, error))?;
        let shapes = Shapes::new(arena).map_err(|message| diagnostic(KORE_LOAD, message))?;
        self.known = Some(known);
        self.info = Some((Info::new(&definition), shapes));
        Ok(loaded)
    }

    /// `$call_hook(symid, sort*, sort, term*) : res<term>`: the hooked symbol
    /// applied to the terms, by the K runtime; `FAIL` if the runtime fails.
    fn call_hook(&self, arena: &mut Arena, values: &[Value]) -> Result<Value, String> {
        let (Some((info, shapes)), Some(runtime)) = (&self.info, &self.runtime) else {
            return Err("hooks need the K runtime of the definition (--lib)".to_owned());
        };
        let [symbol, sorts, sort, terms] = values else {
            return Err(format!("$call_hook takes 4 arguments, not {}", values.len()));
        };
        let symbol = value::get::text(arena, symbol)
            .map_err(|e| e.to_string())?
            .to_owned();
        let sorts = value::get::list(arena, sorts)
            .map_err(|e| e.to_string())?
            .iter()
            .map(|sort| term::sort_of_value(arena, shapes, sort))
            .collect::<Result<Vec<_>, _>>()?;
        let sort = term::sort_of_value(arena, shapes, sort)?;
        let terms = value::get::list(arena, terms)
            .map_err(|e| e.to_string())?
            .iter()
            .map(|t| term::to_pattern(arena, info, shapes, t))
            .collect::<Result<Vec<_>, _>>()?;
        let pattern = Pattern::App(symbol, sorts, terms);
        // KORE_TRACE: each call, before the runtime sees it
        if std::env::var_os("KORE_TRACE").is_some() {
            eprintln!("hook> {pattern} : {sort}");
        }
        let mut maker = term::Maker { arena, info, shapes };
        match runtime.simplify(&pattern.to_string(), &sort) {
            Ok(result) => {
                let result = unwrap_result(binary::decode(&result)?);
                let result = maker.of_pattern(&result)?;
                maker.ok(result)
            }
            Err(message) => {
                if std::env::var_os("KORE_TRACE").is_some() {
                    eprintln!("hook failed: {pattern}: {message}");
                }
                maker.fail()
            }
        }
    }

    /// `$float_literal(text) : text?`: a Float literal in the runtime's
    /// output format, e.g. `0.5` as `0.500000000000000000000000000000000000000e0`.
    fn float_literal(&self, arena: &mut Arena, values: &[Value]) -> Result<Value, String> {
        let (Some((info, _)), Some(runtime)) = (&self.info, &self.runtime) else {
            return Err("Float literals need the K runtime of the definition (--lib)".to_owned());
        };
        let [text] = values else {
            return Err(format!("$float_literal takes 1 argument, not {}", values.len()));
        };
        let text = value::get::text(arena, text)
            .map_err(|e| e.to_string())?
            .to_owned();
        let sort = Sort::Sort(info.sort_of_hook("FLOAT.Float")?.to_owned(), vec![]);
        let literal = Pattern::App("\\dv".to_owned(), vec![sort.clone()], vec![Pattern::Str(text)]);
        let normal = match runtime.simplify(&literal.to_string(), &sort) {
            Ok(result) => match unwrap_result(binary::decode(&result)?) {
                Pattern::App(dv, _, args) if dv == "\\dv" => match args.as_slice() {
                    [Pattern::Str(normal)] => Some(normal.clone()),
                    _ => None,
                },
                _ => None,
            },
            Err(_) => None,
        };
        let normal = normal
            .map(|normal| value::make::text(arena, normal, Span::default()))
            .transpose()
            .map_err(|e| e.to_string())?;
        let typ = typ::make::opt(typ::make::text()).node.into();
        value::make::opt(arena, typ, normal, Span::default()).map_err(|e| e.to_string())
    }

    /// A term of the specification as a KORE pattern.
    pub fn kore_of_term(&self, arena: &Arena, value: &Value) -> Result<Pattern, String> {
        let Some((info, shapes)) = &self.info else {
            return Err("no definition is loaded".to_owned());
        };
        term::to_pattern(arena, info, shapes, value)
    }

    /// The normal form of a pattern for comparing runs (`normal`).
    pub fn normal_text(&self, pattern: &Pattern) -> Result<String, String> {
        let Some((info, _)) = &self.info else {
            return Err("no definition is loaded".to_owned());
        };
        Ok(normal::normal_text(info, pattern))
    }

    /// `$fresh_nat() : nat`: 0, 1, 2, ... within a program.
    fn fresh_nat(&mut self, arena: &mut Arena) -> Result<Value, String> {
        let n = Natural::try_from(BigInt::from(self.fresh)).map_err(|e| e.to_string())?;
        self.fresh += 1;
        value::make::nat(arena, n, Span::default()).map_err(|e| e.to_string())
    }
}

impl Interface for KoreInterface {
    /// Parses one initial term as a `kore` value.
    fn parse_program(
        &mut self,
        arena: &mut Arena,
        _includes: &[PathBuf],
        paths: &[PathBuf],
    ) -> Result<Value, InterfaceError> {
        let [path] = paths else {
            return Err(diagnostic(KORE_INPUT, format!("expected one term, got {}", paths.len())));
        };
        let Some(known) = &self.known else {
            return Err(diagnostic(KORE_INPUT, "a term is read after its definition"));
        };
        let src = read(path)?;
        let pattern = parse::parse_pattern(&src).map_err(|error| parse_error(path, error))?;
        load::load_term(arena, known, &pattern).map_err(|error| load_error(path, error))
    }

    fn call_builtin<Interp, Ext>(
        ctx: &mut RunnerContext<'_, Interp, Self, Ext>,
        id: &Id,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value, InterfaceError>
    where
        Ext: Extern,
        Interp: Interpreter<Self, Ext>,
    {
        let (arena, interface) = ctx.arena_and_interface_mut();
        let (result, effect) = match id.node.as_ref() {
            "call_hook" => (interface.call_hook(arena, values), SideEffect::Pure),
            "float_literal" => (interface.float_literal(arena, values), SideEffect::Pure),
            "fresh_nat" => (interface.fresh_nat(arena), SideEffect::Impure),
            _ => {
                return interface
                    .builtins
                    .invoke(arena, id, targs, values)
                    .map_err(InterfaceError::from);
            }
        };
        // A failure of the boundary itself ends the run
        let effect = if result.is_err() { SideEffect::Impure } else { effect };
        let result = result.map_err(|message| diagnostic(KORE_RUNTIME, message));
        Eval::new(result, effect)
    }

    /// Keeps the loaded definition and runtime; resets builtin state.
    fn clear(&mut self) {
        self.builtins.init();
        self.fresh = 0;
    }
}

/// The result of `kore_simplify` without its wrapping: the runtime gives
/// `rawTerm{}(inj{S, SortKItem{}}(v))` for a domain value `v` of sort `S`.
fn unwrap_result(pattern: Pattern) -> Pattern {
    let mut pattern = pattern;
    for wrapper in ["rawTerm", "inj"] {
        pattern = match pattern {
            Pattern::App(name, _, mut args) if name == wrapper && args.len() == 1 => args.remove(0),
            pattern => pattern,
        };
    }
    pattern
}
