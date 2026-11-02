//! K hooks answered by the K framework's own implementation
//!
//! spec-k delegates the hooks of K's modules of domain values with one extern
//! relation, `Call_hook: |- symid sort* sort term* : res<term>`
//! (`spec-k/2-relation.watsup`), which the K definition above spec-k answers.
//! `Offload::call` writes the application as KORE (`decode`), the K library of the
//! definition (`runtime.so` in its kompiled directory, through `ffi`) evaluates it,
//! and the answer is read back as a term (`binary`, `encode`): `OK` the term, or
//! `ERROR` if the runtime fails.

use super::{
    Loaded,
    ast::{Pattern, Sort},
    binary, decode, diagnostic, encode,
    ffi::Runtime,
    unimplemented,
};
use crate::{
    lang::data::{
        arena::Arena,
        value::flat::{self as value, Value},
    },
    runner::{ExternError, InterfaceError},
};
use std::{cell::RefCell, path::Path, rc::Rc};

/// The file name of the K library in a kompiled directory.
pub(super) const LIBRARY: &str = "runtime.so";

/// Opens the K library in `dir_kompiled`, the directory of `definition.kore`,
/// and gives its runtime.
pub(super) fn open(dir_kompiled: &Path) -> Result<&'static Runtime, InterfaceError> {
    let path = dir_kompiled.join(LIBRARY);
    let error = |message| InterfaceError(diagnostic("kore/library", message));
    if !path.is_file() {
        return Err(error(format!(
            "no K library at {} (llvm-kompile definition.kore dt c -- -o {LIBRARY})",
            path.display()
        )));
    }
    Runtime::open(&path).map_err(error)
}

/// The definition being run, with its K library, shared by every offloaded hook.
///
/// `KoreInterface::parse_program` sets it before `Krun` calls a hook;
/// the top of the tower, `KLibrary`, holds an `Rc` of it.
pub(super) struct Offload {
    loaded: RefCell<Option<Rc<Loaded>>>,
}

impl Offload {
    /// Makes an offload with no definition yet.
    pub(super) fn new() -> Self {
        Self { loaded: RefCell::new(None) }
    }

    /// Keeps the definition read, with its library, for the hooks.
    pub(super) fn set(&self, loaded: Rc<Loaded>) {
        *self.loaded.borrow_mut() = Some(loaded);
    }

    /// The definition set, if any.
    pub(super) fn loaded(&self) -> Option<Rc<Loaded>> {
        self.loaded.borrow().clone()
    }

    /// Evaluates extern relation `name` of spec-k on `values` with the loaded library.
    ///
    /// `Call_hook` is the only one, and the hooked symbol, its first input,
    /// chooses what the library computes.
    pub(super) fn call(
        &self,
        arena: &mut Arena,
        name: &str,
        values: &[Value],
    ) -> Result<Vec<Value>, ExternError> {
        let result = match name {
            "Call_hook" => self.call_hook(arena, values),
            _ => return Err(ExternError(unimplemented(&format!("extern relation {name}")))),
        };
        result
            .map(|value| vec![value])
            .map_err(|message| ExternError(diagnostic("kore/runtime", message)))
    }

    /// `Call_hook: |- symid sort* sort term* : res<term>`: the hooked symbol
    /// applied to the terms, by the K runtime; `ERROR` if the runtime fails.
    fn call_hook(&self, arena: &mut Arena, values: &[Value]) -> Result<Value, String> {
        let loaded = self.loaded().ok_or("Call_hook needs a loaded definition")?;
        let (pattern, sort) = hook_args(arena, &loaded, values)?;
        // The runtime's answer as a term; a failure there is ERROR
        let answer = match loaded.runtime.simplify(&pattern.to_string(), &sort) {
            Ok(result) => Some(unwrap_result(binary::decode(&result)?)),
            Err(_) => None,
        };
        hook_result(arena, &loaded, answer.as_ref())
    }
}

/// The inputs of `Call_hook`, the hooked symbol, its sort parameters, its
/// result sort, and the terms it applies to, as the application to evaluate
/// and its sort.
fn hook_args(arena: &Arena, loaded: &Loaded, values: &[Value]) -> Result<(Pattern, Sort), String> {
    let [symbol, sorts, sort, terms] = values else {
        return Err(format!("Call_hook takes 4 inputs, not {}", values.len()));
    };
    let list = |value| value::get::list(arena, value).map_err(|e| e.to_string());
    let symbol = value::get::text(arena, symbol)
        .map_err(|e| e.to_string())?
        .to_owned();
    let sorts = list(sorts)?
        .iter()
        .map(|sort| decode::sort(arena, &loaded.mixops, sort))
        .collect::<Result<_, _>>()?;
    let sort = decode::sort(arena, &loaded.mixops, sort)?;
    let terms = list(terms)?
        .iter()
        .map(|term| decode::term(arena, &loaded.info, &loaded.mixops, term))
        .collect::<Result<_, _>>()?;
    Ok((Pattern::App(symbol, sorts, terms), sort))
}

/// The output of `Call_hook`, of `res<term>`: `OK` the runtime's answer as a
/// term, or `ERROR` if it failed.
fn hook_result(
    arena: &mut Arena,
    loaded: &Loaded,
    answer: Option<&Pattern>,
) -> Result<Value, String> {
    let result = match answer {
        Some(answer) => encode::term(arena, &loaded.info, &loaded.mixops, answer)
            .and_then(|term| encode::ok(arena, &loaded.mixops, term)),
        None => encode::error(arena, &loaded.mixops),
    };
    result.map_err(|error| error.to_string())
}

/// A Float literal in the runtime's output format: the runtime evaluates it
/// as a domain value, e.g. `0.5` as
/// `0.500000000000000000000000000000000000000e0`.
pub(super) fn float_literal(loaded: &Loaded, text: &str) -> Result<String, String> {
    let sort = Sort::Sort(loaded.info.sort_of_hook("FLOAT.Float")?.to_owned(), vec![]);
    let literal =
        Pattern::App("\\dv".to_owned(), vec![sort.clone()], vec![Pattern::Str(text.into())]);
    let bad = || format!("bad Float literal {text:?}");
    let result = loaded
        .runtime
        .simplify(&literal.to_string(), &sort)
        .map_err(|_| bad())?;
    match unwrap_result(binary::decode(&result)?) {
        Pattern::App(dv, _, args) if dv == "\\dv" => match args.as_slice() {
            [Pattern::Str(normal)] => Ok(normal.clone()),
            _ => Err(bad()),
        },
        _ => Err(bad()),
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
