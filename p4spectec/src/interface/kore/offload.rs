//! K hooks answered by the K framework's own implementation
//!
//! spec-k delegates the hooks of K's modules of domain values with one builtin,
//! `$call_hook(symid, sort*, sort, term*) : res<term>` (`spec-k/6.2-eval.watsup`).
//! `Offload::call` writes the application as KORE (`decode`), the K library of the
//! definition (`runtime.so` in its kompiled directory, through `ffi`) evaluates it,
//! and the answer is read back as a term (`binary`, `encode`): `OK` the term, or
//! `ERROR` if the runtime fails.

use super::{
    Loaded,
    ast::{Pattern, Sort},
    binary, decode, diagnostic, encode,
    ffi::Runtime,
    info::Info,
    unimplemented,
};
use crate::{
    interface::builtin::error::BuiltinError,
    lang::{
        data::{
            arena::Arena,
            value::flat::{self as value, Value},
        },
        il::ast::Typ,
    },
    runner::InterfaceError,
};
use std::{cell::RefCell, path::Path, rc::Rc};

/// The file name of the K library in a kompiled directory.
pub(super) const LIBRARY: &str = "runtime.so";

/// The definition being run, with its K library, shared by every offloaded hook.
///
/// A library is loaded per definition, so `KoreInterface::parse_program` loads
/// it and attaches the definition read before `Krun` calls a hook; the hooks
/// registered in `hooks` hold an `Rc` of the offload.
pub(super) struct Offload {
    /// The definition read, with its runtime, whose sorts and notation the
    /// hooks' terms use.
    loaded: RefCell<Option<Rc<Loaded>>>,
}

impl Offload {
    /// Makes an offload with no definition attached.
    pub(super) fn new() -> Self {
        Self { loaded: RefCell::new(None) }
    }

    /// Opens the K library in `dir_kompiled`, the directory of `definition.kore`.
    pub(super) fn load(dir_kompiled: &Path) -> Result<&'static Runtime, InterfaceError> {
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

    /// Keeps the definition read, with its runtime, for the hooks.
    pub(super) fn attach(&self, loaded: Rc<Loaded>) {
        *self.loaded.borrow_mut() = Some(loaded);
    }

    /// The definition attached, if any.
    pub(super) fn loaded(&self) -> Option<Rc<Loaded>> {
        self.loaded.borrow().clone()
    }

    /// Calls K hook `name` of the loaded library on `values`.
    ///
    /// `name` is the hook as spec-k declares it (`$name` without `$`);
    /// `call_hook` is the only one, and the hooked symbol, its first argument,
    /// chooses what the library computes.
    pub(super) fn call(
        &self,
        arena: &mut Arena,
        name: &str,
        _targs: &[Typ],
        values: &[Value],
    ) -> Result<Value, BuiltinError> {
        let result = match name {
            "call_hook" => self.call_hook(arena, values),
            _ => return Err(BuiltinError(unimplemented(&format!("offloading K hook {name}")))),
        };
        result.map_err(|message| BuiltinError(diagnostic("kore/runtime", message)))
    }

    /// `$call_hook(symid, sort*, sort, term*) : res<term>`: the hooked symbol
    /// applied to the terms, by the K runtime; `ERROR` if the runtime fails.
    fn call_hook(&self, arena: &mut Arena, values: &[Value]) -> Result<Value, String> {
        let loaded = self
            .loaded()
            .ok_or("$call_hook needs a loaded definition")?;
        let (pattern, sort) = hook_args(arena, &loaded, values)?;
        let trace = std::env::var_os("KORE_TRACE").is_some();
        if trace {
            eprintln!("hook> {pattern} : {sort}");
        }
        // The runtime's answer as a term; a failure there is ERROR
        let answer = match loaded.runtime.simplify(&pattern.to_string(), &sort) {
            Ok(result) => {
                let result = unwrap_result(binary::decode(&result)?);
                if trace {
                    eprintln!("hook< {result}");
                }
                Some(result)
            }
            Err(message) => {
                if trace {
                    eprintln!("hook failed: {pattern}: {message}");
                }
                None
            }
        };
        hook_result(arena, &loaded, answer.as_ref())
    }
}

/// The arguments of `$call_hook`, the hooked symbol, its sort parameters, its
/// result sort, and the terms it applies to, as the application to evaluate
/// and its sort.
fn hook_args(arena: &Arena, loaded: &Loaded, values: &[Value]) -> Result<(Pattern, Sort), String> {
    let [symbol, sorts, sort, terms] = values else {
        return Err(format!("$call_hook takes 4 arguments, not {}", values.len()));
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

/// The result of `$call_hook`, of `res<term>`: `OK` the runtime's answer as a
/// term, or `ERROR` if it failed.
fn hook_result(
    arena: &mut Arena,
    loaded: &Loaded,
    answer: Option<&Pattern>,
) -> Result<Value, String> {
    match answer {
        Some(answer) => {
            let term = encode::term(arena, &loaded.info, &loaded.mixops, answer)?;
            encode::ok(arena, &loaded.mixops, term)
        }
        None => encode::error(arena, &loaded.mixops),
    }
}

/// A Float literal in the runtime's output format: the runtime evaluates it
/// as a domain value, e.g. `0.5` as
/// `0.500000000000000000000000000000000000000e0`.
pub(super) fn float_literal(runtime: &Runtime, info: &Info, text: &str) -> Result<String, String> {
    let sort = Sort::Sort(info.sort_of_hook("FLOAT.Float")?.to_owned(), vec![]);
    let literal =
        Pattern::App("\\dv".to_owned(), vec![sort.clone()], vec![Pattern::Str(text.into())]);
    let bad = || format!("bad Float literal {text:?}");
    let result = runtime
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
