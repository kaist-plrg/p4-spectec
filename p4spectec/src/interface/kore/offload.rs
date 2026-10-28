//! K hooks answered by the K framework's own implementation
//!
//! spec-k delegates the hooks of K's modules of domain values (INT, BOOL,
//! STRING, BYTES, FLOAT, JSON, KRYPTO) to the host with one builtin,
//! `$call_hook(symid, sort*, sort, term*) : res<term>` (`spec-k/6.2-eval.watsup`),
//! and the host hands them to the K LLVM backend's runtime:
//! the library `llvm-kompile` made of the K definition being run,
//! `runtime.so` in the kompiled directory next to its `definition.kore`.
//! `Offload::load` opens that library before `parse_program` reads the definition,
//! since reading a Float literal asks the runtime for its normal form (`float_literal`).
//! `Offload::call` is a slot still.
//! Calls into the library go through `ffi`.

use super::{
    Loaded,
    ast::{Pattern, Sort},
    binary,
    ffi::Runtime,
    info::Info,
    unimplemented,
};
use crate::{
    interface::builtin::error::BuiltinError,
    lang::{
        data::{arena::Arena, value::flat::Value},
        il::ast::Typ,
    },
    runner::InterfaceError,
};
use std::{cell::RefCell, path::Path, rc::Rc};

/// The file name of the K library in a kompiled directory.
pub(super) const LIBRARY: &str = "runtime.so";

/// The K library of the definition being run, shared by every offloaded hook.
///
/// It is loaded per definition, so `KoreInterface::parse_program` loads it
/// before `Krun` calls a hook; the hooks registered in `hooks` hold an `Rc` of it.
pub(super) struct Offload {
    /// The runtime of the library, once loaded.
    runtime: RefCell<Option<&'static Runtime>>,
    /// The definition read, whose sorts and notation the hooks' terms use.
    loaded: RefCell<Option<Rc<Loaded>>>,
}

impl Offload {
    /// Makes an offload with no library loaded.
    pub(super) fn new() -> Self {
        Self { runtime: RefCell::new(None), loaded: RefCell::new(None) }
    }

    /// Opens the K library in `dir_kompiled`, the directory of `definition.kore`.
    pub(super) fn load(&self, dir_kompiled: &Path) -> Result<&'static Runtime, InterfaceError> {
        let path = dir_kompiled.join(LIBRARY);
        if !path.is_file() {
            return Err(InterfaceError(unimplemented(&format!(
                "a K library at {} (llvm-kompile definition.kore dt c -- -o {LIBRARY})",
                path.display()
            ))));
        }
        let runtime = Runtime::open(&path)
            .map_err(|message| InterfaceError(super::diagnostic("kore/library", message)))?;
        *self.runtime.borrow_mut() = Some(runtime);
        Ok(runtime)
    }

    /// Keeps the definition read, for the terms of the hooks.
    pub(super) fn attach(&self, loaded: Rc<Loaded>) {
        *self.loaded.borrow_mut() = Some(loaded);
    }

    /// Calls K hook `name` of the loaded library on `values`.
    ///
    /// `name` is the hook as spec-k declares it (`$name` without `$`);
    /// which library symbol answers it is this function's choice.
    pub(super) fn call(
        &self,
        _arena: &mut Arena,
        name: &str,
        _targs: &[Typ],
        _values: &[Value],
    ) -> Result<Value, BuiltinError> {
        Err(BuiltinError(unimplemented(&format!("offloading K hook {name}"))))
    }
}

/// The runtime, which `what` needs.
fn runtime(runtime: Option<&'static Runtime>, what: &str) -> Result<&'static Runtime, String> {
    runtime.ok_or_else(|| format!("{what} needs the K library of the definition"))
}

/// A Float literal in the runtime's output format: the runtime evaluates it
/// as a domain value, e.g. `0.5` as
/// `0.500000000000000000000000000000000000000e0`.
pub(super) fn float_literal(
    runtime: Option<&'static Runtime>,
    info: &Info,
    text: &str,
) -> Result<String, String> {
    let runtime = self::runtime(runtime, "a Float literal")?;
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
