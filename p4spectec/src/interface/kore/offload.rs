//! K hooks answered by the K framework's own implementation
//!
//! spec-k delegates most K hooks (`builtin dec $…`) to the host,
//! and the host hands them to the K LLVM backend's runtime:
//! the library `kompile` made of the K definition being run,
//! in the kompiled directory next to its `definition.kore`, as `krun -d` reads it.
//! `Offload::load` opens that library when `parse_program` reads the definition;
//! `Offload::call` converts the arguments to K terms, calls the hook,
//! and converts its result back to a spec-k value.
//! Calls into the library go through `ffi`.

use super::unimplemented;
use crate::{
    interface::builtin::error::BuiltinError,
    lang::{
        data::{arena::Arena, value::flat::Value},
        il::ast::Typ,
    },
    runner::InterfaceError,
};
use std::path::Path;

/// The K library of the definition being run, shared by every offloaded hook.
///
/// It is loaded per definition, so `KoreInterface::parse_program` loads it
/// before `Krun` calls a hook; the hooks registered in `hooks` hold an `Rc` of it.
pub(super) struct Offload;

impl Offload {
    /// Makes an offload with no library loaded.
    pub(super) fn new() -> Self {
        Self
    }

    /// Opens the K library in `dir_kompiled`, the directory of `definition.kore`.
    pub(super) fn load(&self, dir_kompiled: &Path) -> Result<(), InterfaceError> {
        if !dir_kompiled.is_dir() {
            return Err(InterfaceError(unimplemented(&format!(
                "a kompiled directory at {}",
                dir_kompiled.display()
            ))));
        }
        Err(InterfaceError(unimplemented("Offload::load")))
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
