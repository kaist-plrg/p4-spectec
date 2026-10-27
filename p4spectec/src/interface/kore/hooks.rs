//! The K hooks that spec-k delegates to the host
//!
//! spec-k declares each delegated hook as `builtin dec $name(…) : …`
//! (`spec-k/3.1-hook-builtin.watsup`); `hooks` registers an implementation
//! for each name, which overrides a standard builtin of the same name.
//! #395 implements them in `interface/kore/builtins.ml` and `floats.ml`,
//! following the K LLVM backend's runtime.
//! A hook without an entry here fails with `runtime/builtin-implementation-missing`.

use crate::interface::builtin::call::BuiltinImpl;

/// Returns the K hooks by the name spec-k declares them with.
pub(super) fn hooks() -> [(&'static str, BuiltinImpl); 0] {
    []
}
