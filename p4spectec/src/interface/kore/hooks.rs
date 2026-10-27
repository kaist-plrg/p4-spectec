//! The K hooks that spec-k delegates to the host
//!
//! spec-k declares each delegated hook as `builtin dec $name(…) : …`
//! (`spec-k/3.1-hook-builtin.watsup`).
//! Most are offloaded: `OFFLOADED` names them, and each forwards to `Offload::call`,
//! which runs the K framework's own implementation.
//! A hook the K library cannot answer gets a Rust entry of its own instead,
//! as #395 implements them in `interface/kore/builtins.ml`.
//! An entry overrides a standard builtin of the same name;
//! a hook without one fails with `runtime/builtin-implementation-missing`.

use super::offload::Offload;
use crate::interface::builtin::call::BuiltinImpl;
use std::rc::Rc;

/// The hooks the K library answers, by the name spec-k declares them with.
const OFFLOADED: [&str; 0] = [];

/// Returns the K hooks by name, each answered through `offload`.
pub(super) fn hooks(offload: &Rc<Offload>) -> [(&'static str, BuiltinImpl); 0] {
    OFFLOADED.map(|name| (name, offloaded(Rc::clone(offload), name)))
}

/// Forwards hook `name` to the K library.
fn offloaded(offload: Rc<Offload>, name: &'static str) -> BuiltinImpl {
    Box::new(move |arena, targs, values| offload.call(arena, name, targs, values))
}
