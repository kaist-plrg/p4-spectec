//! The builtins spec-k declares beyond the standard ones
//!
//! spec-k delegates the K hooks of domain values with one builtin,
//! `builtin dec $call_hook(symid, sort*, sort, term*) : res<term>`
//! (`spec-k/6.2-eval.watsup`). Offloaded hooks, named in `OFFLOADED`,
//! forward to `Offload::call`, which runs the K framework's own implementation.
//! The other entries are spec-k's text builtins for reading hook names (`texts`),
//! which are not K hooks. `$fresh_nat`, the one with a state,
//! is answered by `KoreInterface::call_builtin` itself.
//! An entry overrides a standard builtin of the same name;
//! a builtin without one fails with `runtime/builtin-implementation-missing`.

use super::{offload::Offload, texts};
use crate::interface::builtin::call::BuiltinImpl;
use std::rc::Rc;

/// The hooks the K library answers, by the name spec-k declares them with.
const OFFLOADED: [&str; 0] = [];

/// Returns the builtins of spec-k by name: the offloaded hooks, answered
/// through `offload`, and the text builtins.
pub(super) fn hooks(offload: &Rc<Offload>) -> [(&'static str, BuiltinImpl); 2] {
    let [] = OFFLOADED.map(|name| (name, offloaded(Rc::clone(offload), name)));
    let [has_prefix, string_to_int] = texts::entries();
    [has_prefix, string_to_int]
}

/// Forwards hook `name` to the K library.
fn offloaded(offload: Rc<Offload>, name: &'static str) -> BuiltinImpl {
    Box::new(move |arena, targs, values| offload.call(arena, name, targs, values))
}
