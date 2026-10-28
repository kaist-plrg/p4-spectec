//! The builtins spec-k declares beyond the standard ones
//!
//! `$call_hook`, through which spec-k delegates the K hooks of domain values,
//! is offloaded to the K library (`Offload::call`); the text builtins of `texts`
//! are answered here. `$fresh_nat` is answered by `KoreInterface::call_builtin`.

use super::{offload::Offload, texts};
use crate::interface::builtin::call::BuiltinImpl;
use std::rc::Rc;

/// The hooks the K library answers, by the name spec-k declares them with.
const OFFLOADED: [&str; 1] = ["call_hook"];

/// Returns the builtins of spec-k by name: the offloaded hooks, answered
/// through `offload`, and the text builtins.
pub(super) fn hooks(offload: &Rc<Offload>) -> [(&'static str, BuiltinImpl); 3] {
    let [call_hook] = OFFLOADED.map(|name| (name, offloaded(Rc::clone(offload), name)));
    let [has_prefix, string_to_int] = texts::entries();
    [call_hook, has_prefix, string_to_int]
}

/// Forwards hook `name` to the K library.
fn offloaded(offload: Rc<Offload>, name: &'static str) -> BuiltinImpl {
    Box::new(move |arena, targs, values| offload.call(arena, name, targs, values))
}
