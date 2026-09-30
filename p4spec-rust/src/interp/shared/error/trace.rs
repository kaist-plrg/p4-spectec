//! Invocation and evaluation frames
//!
//! Frame messages are formatted only after evaluation fails.

use crate::lang::{
    il::ast::{Id, Typ},
    traits::print::Print,
};

/// Describes a function invocation, including supplied type arguments.
pub fn message_func_invocation(id: &Id, targs: &[Typ]) -> String {
    // Omit the angle brackets for monomorphic calls
    if targs.is_empty() {
        return format!("while invoking ${}", id.node);
    }
    // Format instantiated arguments only on the failure path
    format!(
        "while invoking ${}<{}>",
        id.node,
        targs
            .iter()
            .map(Print::to_string)
            .collect::<Vec<_>>()
            .join(", ")
    )
}

/// Describes a relation invocation.
pub fn message_rel_invocation(id: &Id) -> String {
    format!("while invoking {}", id.node)
}
