//! Stateful fresh-type-id builtin
//!
//! `make` creates a builtin instance with its own counter.
//! `fresh_type_id` names fresh types `FRESH__0`, `FRESH__1`, ...;
//! recreating its closure restarts only that runner's sequence.

use crate::lang::{
    common::source::Span,
    data::{
        arena::Arena,
        value::flat::{self as value, Value},
    },
    il::ast::Typ,
};

use super::{BuiltinError, call::BuiltinImpl, extract};

/// Creates a fresh-type-id builtin with its own zeroed counter.
pub(super) fn make() -> BuiltinImpl {
    let mut counter = 0_u64;
    Box::new(move |arena, targs, values| fresh_type_id(arena, &mut counter, targs, values))
}

/// Returns a fresh type id and advances the supplied counter.
fn fresh_type_id(
    arena: &mut Arena,
    counter: &mut u64,
    targs: &[Typ],
    values: &[Value],
) -> Result<Value, BuiltinError> {
    // Validate before advancing the local sequence
    extract::zero(targs)?;
    extract::zero(values)?;
    // Name the type and advance the sequence
    let type_id = format!("FRESH__{counter}");
    *counter = (*counter).wrapping_add(1);
    // Store the type id in the caller's arena
    let value = value::make::text(arena, type_id, Span::default())?;
    Ok(value)
}
