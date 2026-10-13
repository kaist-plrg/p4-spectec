//! Stateful fresh-type-id builtin
//!
//! Each builtin instance names fresh types `FRESH__0`, `FRESH__1`, ...;
//! recreating its closure restarts only that runner's sequence.

use crate::lang::{common::source::Span, data::value::flat as value};

use super::{call::BuiltinImpl, extract};

/// Creates a fresh-type-id builtin with its own zeroed counter.
pub(super) fn init() -> BuiltinImpl {
    let mut counter = 0_u64;
    Box::new(move |arena, targs, values| {
        // Validate before advancing the local sequence
        extract::zero(targs)?;
        extract::zero(values)?;
        let type_id = format!("FRESH__{counter}");
        counter = counter.wrapping_add(1);
        let value = value::make::text(arena, type_id, Span::default())?;
        Ok(value)
    })
}
