//! Stateful fresh-type-id builtin
//!
//! A process-wide counter names fresh types `FRESH__0`, `FRESH__1`, ...;
//! `init` restarts it for each program so runs are reproducible.

use std::sync::atomic::{AtomicU64, Ordering};

use crate::lang::{
    common::source::Span,
    data::{
        arena::Arena,
        value::flat::{self as value, Value},
    },
};

use crate::lang::il::ast::Typ;

use super::{BuiltinError, extract};

/// Next fresh index.
static COUNTER: AtomicU64 = AtomicU64::new(0);

/// Restarts the counter.
pub fn init() {
    COUNTER.store(0, Ordering::Relaxed);
}

/// `dec $fresh_typeId() : typeId`, the next `FRESH__n` name.
pub fn fresh_type_id(
    arena: &mut Arena,
    targs: &[Typ],
    values: &[Value],
) -> Result<Value, BuiltinError> {
    extract::zero(targs)?;
    extract::zero(values)?;
    let counter = COUNTER.fetch_add(1, Ordering::Relaxed);
    let type_id = format!("FRESH__{counter}");
    let value = value::make::text(arena, type_id, Span::default())?;
    Ok(value)
}
