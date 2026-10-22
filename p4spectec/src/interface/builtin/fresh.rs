//! Fresh-name builtins
//!
//! `make` creates a fresh-type-id builtin instance with its own counter.
//! `fresh_type_id` names fresh types `FRESH__0`, `FRESH__1`, ...;
//! recreating its closure restarts only that runner's sequence.
//! `fresh` is stateless: `fresh("x", ["x", "x'"])` yields `x''`.

use crate::lang::{
    common::source::Span,
    data::{
        arena::Arena,
        value::flat::{self as value, Value},
    },
    il::ast::Typ,
};

use super::{BuiltinError, call::BuiltinImpl, extract};

/// `dec $fresh(id, id*) : id`,
/// the base identifier with the fewest primes that avoids every used identifier.
pub fn fresh(arena: &mut Arena, targs: &[Typ], values: &[Value]) -> Result<Value, BuiltinError> {
    extract::zero(targs)?;
    let (value_base, value_ids) = extract::two(values)?;
    let id_base = value::get::text(arena, value_base)?.to_owned();
    let ids_used = value::get::list(arena, value_ids)?
        .iter()
        .map(|value_id| value::get::text(arena, value_id).map(str::to_owned))
        .collect::<Result<std::collections::HashSet<_>, _>>()?;
    // Prime the base until it differs from every used identifier
    let mut id_fresh = id_base;
    while ids_used.contains(&id_fresh) {
        id_fresh.push('\'');
    }
    Ok(value::make::text(arena, id_fresh, Span::default())?)
}

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
