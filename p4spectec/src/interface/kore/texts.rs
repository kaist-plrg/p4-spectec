//! Text builtins of spec-k, which are not K hooks
//!
//! Reading hook names (`spec-k/3-load.watsup`) looks at the start of a text,
//! and a hook that counts (`spec-k/5.3-hook-collection.watsup`) writes an Int
//! as K does. A KORE string has one character per byte (`parse`).
//! For example, `$has_prefix("INT.add", "INT.")` is true, and `$int_text(2)`
//! is `2`, where spec-meta's `$int_to_text` gives `+2`.

use crate::lang::{
    common::{prim::num, source::Span},
    data::{
        arena::Arena,
        value::flat::{self as value, Value},
    },
    il::ast::Typ,
};

use crate::interface::builtin::{BuiltinError, call::BuiltinImpl, extract};

/// A builtin result.
type Result<T> = std::result::Result<T, BuiltinError>;

/// The text of a value.
fn text(arena: &Arena, value: &Value) -> Result<String> {
    Ok(value::get::text(arena, value)?.to_owned())
}

/// `dec $has_prefix(text, text) : bool`, whether the first text begins
/// with the second.
fn has_prefix(arena: &mut Arena, targs: &[Typ], values: &[Value]) -> Result<Value> {
    extract::zero(targs)?;
    let (t, prefix) = extract::two(values)?;
    let found = text(arena, t)?.starts_with(&text(arena, prefix)?);
    Ok(value::make::bool(arena, found, Span::default())?)
}

/// `dec $int_text(int) : text`, the number in decimal, with `-` for a
/// negative one, as the K runtime writes an Int.
fn int_text(arena: &mut Arena, targs: &[Typ], values: &[Value]) -> Result<Value> {
    extract::zero(targs)?;
    let number = value::get::num(arena, extract::one(values)?)?;
    let text = num::to_int(number).to_string();
    Ok(value::make::text(arena, text, Span::default())?)
}

/// The builtins above, by name.
pub(super) fn entries() -> [(&'static str, BuiltinImpl); 2] {
    [("has_prefix", Box::new(has_prefix)), ("int_text", Box::new(int_text))]
}
