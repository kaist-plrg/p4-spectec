//! Text builtins of the K specification, outside hooks
//!
//! Reading domain values and hook names (`spec-k/3-load.watsup`) takes
//! text apart. A KORE string has one character per byte (`parse`).
//! For example, `$has_prefix("INT.add", "INT.")` is true.

use num_bigint::BigInt;

use crate::lang::{
    common::{prim::num, source::Span},
    data::{
        arena::Arena,
        typ,
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

/// The integer written in a base, as K's `String2Int` reads it: an optional
/// `+` or `-`, digits, and whitespace anywhere.
fn parse_base(t: &str, base: u32) -> Option<BigInt> {
    let t = t.strip_prefix('+').unwrap_or(t);
    let t: String = t
        .chars()
        .filter(|c| !matches!(c, ' ' | '\t' | '\n' | '\x0b' | '\x0c' | '\r'))
        .collect();
    let (neg, body) = match t.strip_prefix('-') {
        Some(body) => (true, body),
        None => (false, t.as_str()),
    };
    if body.is_empty() || !body.chars().all(|c| c.is_digit(base)) {
        return None;
    }
    let n = BigInt::parse_bytes(body.as_bytes(), base)?;
    Some(if neg { -n } else { n })
}

/// `dec $string_to_int(text) : int?`, in decimal.
fn string_to_int(arena: &mut Arena, targs: &[Typ], values: &[Value]) -> Result<Value> {
    extract::zero(targs)?;
    let t = text(arena, extract::one(values)?)?;
    let i = parse_base(&t, 10)
        .map(|i| value::make::int(arena, i, Span::default()))
        .transpose()?;
    let typ = typ::make::opt(typ::make::int()).node.into();
    Ok(value::make::opt(arena, typ, i, Span::default())?)
}

/// `dec $string_to_bytes(text) : nat*`, one number per byte.
fn string_to_bytes(arena: &mut Arena, targs: &[Typ], values: &[Value]) -> Result<Value> {
    extract::zero(targs)?;
    let t = text(arena, extract::one(values)?)?;
    let bytes = t
        .chars()
        .map(|c| {
            let n = num::Natural::try_from(BigInt::from(u32::from(c)))?;
            Ok(value::make::nat(arena, n, Span::default())?)
        })
        .collect::<Result<Vec<_>>>()?;
    let typ = typ::make::list(typ::make::nat()).node.into();
    Ok(value::make::list(arena, typ, bytes, Span::default())?)
}

/// The builtins above, by name.
pub fn entries() -> [(&'static str, BuiltinImpl); 3] {
    [
        ("has_prefix", Box::new(has_prefix)),
        ("string_to_int", Box::new(string_to_int)),
        ("string_to_bytes", Box::new(string_to_bytes)),
    ]
}
