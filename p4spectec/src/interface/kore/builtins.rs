//! Text builtins of the K specification, outside hooks
//!
//! Reading domain values (`spec-k/6-load.watsup`) and fresh variables
//! (`spec-k/4.2-eval.watsup`) takes text apart. A KORE string has
//! one character per byte (`parse`), so positions and lengths are in bytes,
//! as in the LLVM backend. For example, `$string_find("12p8", "p", 0)` is 2.

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

type Result<T> = std::result::Result<T, BuiltinError>;

fn text(arena: &Arena, value: &Value) -> Result<String> {
    Ok(value::get::text(arena, value)?.to_owned())
}

fn int(arena: &Arena, value: &Value) -> Result<BigInt> {
    Ok(num::to_int(value::get::num(arena, value)?).clone())
}

/// A position as an index into the text's characters.
fn index(i: &BigInt) -> Result<usize> {
    usize::try_from(i).map_err(|_| BuiltinError::argument_invalid(format!("bad position {i}")))
}

fn make_int(arena: &mut Arena, i: BigInt) -> Result<Value> {
    Ok(value::make::int(arena, i, Span::default())?)
}

/// `dec $string_length(text) : int`, in bytes.
fn string_length(arena: &mut Arena, targs: &[Typ], values: &[Value]) -> Result<Value> {
    extract::zero(targs)?;
    let t = text(arena, extract::one(values)?)?;
    make_int(arena, BigInt::from(t.chars().count()))
}

/// `dec $string_substr(text, int, int) : text`, the bytes in [start, end).
fn string_substr(arena: &mut Arena, targs: &[Typ], values: &[Value]) -> Result<Value> {
    extract::zero(targs)?;
    let (t, start, end) = extract::three(values)?;
    let t = text(arena, t)?;
    let (start, end) = (index(&int(arena, start)?)?, index(&int(arena, end)?)?);
    if start > end || end > t.chars().count() {
        return Err(BuiltinError::argument_invalid(format!("bad range [{start}, {end})")));
    }
    let sub = t.chars().skip(start).take(end - start).collect();
    Ok(value::make::text(arena, sub, Span::default())?)
}

/// `dec $string_find(text, text, int) : int`, the first position from the
/// given one where the second text occurs in the first, or -1.
fn string_find(arena: &mut Arena, targs: &[Typ], values: &[Value]) -> Result<Value> {
    extract::zero(targs)?;
    let (haystack, needle, pos) = extract::three(values)?;
    let haystack: Vec<char> = text(arena, haystack)?.chars().collect();
    let needle: Vec<char> = text(arena, needle)?.chars().collect();
    let pos = int(arena, pos)?;
    let found = usize::try_from(&pos)
        .ok()
        .filter(|&pos| pos < haystack.len())
        .and_then(|pos| {
            (pos..=haystack.len().saturating_sub(needle.len()))
                .find(|&i| haystack[i..].starts_with(&needle))
        });
    let found = found.map_or(BigInt::from(-1), BigInt::from);
    make_int(arena, found)
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
    let i = parse_base(&t, 10).map(|i| make_int(arena, i)).transpose()?;
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
pub fn entries() -> [(&'static str, BuiltinImpl); 5] {
    [
        ("string_length", Box::new(string_length)),
        ("string_substr", Box::new(string_substr)),
        ("string_find", Box::new(string_find)),
        ("string_to_int", Box::new(string_to_int)),
        ("string_to_bytes", Box::new(string_to_bytes)),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn string_to_int_as_k() {
        assert_eq!(parse_base("+12", 10), Some(BigInt::from(12)));
        assert_eq!(parse_base("- 1 2", 10), Some(BigInt::from(-12)));
        assert_eq!(parse_base("1x", 10), None);
        assert_eq!(parse_base("", 10), None);
    }
}
