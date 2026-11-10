//! Text builtins of spec-k, which are not K hooks
//!
//! Reading hook names (`spec-k/3-load.watsup`) looks at the start of a text,
//! and a hook that counts (`spec-k/5.3-hook-collection.watsup`) writes an Int
//! as K does. A KORE string has one character per byte (`parse`).
//! For example, `$has_prefix("INT.add", "INT.")` is true, and `$int_text(2)`
//! is `2`, where spec-meta's `$int_to_text` gives `+2`. `$kore_string`
//! escapes a string as the LLVM backend's printer does, and `$drop_prefix`
//! gives the width of an MInt sort, for `#unparseKORE`
//! (`spec-k/5.2-hook.watsup`).

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

/// `dec $kore_string(text) : text`, the text with its bytes escaped as in a
/// KORE string, as the LLVM backend's printer writes one
/// (`runtime/util/ConfigurationPrinter.cpp`): `\\`, `\"`, `\n`, `\t`, `\r`,
/// `\f`, a printable ASCII byte as it is, and any other byte as `\xNN`.
fn kore_string(arena: &mut Arena, targs: &[Typ], values: &[Value]) -> Result<Value> {
    extract::zero(targs)?;
    let mut escaped = String::new();
    for c in text(arena, extract::one(values)?)?.chars() {
        match c {
            '\\' => escaped.push_str("\\\\"),
            '"' => escaped.push_str("\\\""),
            '\n' => escaped.push_str("\\n"),
            '\t' => escaped.push_str("\\t"),
            '\r' => escaped.push_str("\\r"),
            '\u{c}' => escaped.push_str("\\f"),
            ' '..='~' => escaped.push(c),
            // One character per byte
            _ => escaped.push_str(&format!("\\x{:02x}", u32::from(c) & 0xff)),
        }
    }
    Ok(value::make::text(arena, escaped, Span::default())?)
}

/// `dec $drop_prefix(text, text) : text`, the first text without the second
/// at its start, if it is there: the width `64` of the sort `Sort64`.
fn drop_prefix(arena: &mut Arena, targs: &[Typ], values: &[Value]) -> Result<Value> {
    extract::zero(targs)?;
    let (t, prefix) = extract::two(values)?;
    let t = text(arena, t)?;
    let prefix = text(arena, prefix)?;
    let rest = t.strip_prefix(prefix.as_str()).unwrap_or(t.as_str());
    Ok(value::make::text(arena, rest.to_owned(), Span::default())?)
}

/// The builtins above, by name.
pub(super) fn entries() -> [(&'static str, BuiltinImpl); 4] {
    [
        ("has_prefix", Box::new(has_prefix)),
        ("int_text", Box::new(int_text)),
        ("kore_string", Box::new(kore_string)),
        ("drop_prefix", Box::new(drop_prefix)),
    ]
}
