//! Text builtins in specification order
//!
//! Arguments are decoded from runtime values before the textual operation,
//! then the encoded result is returned.
//! For example, `strip_prefix("prebody", "pre")` yields `"body"`.

use num_bigint::BigInt;

use crate::lang::{
    common::source::Span,
    data::{
        arena::Arena,
        typ,
        value::flat::{self as value, Value},
    },
    traits::print::Print,
};

use crate::lang::il::ast::Typ;

use super::{BuiltinError, extract};

// == Conversion between runtime values and text

/// The text in a text value.
fn text_of_value<'a>(arena: &'a Arena, value: &Value) -> Result<&'a str, BuiltinError> {
    value::get::text(arena, value).map_err(BuiltinError::from)
}

/// A number value printed as text.
fn numeric_text(arena: &Arena, value: &Value) -> Result<String, BuiltinError> {
    let num = value::get::num(arena, value).map_err(BuiltinError::from)?;
    Ok(Print::to_string(num))
}

// == Built-in implementations

/// `dec $text_to_int(text) : int`,
/// an optionally signed integer in decimal, `0x`, `0o`, or `0b`.
pub fn text_to_int(
    arena: &mut Arena,
    targs: &[Typ],
    values: &[Value],
) -> Result<Value, BuiltinError> {
    extract::zero(targs)?;
    let value_text = extract::one(values)?;
    let text = text_of_value(arena, value_text)?;
    // An optional sign, then an optional radix prefix
    let (negative, unsigned) = match text.as_bytes().first() {
        Some(b'-') => (true, &text[1..]),
        Some(b'+') => (false, &text[1..]),
        _ => (false, text),
    };
    let (radix, digits) = if let Some(digits) = unsigned.strip_prefix("0x") {
        (16, digits)
    } else if let Some(digits) = unsigned.strip_prefix("0X") {
        (16, digits)
    } else if let Some(digits) = unsigned.strip_prefix("0o") {
        (8, digits)
    } else if let Some(digits) = unsigned.strip_prefix("0O") {
        (8, digits)
    } else if let Some(digits) = unsigned.strip_prefix("0b") {
        (2, digits)
    } else if let Some(digits) = unsigned.strip_prefix("0B") {
        (2, digits)
    } else {
        (10, unsigned)
    };
    // Digits must all be valid in the radix
    let mut int = BigInt::parse_bytes(digits.as_bytes(), radix)
        .ok_or_else(|| BuiltinError::argument_invalid("invalid digit found in string"))?;
    if negative {
        int = -int;
    }
    let value = value::make::int(arena, int, Span::default())?;
    Ok(value)
}

/// `dec $int_to_text(int) : text`, the number printed.
pub fn int_to_text(
    arena: &mut Arena,
    targs: &[Typ],
    values: &[Value],
) -> Result<Value, BuiltinError> {
    extract::zero(targs)?;
    let value_int = extract::one(values)?;
    let text = numeric_text(arena, value_int)?;
    let value = value::make::text(arena, text, Span::default())?;
    Ok(value)
}

/// `dec $split_text(text, text) : text*`,
/// the pieces between a one-byte separator.
pub fn split_text(
    arena: &mut Arena,
    targs: &[Typ],
    values: &[Value],
) -> Result<Value, BuiltinError> {
    extract::zero(targs)?;
    let (value_text, value_separator) = extract::two(values)?;
    let text = text_of_value(arena, value_text)?;
    let separator = text_of_value(arena, value_separator)?;
    // The separator is a single byte
    if separator.len() != 1 {
        return Err(BuiltinError::argument_invalid("separator must be one byte"));
    }
    let separator = char::from(separator.as_bytes()[0]);
    let parts = text.split(separator).map(str::to_owned).collect::<Vec<_>>();
    let parts = parts
        .into_iter()
        .map(|part| value::make::text(arena, part, Span::default()))
        .collect::<Result<Vec<_>, _>>()?;
    let typ_list = typ::make::list(typ::make::bool());
    let value = value::make::list(arena, typ_list.node.into(), parts, Span::default())?;
    Ok(value)
}

/// `dec $strip_prefix(text, text) : text`,
/// the text without its prefix, which must be present.
pub fn strip_prefix(
    arena: &mut Arena,
    targs: &[Typ],
    values: &[Value],
) -> Result<Value, BuiltinError> {
    extract::zero(targs)?;
    let (value_text, value_prefix) = extract::two(values)?;
    let text = text_of_value(arena, value_text)?;
    let prefix = text_of_value(arena, value_prefix)?;
    // A missing prefix is an error, not a no-op
    let text = text
        .strip_prefix(prefix)
        .ok_or_else(|| BuiltinError::argument_invalid("text does not start with prefix"))?;
    let text = text.to_owned();
    let value = value::make::text(arena, text, Span::default())?;
    Ok(value)
}

/// `dec $strip_suffix(text, text) : text`,
/// the text without its suffix, which must be present.
pub fn strip_suffix(
    arena: &mut Arena,
    targs: &[Typ],
    values: &[Value],
) -> Result<Value, BuiltinError> {
    extract::zero(targs)?;
    let (value_text, value_suffix) = extract::two(values)?;
    let text = text_of_value(arena, value_text)?;
    let suffix = text_of_value(arena, value_suffix)?;
    // A missing suffix is an error, not a no-op
    let text = text
        .strip_suffix(suffix)
        .ok_or_else(|| BuiltinError::argument_invalid("text does not end with suffix"))?;
    let text = text.to_owned();
    let value = value::make::text(arena, text, Span::default())?;
    Ok(value)
}

/// `dec $strip_all_whitespace(text) : text`, the text without spaces.
pub fn strip_all_whitespace(
    arena: &mut Arena,
    targs: &[Typ],
    values: &[Value],
) -> Result<Value, BuiltinError> {
    extract::zero(targs)?;
    let value_text = extract::one(values)?;
    let text = text_of_value(arena, value_text)?.replace(' ', "");
    let value = value::make::text(arena, text, Span::default())?;
    Ok(value)
}
