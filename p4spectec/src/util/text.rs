//! Text encoding utilities

use std::fmt::Write;

/// Escapes a string for a double-quoted literal, octal-escaping other bytes.
pub(crate) fn escape_text(text: &str) -> String {
    text.bytes().fold(String::new(), |mut escaped, byte| {
        match byte {
            b'\\' => escaped.push_str("\\\\"),
            b'"' => escaped.push_str("\\\""),
            b'\n' => escaped.push_str("\\n"),
            b'\r' => escaped.push_str("\\r"),
            b'\t' => escaped.push_str("\\t"),
            b'\x08' => escaped.push_str("\\b"),
            32..=126 => escaped.push(char::from(byte)),
            _ => write!(escaped, "\\{byte:03}").unwrap(),
        }
        escaped
    })
}
