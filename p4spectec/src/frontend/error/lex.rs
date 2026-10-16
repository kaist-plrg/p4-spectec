//! Diagnostics for characters, text literals, holes, and comments
//!
//! Lexer sites supply responsible spans and invalid input details.
//! Character descriptions remain local to their lexical diagnostics.

use crate::lang::common::source::Span;

use crate::diagnostic::Label;

use super::{LexError, describe_utf8_error, diagnostic};

// = Helpers

/// Names control characters without putting them into diagnostic output.
fn describe_character(character: char) -> String {
    let name = match character {
        '\0' => "null",
        '\u{7}' => "bell",
        '\u{8}' => "backspace",
        '\t' => "horizontal tab",
        '\n' => "line feed",
        '\u{b}' => "vertical tab",
        '\u{c}' => "form feed",
        '\r' => "carriage return",
        '\u{1b}' => "escape",
        '\u{7f}' => "delete",
        _ if character.is_control() => "control character",
        _ => return format!("character {character:?} (U+{:04X})", u32::from(character)),
    };
    format!("U+{:04X} {name}", u32::from(character))
}

// = Text literals

const TEXT_LITERAL_INCOMPLETE: &str = "parse/text-literal-incomplete";

/// Reports an unclosed text literal.
pub(crate) fn text_literal_incomplete(span: Span) -> LexError {
    let diagnostic_error = diagnostic(
        TEXT_LITERAL_INCOMPLETE,
        "unclosed text literal".to_owned(),
        vec![Label::primary(&span, "expected a closing quote")],
    );
    Box::new(diagnostic_error.into())
}

const TEXT_CHARACTER_INVALID: &str = "parse/text-character-invalid";

/// Reports a forbidden control character in a text literal.
pub(crate) fn text_character_invalid(span: Span, character: char) -> LexError {
    let mut diagnostic_error = diagnostic(
        TEXT_CHARACTER_INVALID,
        format!("{} is not allowed literally in a text literal", describe_character(character)),
        vec![Label::primary(&span, "escape this control character")],
    );
    diagnostic_error.notes.push(format!(
        "Use `\\u{{{:X}}}` to include this character in the text.",
        u32::from(character)
    ));
    Box::new(diagnostic_error.into())
}

const TEXT_ESCAPE_INVALID: &str = "parse/text-escape-invalid";

/// Reports a forbidden text escape.
pub(crate) fn text_escape_invalid(span: Span, escape: &str) -> LexError {
    let mut diagnostic_error = diagnostic(
        TEXT_ESCAPE_INVALID,
        format!("escape `\\{}` is not allowed in a text literal", escape[1..].escape_debug()),
        vec![Label::primary(&span, "invalid escape")],
    );
    diagnostic_error.notes.push(
        concat!(
            r#"Supported escapes are \n, \r, \t, \\, \', \", \HH "#,
            r#"(two hexadecimal digits for one byte), and \u{HEX} (a Unicode scalar value)."#,
        )
        .to_owned(),
    );
    Box::new(diagnostic_error.into())
}

const TEXT_ENCODING_INVALID: &str = "parse/text-encoding-invalid";

/// Reports decoded text bytes that are not valid UTF-8.
pub(crate) fn text_encoding_invalid(span: Span, error: &std::string::FromUtf8Error) -> LexError {
    let error_utf8 = error.utf8_error();
    let mut diagnostic_error = diagnostic(
        TEXT_ENCODING_INVALID,
        "escaped/decoded bytes in the text literal are not valid UTF-8".to_owned(),
        vec![Label::primary(
            &span,
            format!(
                "{} at decoded byte offset {}",
                describe_utf8_error(error.as_bytes(), &error_utf8),
                error_utf8.valid_up_to()
            ),
        )],
    );
    let byte = error.as_bytes()[error_utf8.valid_up_to()];
    diagnostic_error.notes.push(format!(
        "Hex escapes encode bytes, not Unicode characters. If you intended \
        U+{byte:04X}, use `\\u{{{byte:X}}}`; otherwise supply a complete \
        UTF-8 byte sequence."
    ));
    Box::new(diagnostic_error.into())
}

const TEXT_ESCAPE_CODEPOINT_INVALID: &str = "parse/text-escape-codepoint-invalid";

/// Reports a text escape that does not encode a Unicode scalar value.
pub(crate) fn text_escape_codepoint_invalid(span: Span, digits: &str) -> LexError {
    // Surrogates fit in u32; larger values and overflow exceed Unicode's maximum
    let message = match u32::from_str_radix(digits, 16) {
        Ok(0xD800..=0xDFFF) => {
            format!("Unicode escape U+{digits} is a surrogate, not a Unicode scalar value")
        }
        _ => format!("Unicode escape U+{digits} exceeds the maximum Unicode scalar value U+10FFFF"),
    };
    let mut diagnostic_error = diagnostic(
        TEXT_ESCAPE_CODEPOINT_INVALID,
        message,
        vec![Label::primary(&span, "expected a Unicode scalar value")],
    );
    diagnostic_error.notes.push(
        "Valid Unicode scalar values are U+0000–U+D7FF and U+E000–U+10FFFF; \
        U+D800–U+DFFF are reserved for UTF-16 surrogates."
            .to_owned(),
    );
    Box::new(diagnostic_error.into())
}

// = Numbered holes

const HOLE_INDEX_OUT_OF_BOUNDS: &str = "parse/hole-index-out-of-bounds";

/// Reports a numbered hole outside the supported index range.
pub(crate) fn hole_index_out_of_bounds(span: Span) -> LexError {
    let mut diagnostic_error = diagnostic(
        HOLE_INDEX_OUT_OF_BOUNDS,
        "numbered hole is out of range".to_owned(),
        vec![Label::primary(&span, "hole index exceeds the supported range")],
    );
    diagnostic_error
        .notes
        .push("Use a smaller nonnegative decimal hole index.".to_owned());
    Box::new(diagnostic_error.into())
}

// = Block comments

const BLOCK_COMMENT_INCOMPLETE: &str = "parse/block-comment-incomplete";

/// Reports an unclosed block comment.
pub(crate) fn block_comment_incomplete(span: Span, spans_open: Vec<Span>) -> LexError {
    let mut diagnostic_error = diagnostic(
        BLOCK_COMMENT_INCOMPLETE,
        "unclosed comment".to_owned(),
        vec![Label::primary(&span, "expected a closing `;)`")],
    );
    // Each remaining opener needs its own closing delimiter
    diagnostic_error.labels.extend(
        spans_open
            .into_iter()
            .map(|span| Label::secondary(&span, "comment opened here")),
    );
    Box::new(diagnostic_error.into())
}

// = Invalid characters

const CHARACTER_INVALID: &str = "parse/character-invalid";

/// Reports a character outside the token alphabet.
pub(crate) fn character_invalid(span: Span, character: char) -> LexError {
    let diagnostic_error = diagnostic(
        CHARACTER_INVALID,
        format!("{} is not allowed here", describe_character(character)),
        vec![Label::primary(&span, "invalid character")],
    );
    Box::new(diagnostic_error.into())
}
