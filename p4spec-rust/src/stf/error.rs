//! Diagnostics for STF lexer, parser, and file input failures
//!
//! Constructors retain the failure code and source span directly in a report.
//! Named file-only spans identify unreadable inputs without source occurrences.

use std::io;

use crate::{
    diagnostic::{Diagnostic, Label, Report, Severity},
    lang::common::source::Span,
};

/// A diagnostic produced while reading or parsing STF input.
pub type StfError = Box<Report>;

const CHARACTER_INVALID: &str = "stf/character-invalid";
const QUOTED_IDENTIFIER_INCOMPLETE: &str = "stf/quoted-identifier-incomplete";
const PRIORITY_OUT_OF_BOUNDS: &str = "stf/priority-out-of-bounds";
const NUMBER_INVALID: &str = "stf/number-invalid";
const INPUT_INCOMPLETE: &str = "stf/input-incomplete";
const TOKEN_UNEXPECTED: &str = "stf/token-unexpected";
const TOKEN_EXTRA: &str = "stf/token-extra";
const TOKEN_INVALID: &str = "stf/token-invalid";
const INPUT_UNREADABLE: &str = "stf/input-unreadable";

/// Builds a cause while retaining named file-only spans.
fn diagnostic(span: &Span, code: &str, message: impl Into<String>) -> StfError {
    let labels = if *span == Span::default() { Vec::new() } else { vec![Label::primary(span, "")] };
    Box::new(
        Diagnostic::new(
            "stf",
            Severity::Error,
            Some(code.to_owned()),
            message.into(),
            labels,
            Vec::new(),
        )
        .into(),
    )
}

/// Reports a character outside the STF token vocabulary.
pub(crate) fn character_invalid(span: &Span, character: char) -> StfError {
    diagnostic(span, CHARACTER_INVALID, format!("invalid character {character:?}"))
}

/// Reports a quoted identifier without its closing quote.
pub(crate) fn quoted_identifier_incomplete(span: &Span) -> StfError {
    diagnostic(span, QUOTED_IDENTIFIER_INCOMPLETE, "unterminated quoted identifier")
}

/// Reports a priority outside the supported integer range.
pub(crate) fn priority_out_of_bounds(span: &Span, spelling: &str) -> StfError {
    diagnostic(
        span,
        PRIORITY_OUT_OF_BOUNDS,
        format!("integer priority is out of range: {spelling}"),
    )
}

/// Reports digits that do not form a numeric literal.
pub(crate) fn number_invalid(span: &Span, spelling: &str) -> StfError {
    diagnostic(span, NUMBER_INVALID, format!("invalid numeric literal: {spelling}"))
}

/// Reports input ending before the grammar accepts it.
pub(crate) fn input_incomplete(span: &Span) -> StfError {
    diagnostic(span, INPUT_INCOMPLETE, "unexpected end of input")
}

/// Reports a token that the current grammar state does not accept.
pub(crate) fn token_unexpected(span: &Span) -> StfError {
    diagnostic(span, TOKEN_UNEXPECTED, "unexpected token")
}

/// Reports a token after the grammar has accepted the input.
pub(crate) fn token_extra(span: &Span) -> StfError {
    diagnostic(span, TOKEN_EXTRA, "extra token")
}

/// Reports an invalid token from the parser.
pub(crate) fn token_invalid(span: &Span) -> StfError {
    diagnostic(span, TOKEN_INVALID, "invalid token")
}

/// Reports a failure to read the input file.
pub(crate) fn input_unreadable(span: &Span, error: &io::Error) -> StfError {
    diagnostic(span, INPUT_UNREADABLE, error.to_string())
}
