//! Typed STF lexer, parser, and file input failures
//!
//! Lexer and parser checks retain their spans and diagnostic codes.
//! Reports are constructed at the diagnostic output boundary.

use std::io;

use thiserror::Error;

use crate::{
    diagnostic::{Diagnostic, Label, Report, Severity},
    lang::common::source::Span,
};

#[derive(Debug, Error)]
/// A kind of STF lexing or parsing failure.
pub enum StfErrorKind {
    #[error("invalid character {0:?}")]
    CharacterInvalid(char),
    #[error("unterminated quoted identifier")]
    QuotedIdentifierUnterminated,
    #[error("integer priority is out of range: {0}")]
    PriorityOutOfBounds(String),
    #[error("invalid numeric literal: {0}")]
    NumberInvalid(String),
    #[error("unexpected end of input")]
    InputIncomplete,
    #[error("unexpected token")]
    TokenUnexpected,
    #[error("extra token")]
    TokenExtra,
    #[error("invalid token")]
    TokenInvalid,
    #[error(transparent)]
    Io(#[from] io::Error),
}

const CHARACTER_INVALID: &str = "stf/character-invalid";
const QUOTED_IDENTIFIER_INCOMPLETE: &str = "stf/quoted-identifier-incomplete";
const PRIORITY_OUT_OF_BOUNDS: &str = "stf/priority-out-of-bounds";
const NUMBER_INVALID: &str = "stf/number-invalid";
const INPUT_INCOMPLETE: &str = "stf/input-incomplete";
const TOKEN_UNEXPECTED: &str = "stf/token-unexpected";
const TOKEN_EXTRA: &str = "stf/token-extra";
const TOKEN_INVALID: &str = "stf/token-invalid";
const INPUT_UNREADABLE: &str = "stf/input-unreadable";

/// A local STF cause with its source location.
#[derive(Debug, Error)]
#[error("{kind}")]
pub struct StfError {
    pub span: Span,
    pub kind: StfErrorKind,
}

impl StfError {
    /// Retains a frontend failure and its span.
    pub fn new(span: Span, kind: impl Into<StfErrorKind>) -> Self {
        Self { span, kind: kind.into() }
    }

    /// Converts a frontend failure at a diagnostic output boundary.
    pub fn into_report(self) -> Box<Report> {
        let Self { span, kind } = self;
        // Select the stable check code while retaining the local failure message
        let code = match &kind {
            StfErrorKind::CharacterInvalid(_) => CHARACTER_INVALID,
            StfErrorKind::QuotedIdentifierUnterminated => QUOTED_IDENTIFIER_INCOMPLETE,
            StfErrorKind::PriorityOutOfBounds(_) => PRIORITY_OUT_OF_BOUNDS,
            StfErrorKind::NumberInvalid(_) => NUMBER_INVALID,
            StfErrorKind::InputIncomplete => INPUT_INCOMPLETE,
            StfErrorKind::TokenUnexpected => TOKEN_UNEXPECTED,
            StfErrorKind::TokenExtra => TOKEN_EXTRA,
            StfErrorKind::TokenInvalid => TOKEN_INVALID,
            StfErrorKind::Io(_) => INPUT_UNREADABLE,
        };
        // Retain named file-only spans without inventing a source occurrence
        let labels =
            if span == Span::default() { Vec::new() } else { vec![Label::primary(&span, "")] };
        Box::new(
            Diagnostic::new(
                "stf",
                Severity::Error,
                Some(code.to_owned()),
                kind.to_string(),
                labels,
                Vec::new(),
            )
            .into(),
        )
    }
}
