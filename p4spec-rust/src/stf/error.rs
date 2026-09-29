//! Structured STF failures with syntax and input classification
//!
//! Lexer and parser checks retain their spans and diagnostic codes.
//! File input failures stay distinct so callers cannot count them as rejection.

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
    InvalidCharacter(char),
    #[error("unterminated quoted identifier")]
    UnterminatedQuotedIdentifier,
    #[error("integer priority is out of range: {0}")]
    InvalidPriority(String),
    #[error("invalid numeric literal: {0}")]
    InvalidNumber(String),
    #[error("unexpected end of input")]
    UnexpectedEndOfInput,
    #[error("unexpected token")]
    UnexpectedToken,
    #[error("extra token")]
    ExtraToken,
    #[error("invalid token")]
    InvalidToken,
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

/// Distinguishes STF source rejection from failures reading its input.
#[derive(Debug, Error)]
pub enum StfError {
    /// Lexical, grammatical, or numeric source validation failed.
    #[error(transparent)]
    Syntax(Box<Report>),
    /// The source could not be read.
    #[error(transparent)]
    Input(Box<Report>),
}

impl StfError {
    /// Converts a local frontend failure into its structured report and class.
    pub fn new(kind: impl Into<StfErrorKind>, span: Span) -> Self {
        let kind = kind.into();
        // Select the stable check code while retaining the local failure message
        let code = match &kind {
            StfErrorKind::InvalidCharacter(_) => CHARACTER_INVALID,
            StfErrorKind::UnterminatedQuotedIdentifier => QUOTED_IDENTIFIER_INCOMPLETE,
            StfErrorKind::InvalidPriority(_) => PRIORITY_OUT_OF_BOUNDS,
            StfErrorKind::InvalidNumber(_) => NUMBER_INVALID,
            StfErrorKind::UnexpectedEndOfInput => INPUT_INCOMPLETE,
            StfErrorKind::UnexpectedToken => TOKEN_UNEXPECTED,
            StfErrorKind::ExtraToken => TOKEN_EXTRA,
            StfErrorKind::InvalidToken => TOKEN_INVALID,
            StfErrorKind::Io(_) => INPUT_UNREADABLE,
        };
        // Retain named file-only spans without inventing a source occurrence
        let labels =
            if span == Span::default() { Vec::new() } else { vec![Label::primary(&span, "")] };
        let report = Box::new(
            Diagnostic::new(
                "stf",
                Severity::Error,
                Some(code.to_owned()),
                kind.to_string(),
                labels,
                Vec::new(),
            )
            .into(),
        );
        // Reading failures cannot count as expected syntax rejection
        match kind {
            StfErrorKind::Io(_) => Self::Input(report),
            _ => Self::Syntax(report),
        }
    }

    /// Borrows the report without losing the failure class.
    pub fn report(&self) -> &Report {
        match self {
            Self::Syntax(report) | Self::Input(report) => report,
        }
    }

    /// Returns the original report when a caller no longer needs classification.
    pub fn into_report(self) -> Box<Report> {
        match self {
            Self::Syntax(report) | Self::Input(report) => report,
        }
    }
}
