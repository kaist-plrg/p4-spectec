//! Located splice failures and collected warnings
//!
//! Parser failures retain skeleton positions; rendering failures retain EL spans.
//! Callers receive warnings separately from the result, including on failure.

use std::path::PathBuf;

use super::super::latex;
use crate::{
    diagnostic::{Diagnostic, Label, Report, Severity},
    lang::common::source::Span,
};

// == Error

/// A rejected marker, renderer failure, or file operation.
#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("cannot parse identifier")]
    Identifier(Span),
    #[error(transparent)]
    Latex(#[from] latex::Error),
    #[error("{path}: {source}")]
    Io { path: PathBuf, source: std::io::Error },
}

impl Error {
    /// Returns the responsible source span when one exists.
    pub fn span(&self) -> Span {
        match self {
            Self::Identifier(span) => span.clone(),
            Self::Latex(error) => error.span(),
            Self::Io { .. } => Span::default(),
        }
    }
}

/// Collects a splice warning without choosing an output stream.
pub(super) fn warn(warnings: &mut Vec<Report>, span: &Span, message: String) {
    let labels =
        if span.left.line == 0 { Vec::new() } else { vec![Label::primary(span, "splice marker")] };
    warnings.push(
        Diagnostic::new("splice", Severity::Warning, None, message, labels, Vec::new()).into(),
    );
}
