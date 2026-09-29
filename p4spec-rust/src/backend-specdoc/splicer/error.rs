//! Located splice failures and immediate warnings
//!
//! Parser failures retain skeleton positions; rendering failures retain EL spans.
//! Warnings are rendered to stderr when they occur.

use std::path::PathBuf;

use super::super::latex;
use crate::{
    diagnostic::{Diagnostic, Label, RenderConfig, Renderer, Report, Severity},
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

/// Renders a splice warning immediately to stderr.
pub(super) fn warn(span: &Span, message: String) {
    let labels =
        if span.left.line == 0 { Vec::new() } else { vec![Label::primary(span, "splice marker")] };
    let report: Report =
        Diagnostic::new("splice", Severity::Warning, None, message, labels, Vec::new()).into();
    let mut renderer = Renderer::new(RenderConfig::default());
    if let Err(error) = renderer.render_to_stderr(&report) {
        eprintln!("{report}\n{error}");
    }
}
