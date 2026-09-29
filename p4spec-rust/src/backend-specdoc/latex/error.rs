//! Failures in canonical EL rendering and TeX layout
//!
//! Unsupported EL expressions retain their source span.
//! Structural TeX failures have no source position;
//! `Error::span` returns the generated span for those failures.

use crate::lang::common::source::Span;

/// A rejected EL expression or malformed TeX document.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum Error {
    #[error("LaTeX rendering is undefined for a hole expression")]
    Hole(Span),
    #[error("LaTeX rendering is undefined for a fuse expression")]
    Fuse(Span),
    #[error("LaTeX rendering is undefined for an unparen expression")]
    Unparen(Span),
    #[error("raw LaTeX expressions are not allowed in canonical rendering")]
    RawLatex(Span),
    #[error("Layout.resolve: width must be positive")]
    InvalidLayoutWidth,
    #[error("Doc.grid: rows require columns")]
    GridWithoutColumns,
    #[error("Doc.grid: cell count does not match columns")]
    GridCellCount { expected: usize, actual: usize },
    #[error("invalid LaTeX link target")]
    InvalidLinkTarget(String),
}

impl Error {
    /// Returns the original expression span or a generated structural span.
    pub fn span(&self) -> Span {
        match self {
            Self::Hole(span) | Self::Fuse(span) | Self::Unparen(span) | Self::RawLatex(span) => {
                span.clone()
            }
            Self::InvalidLayoutWidth
            | Self::GridWithoutColumns
            | Self::GridCellCount { .. }
            | Self::InvalidLinkTarget(_) => Span::default(),
        }
    }
}

pub(super) type Result<T> = std::result::Result<T, Error>;
