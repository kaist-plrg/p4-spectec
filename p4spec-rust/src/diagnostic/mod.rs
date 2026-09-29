//! Located diagnostics and output-boundary rendering
//!
//! Reports retain the language's `Span` without reading source files.
//! `Renderer` resolves source text and delegates snippet layout to codespan.
//! Semantic callers keep typed control failures separate from diagnostics.

mod render;
mod report;

pub use codespan_reporting::diagnostic::{LabelStyle, Severity};
pub use codespan_reporting::term::{Config as SnippetConfig, DisplayStyle, termcolor::ColorChoice};
pub use render::{RenderConfig, RenderError, Renderer};
pub use report::{Diagnostic, Label, Report, ReportKind};
