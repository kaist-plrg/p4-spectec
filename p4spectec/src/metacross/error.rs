//! Diagnostics for emitting K and Redex inputs and for the host boundary
//!
//! `Error` keeps each cause typed until a boundary renders it: the CLI and
//! `host::eval` call `into_report` once, and `render` prints with short frames.

use std::io;

use crate::{
    diagnostic::{ColorChoice, Diagnostic, DisplayStyle, RenderConfig, Renderer, Report, Severity},
    lang::{
        common::source::Span,
        data::{notation::ArityMismatch, value::ValueError},
        hints::input::InputError,
    },
    util::json::json,
};

/// A failure emitting K or Redex inputs, or reading or answering a host request.
#[derive(Debug, thiserror::Error)]
pub enum Error {
    /// A relation whose notation does not fit its input hint.
    #[error(transparent)]
    Input(#[from] InputError),
    /// Request text that is not JSON.
    #[error("malformed request JSON: {0}")]
    Json(#[from] serde_json::Error),
    /// JSON of another shape than the wire grammar gives.
    #[error("expected {expected}, but got {found}")]
    Unexpected { expected: &'static str, found: String },
    /// A request object with the wrong fields.
    #[error("{0}")]
    Request(String),
    /// A decoded value the arena cannot allocate.
    #[error(transparent)]
    Value(#[from] ValueError),
    /// Case arguments that do not fit their mixop.
    #[error(transparent)]
    Arity(#[from] ArityMismatch),
    /// An atoms matrix that gives no mixop.
    #[error("cannot rebuild the mixop: {0}")]
    Mixop(String),
    /// Atom text that is not one notation atom.
    #[error("expected one notation atom, but got {0:?}")]
    Atom(String),
    /// Atom text the notation parser rejects.
    #[error(transparent)]
    Syntax(Box<Report>),
    /// A diagnostic of another stage: elaboration, P4 parsing, or the runner.
    #[error(transparent)]
    Report(#[from] Box<Report>),
    /// The host could not build a runner for a specification path.
    #[error("cannot build a runner for {path}")]
    Init { path: String, cause: Box<Report> },
    /// The emitted text could not be written.
    #[error(transparent)]
    Output(#[from] io::Error),
}

/// A metacross result.
pub type Result<T> = std::result::Result<T, Error>;

impl Error {
    /// JSON of another shape than expected.
    pub(super) fn unexpected(expected: &'static str, json: &json) -> Self {
        Self::Unexpected { expected, found: json.to_string() }
    }

    /// Classifies this error under a metacross code; reports of other stages pass through.
    pub fn into_report(self) -> Box<Report> {
        let code = match &self {
            Self::Input(_) => "metacross/emit-invalid",
            Self::Output(_) => "metacross/output-failed",
            _ => "metacross/json-invalid",
        };
        match self {
            Self::Report(report) => report,
            Self::Syntax(report) => {
                Box::new(Report::frame(Span::default(), "decoding a wire atom", vec![*report]))
            }
            Self::Init { path, cause } => Box::new(Report::frame(
                Span::default(),
                format!("cannot build a runner for {path}"),
                vec![*cause],
            )),
            error => Box::new(
                Diagnostic::new(
                    "metacross",
                    Severity::Error,
                    Some(code.into()),
                    error.to_string(),
                    vec![],
                    vec![],
                )
                .into(),
            ),
        }
    }
}

// == Rendering

fn config(color: ColorChoice) -> RenderConfig {
    RenderConfig { frame_style: Some(DisplayStyle::Short), color, ..RenderConfig::default() }
}

/// Renders a report to stderr with short frames.
pub fn render(report: &Report) {
    if let Err(error) = Renderer::new(config(ColorChoice::Auto)).render_to_stderr(report) {
        eprintln!("{report}\ndiagnostic rendering failed: {error}");
    }
}

/// Renders a report to plain text with short frames.
pub fn render_to_string(report: &Report) -> String {
    Renderer::new(config(ColorChoice::Never))
        .render_to_string(report)
        .unwrap_or_else(|error| format!("{report}\ndiagnostic rendering failed: {error}"))
        .trim_end()
        .to_owned()
}
