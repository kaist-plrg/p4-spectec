//! Errors produced while reading and rendering P4 programs
//!
//! Syntax rejection and input failures retain distinct report variants;
//! rendering failures are separate, since they arise inside a builtin.

use thiserror::Error;

use crate::{
    diagnostic::{Diagnostic, Label, Report, ReportKind, Severity},
    lang::{common::source::Span, hints::alter::AlterationError},
};

/// A misuse of the parser's scope stack.
#[derive(Clone, Debug, Error, PartialEq, Eq)]
pub enum ContextError {
    /// No scope to declare into.
    #[error("P4 context has no scope")]
    ScopeMissing,
    /// The global scope was popped.
    #[error("cannot pop the root P4 scope")]
    RootScopePopForbidden,
}

/// A parse-tree value of an unexpected shape.
#[derive(Clone, Debug, Error, PartialEq, Eq)]
pub enum ExtractError {
    /// The named extractor met a value it has no case for.
    #[error("@{0}: unexpected value")]
    ValueUnexpected(&'static str),
}

/// A lexical failure in P4 source.
#[derive(Clone, Debug, Error, PartialEq, Eq)]
pub enum LexErrorKind {
    /// The source ended inside a string.
    #[error("unterminated string literal")]
    StringUnterminated,
    /// An escape other than `\"`, `\n`, or `\\`.
    #[error("unsupported escape sequence {0}")]
    EscapeUnsupported(String),
    /// The source ended inside `/* */`.
    #[error("unterminated block comment")]
    CommentUnterminated,
    /// Digits that do not parse in their radix.
    #[error("invalid integer literal {0}")]
    IntegerInvalid(String),
    /// A `1s...` literal.
    #[error("signed integers must have width at least 2")]
    SignedWidthInvalid,
}

/// Why reading a P4 program failed.
#[derive(Clone, Debug, Error, PartialEq, Eq)]
pub enum P4ErrorKind {
    /// Building a parse-tree value failed.
    #[error(transparent)]
    Value(#[from] crate::lang::data::value::ValueError),
    /// `cc -E` failed or produced non-UTF-8 output.
    #[error("preprocessor failed with status {status:?}: {stderr}")]
    Preprocessor { status: Option<i32>, stderr: String },
    /// The scope stack was misused.
    #[error(transparent)]
    Context(#[from] ContextError),
    /// The lexer rejected the source.
    #[error(transparent)]
    Lex(#[from] LexErrorKind),
    /// The parser rejected the token stream.
    #[error("P4 syntax error")]
    Syntax,
}

const VALUE_INVALID: &str = "p4/value-invalid";
const PREPROCESSOR_FAILED: &str = "p4/preprocessor-failed";
const CONTEXT_INVALID: &str = "p4/context-invalid";
const TEXT_LITERAL_INCOMPLETE: &str = "p4/text-literal-incomplete";
const TEXT_ESCAPE_UNSUPPORTED: &str = "p4/text-escape-unsupported";
const BLOCK_COMMENT_INCOMPLETE: &str = "p4/block-comment-incomplete";
const INTEGER_LITERAL_INVALID: &str = "p4/integer-literal-invalid";
const INTEGER_WIDTH_INVALID: &str = "p4/integer-width-invalid";
const SYNTAX_INVALID: &str = "p4/syntax-invalid";

/// Distinguishes source rejection from failures preparing or constructing input.
#[derive(Debug, Error)]
pub enum P4Error {
    /// The source's lexical or grammatical form was rejected.
    #[error(transparent)]
    Syntax(Box<Report>),
    /// Preprocessing or parse-tree construction failed.
    #[error(transparent)]
    Input(Box<Report>),
}

impl P4Error {
    /// Converts a local frontend failure into its structured report and class.
    pub fn new(kind: impl Into<P4ErrorKind>, span: Span) -> Self {
        let kind = kind.into();
        // Select the stable check code while retaining the local failure message
        let code = match &kind {
            P4ErrorKind::Value(_) => VALUE_INVALID,
            P4ErrorKind::Preprocessor { .. } => PREPROCESSOR_FAILED,
            P4ErrorKind::Context(_) => CONTEXT_INVALID,
            P4ErrorKind::Lex(LexErrorKind::StringUnterminated) => TEXT_LITERAL_INCOMPLETE,
            P4ErrorKind::Lex(LexErrorKind::EscapeUnsupported(_)) => TEXT_ESCAPE_UNSUPPORTED,
            P4ErrorKind::Lex(LexErrorKind::CommentUnterminated) => BLOCK_COMMENT_INCOMPLETE,
            P4ErrorKind::Lex(LexErrorKind::IntegerInvalid(_)) => INTEGER_LITERAL_INVALID,
            P4ErrorKind::Lex(LexErrorKind::SignedWidthInvalid) => INTEGER_WIDTH_INVALID,
            P4ErrorKind::Syntax => SYNTAX_INVALID,
        };
        // Retain named file-only spans without inventing a source occurrence
        let labels =
            if span == Span::default() { Vec::new() } else { vec![Label::primary(&span, "")] };
        let report = Box::new(
            Diagnostic::new(
                "p4",
                Severity::Error,
                Some(code.to_owned()),
                kind.to_string(),
                labels,
                Vec::new(),
            )
            .into(),
        );
        // Only lexical and grammar failures count as program rejection
        match kind {
            P4ErrorKind::Lex(_) | P4ErrorKind::Syntax => Self::Syntax(report),
            _ => Self::Input(report),
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

    /// Marks columns as expanded-text coordinates unsuitable for source snippets.
    pub(crate) fn with_line_only(mut self) -> Self {
        let report = match &mut self {
            Self::Syntax(report) | Self::Input(report) => report,
        };
        // Preserve the full span while preventing original-source column lookup
        if let ReportKind::Cause(diagnostic) = &mut report.kind {
            for label in &mut diagnostic.labels {
                label.line_only = true;
            }
        }
        self
    }
}

/// Why rendering a value to P4 failed.
#[derive(Clone, Debug, Error, PartialEq, Eq)]
pub enum P4UnparseError {
    /// Structs, functions, and externs have no P4 spelling.
    #[error("cannot unparse runtime value kind {0}")]
    ValueUnsupported(&'static str),
    /// A print hint asked for an item that does not exist.
    #[error(transparent)]
    Alteration(#[from] AlterationError),
}

impl From<crate::lang::data::value::ValueError> for P4Error {
    fn from(error: crate::lang::data::value::ValueError) -> Self {
        Self::new(error, Span::default())
    }
}
