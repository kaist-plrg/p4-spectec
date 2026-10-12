//! Errors produced while reading and rendering P4 programs
//!
//! Syntax, preprocessing, and construction retain distinct typed causes;
//! rendering failures are separate, since they arise inside a builtin.

use thiserror::Error;

use crate::lang::{common::source::Span, hints::alter::AlterationError};

use crate::diagnostic::{Diagnostic, Report, Severity};

/// A classified parser failure with its original source location.
#[derive(Clone, Debug, Error)]
#[error("{kind}")]
pub struct P4Error {
    pub span: Span,
    pub kind: P4ErrorKind,
}

/// Why reading a P4 program failed.
#[derive(Clone, Debug, Error, PartialEq, Eq)]
pub enum P4ErrorKind {
    /// The lexer or parser rejected the source.
    #[error(transparent)]
    Syntax(#[from] P4SyntaxError),
    /// `cc -E` failed or produced non-UTF-8 output.
    #[error("preprocessor failed with status {status:?}: {stderr}")]
    Preprocess { status: Option<i32>, stderr: String },
    /// Building a parse-tree value failed.
    #[error(transparent)]
    Construction(#[from] crate::lang::data::value::ValueError),
}

/// A lexical or grammatical rejection of P4 source.
#[derive(Clone, Debug, Error, PartialEq, Eq)]
pub enum P4SyntaxError {
    #[error(transparent)]
    Lex(#[from] LexErrorKind),
    #[error("P4 syntax error")]
    GrammarInvalid,
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

const VALUE_INVALID: &str = "p4/value-invalid";
const PREPROCESSOR_FAILED: &str = "p4/preprocessor-failed";
const TEXT_LITERAL_INCOMPLETE: &str = "p4/text-literal-incomplete";
const TEXT_ESCAPE_UNSUPPORTED: &str = "p4/text-escape-unsupported";
const BLOCK_COMMENT_INCOMPLETE: &str = "p4/block-comment-incomplete";
const INTEGER_LITERAL_INVALID: &str = "p4/integer-literal-invalid";
const INTEGER_WIDTH_INVALID: &str = "p4/integer-width-invalid";
const SYNTAX_INVALID: &str = "p4/syntax-invalid";

impl P4Error {
    /// Retains a local frontend cause and its span.
    pub fn new(span: Span, kind: impl Into<P4ErrorKind>) -> Self {
        Self { span, kind: kind.into() }
    }

    /// Converts the classified failure at a diagnostic output boundary.
    pub fn into_report(self) -> Box<Report> {
        let Self { span, kind } = self;
        // Select the stable check code while retaining the local failure message
        let code = match &kind {
            P4ErrorKind::Construction(_) => VALUE_INVALID,
            P4ErrorKind::Preprocess { .. } => PREPROCESSOR_FAILED,
            P4ErrorKind::Syntax(P4SyntaxError::Lex(LexErrorKind::StringUnterminated)) => {
                TEXT_LITERAL_INCOMPLETE
            }
            P4ErrorKind::Syntax(P4SyntaxError::Lex(LexErrorKind::EscapeUnsupported(_))) => {
                TEXT_ESCAPE_UNSUPPORTED
            }
            P4ErrorKind::Syntax(P4SyntaxError::Lex(LexErrorKind::CommentUnterminated)) => {
                BLOCK_COMMENT_INCOMPLETE
            }
            P4ErrorKind::Syntax(P4SyntaxError::Lex(LexErrorKind::IntegerInvalid(_))) => {
                INTEGER_LITERAL_INVALID
            }
            P4ErrorKind::Syntax(P4SyntaxError::Lex(LexErrorKind::SignedWidthInvalid)) => {
                INTEGER_WIDTH_INVALID
            }
            P4ErrorKind::Syntax(P4SyntaxError::GrammarInvalid) => SYNTAX_INVALID,
        };
        // P4 locations identify logical files and lines without source snippets
        let notes = if span == Span::default() {
            Vec::new()
        } else {
            let file = span.left.file.name().escape_debug();
            let loc = if span.left.line == 0 {
                file.to_string()
            } else if span.left.file != span.right.file {
                format!(
                    "{file}:{}-{}:{}",
                    span.left.line,
                    span.right.file.name().escape_debug(),
                    span.right.line
                )
            } else if span.left.line != span.right.line {
                format!("{file}:{}-{}", span.left.line, span.right.line)
            } else {
                format!("{file}:{}", span.left.line)
            };
            vec![format!("at {loc}")]
        };
        Box::new(
            Diagnostic::new(
                "p4",
                Severity::Error,
                Some(code.to_owned()),
                kind.to_string(),
                Vec::new(),
                notes,
            )
            .into(),
        )
    }
}

impl From<LexErrorKind> for P4ErrorKind {
    fn from(error: LexErrorKind) -> Self {
        Self::Syntax(P4SyntaxError::Lex(error))
    }
}

impl From<crate::lang::data::value::ValueError> for P4Error {
    fn from(error: crate::lang::data::value::ValueError) -> Self {
        Self::new(Span::default(), error)
    }
}
