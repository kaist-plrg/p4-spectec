//! Diagnostics for source encoding, filesystem input, and mixfix shapes
//!
//! Input entry points retain original bytes and paths in their reports.
//! Filesystem cause descriptions remain local to these constructors.

use crate::lang::common::source::Span;

use crate::diagnostic::Label;

use super::{FrontendError, describe_utf8_error, diagnostic};

// = Helpers

/// Describes a filesystem failure using frontend vocabulary.
fn describe_io_error(error: &std::io::Error) -> String {
    match error.kind() {
        std::io::ErrorKind::NotFound => "file does not exist".to_owned(),
        std::io::ErrorKind::PermissionDenied => "permission denied".to_owned(),
        _ => error.to_string(),
    }
}

// = Source encoding

const SOURCE_ENCODING_INVALID: &str = "parse/source-encoding-invalid";

/// Reports source bytes that are not valid UTF-8.
pub(crate) fn source_encoding_invalid(
    span: Span,
    bytes: &[u8],
    error: &std::str::Utf8Error,
) -> FrontendError {
    let diagnostic_error = diagnostic(
        SOURCE_ENCODING_INVALID,
        "source is not valid UTF-8".to_owned(),
        vec![Label::primary(&span, describe_utf8_error(bytes, error))],
    );
    Box::new(diagnostic_error.into())
}

const COMMENT_ENCODING_INVALID: &str = "parse/comment-encoding-invalid";

/// Reports comment bytes that are not valid UTF-8.
pub(crate) fn comment_encoding_invalid(
    span: Span,
    bytes: &[u8],
    error: &std::str::Utf8Error,
) -> FrontendError {
    let diagnostic_error = diagnostic(
        COMMENT_ENCODING_INVALID,
        "comment is not valid UTF-8".to_owned(),
        vec![Label::primary(&span, describe_utf8_error(bytes, error))],
    );
    Box::new(diagnostic_error.into())
}

// = Filesystem input

const FILE_READ_FAILED: &str = "parse/file-read-failed";

/// Reports an unreadable source file at its file-only position.
pub(crate) fn file_read_failed(span: Span, error: &std::io::Error) -> FrontendError {
    let diagnostic_error = diagnostic(
        FILE_READ_FAILED,
        format!("cannot read {:?}: {}", span.left.file, describe_io_error(error)),
        vec![Label::primary(&span, "could not read this file")],
    );
    Box::new(diagnostic_error.into())
}

const INPUT_PATH_READ_FAILED: &str = "parse/input-path-read-failed";

/// Reports a failure to enumerate an input directory.
pub(crate) fn input_path_read_failed(
    path: &std::path::Path,
    error: &std::io::Error,
) -> FrontendError {
    let diagnostic_error = diagnostic(
        INPUT_PATH_READ_FAILED,
        format!("cannot read {:?}: {}", path, describe_io_error(error)),
        Vec::new(),
    );
    Box::new(diagnostic_error.into())
}

// = Mixfix shapes

const MIXFIX_OPERATOR_INVALID: &str = "parse/mixfix-operator-invalid";

/// Reports a malformed runtime mixfix shape at its token or EOF location.
pub(crate) fn mixfix_operator_invalid(span: Span, source: &str) -> FrontendError {
    let message = if source.is_empty() {
        "mixfix operator must not be empty".to_owned()
    } else {
        format!("mixfix operator {source:?} is malformed")
    };
    let diagnostic_error = diagnostic(
        MIXFIX_OPERATOR_INVALID,
        message,
        vec![Label::primary(&span, "invalid mixfix operator")],
    );
    Box::new(diagnostic_error.into())
}
