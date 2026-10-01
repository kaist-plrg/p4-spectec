//! Diagnostic constructors for SpecTec source input failures
//!
//! The facade groups lexical, grammar, token, and input diagnostics
//! in private modules while preserving constructor paths.
//! Error aliases and helpers shared across these groups live here.

mod input;
mod lex;
mod syntax;
mod token;

use crate::diagnostic::{Diagnostic, Label, Report, Severity};

// = Error aliases

/// Names a structured frontend failure without adding a wrapper.
pub type FrontendError = Box<Report>;

/// Names a lexical failure with the same representation as frontend failures.
pub type LexError = FrontendError;

// = Helpers

/// Displays exactly the invalid sequence identified by the UTF-8 decoder.
fn describe_utf8_error(bytes: &[u8], error: &std::str::Utf8Error) -> String {
    // Keep the decoder's invalid sequence without lossy text conversion
    let offset = error.valid_up_to();
    let len = error.error_len().unwrap_or(bytes.len() - offset);
    let bytes = bytes[offset..offset + len]
        .iter()
        .map(|byte| format!("0x{byte:02X}"))
        .collect::<Vec<_>>()
        .join(" ");
    // A missing length means the sequence ended before its final byte
    if error.error_len().is_none() {
        format!("truncated UTF-8 sequence: {bytes}")
    } else {
        format!("invalid UTF-8 bytes: {bytes}")
    }
}

/// Creates diagnostic data authored by the SpecTec frontend.
fn diagnostic(code: &str, message: String, labels: Vec<Label>) -> Diagnostic {
    Diagnostic::new("parse", Severity::Error, Some(code.to_owned()), message, labels, Vec::new())
}

// = Diagnostic helpers and constructors

pub(crate) use input::{
    comment_encoding_invalid, file_read_failed, input_path_read_failed, mixfix_operator_invalid,
    source_encoding_invalid,
};
pub(crate) use lex::{
    block_comment_incomplete, character_invalid, hole_index_out_of_bounds, text_character_invalid,
    text_encoding_invalid, text_escape_codepoint_invalid, text_escape_invalid,
    text_literal_incomplete,
};
pub(crate) use syntax::{
    plain_type_hint_unsupported, relation_signature_invalid, struct_field_missing,
    syntax_body_missing, syntax_identifier_missing, variant_case_missing,
};
pub(crate) use token::{describe_token, input_incomplete, token_invalid};
