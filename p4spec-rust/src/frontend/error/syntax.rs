//! Diagnostics for grammar constraints
//!
//! Constructors retain the parser's responsible locations
//! for relation signatures, type definitions, and syntax declarations.

use crate::lang::common::source::Span;

use crate::diagnostic::Label;

use super::{FrontendError, diagnostic};

// = Relation signatures

const RELATION_SIGNATURE_INVALID: &str = "parse/relation-signature-invalid";

/// Reports a plain type used as a relation signature.
pub(crate) fn relation_signature_invalid(span: Span) -> FrontendError {
    let mut diagnostic_error = diagnostic(
        RELATION_SIGNATURE_INVALID,
        "relation signature must be a notation type".to_owned(),
        vec![Label::primary(&span, "expected a notation type")],
    );
    diagnostic_error.notes.push(
        "A notation type includes literal tokens like `|-` or `:` that rules \
        pattern-match against. A bare type like `nat` names a set of values \
        without any tokens, so it cannot serve as a relation signature."
            .to_owned(),
    );
    Box::new(diagnostic_error.into())
}

// = Type definitions

const STRUCT_FIELD_MISSING: &str = "parse/struct-field-missing";

/// Reports a struct type without fields.
pub(crate) fn struct_field_missing(span: Span) -> FrontendError {
    let diagnostic_error = diagnostic(
        STRUCT_FIELD_MISSING,
        "empty struct type".to_owned(),
        vec![Label::primary(&span, "expected at least one field")],
    );
    Box::new(diagnostic_error.into())
}

const VARIANT_CASE_MISSING: &str = "parse/variant-case-missing";

/// Reports a variant type without cases.
pub(crate) fn variant_case_missing(span: Span) -> FrontendError {
    let diagnostic_error = diagnostic(
        VARIANT_CASE_MISSING,
        "empty variant type".to_owned(),
        vec![Label::primary(&span, "expected at least one case")],
    );
    Box::new(diagnostic_error.into())
}

const PLAIN_TYPE_HINT_UNSUPPORTED: &str = "parse/plain-type-hint-unsupported";

/// Reports hints attached to a plain type definition.
pub(crate) fn plain_type_hint_unsupported(span: Span) -> FrontendError {
    let mut diagnostic_error = diagnostic(
        PLAIN_TYPE_HINT_UNSUPPORTED,
        "hints are not allowed on a plain type definition".to_owned(),
        vec![Label::primary(&span, "plain types inherit their hints")],
    );
    diagnostic_error.notes.push(
        "A plain type definition aliases an existing type, as in `syntax x = nat`. \
        It inherits the aliased type's hints and cannot declare its own."
            .to_owned(),
    );
    Box::new(diagnostic_error.into())
}

// = Syntax declarations

const SYNTAX_BODY_MISSING: &str = "parse/syntax-body-missing";

/// Reports a syntax definition without a body.
pub(crate) fn syntax_body_missing(span: Span) -> FrontendError {
    let diagnostic_error = diagnostic(
        SYNTAX_BODY_MISSING,
        "syntax definition has no body".to_owned(),
        vec![Label::primary(&span, "expected a type body")],
    );
    Box::new(diagnostic_error.into())
}

const SYNTAX_IDENTIFIER_MISSING: &str = "parse/syntax-identifier-missing";

/// Reports a syntax declaration without identifiers.
pub(crate) fn syntax_identifier_missing(span: Span) -> FrontendError {
    let diagnostic_error = diagnostic(
        SYNTAX_IDENTIFIER_MISSING,
        "empty syntax declaration".to_owned(),
        vec![Label::primary(&span, "expected at least one identifier")],
    );
    Box::new(diagnostic_error.into())
}
