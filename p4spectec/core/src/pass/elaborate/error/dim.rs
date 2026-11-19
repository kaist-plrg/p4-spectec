//! Iteration dimension inference and annotation diagnostics
//!
//! Dimension analysis calls these constructors after elaboration has populated
//! bodies, preserving both occurrence spans for conflicts found during merging.

use crate::lang::{
    common::{Id, source::Span},
    traits::print::Print,
};

use crate::diagnostic::Label;

use crate::runtime::dim::Dim;

use super::{ElabError, cause};

const ITERATION_DIMENSION_MISMATCH: &str = "elab/iteration-dimension-mismatch";

/// Reports two occurrences of an identifier with incompatible dimensions.
pub(in crate::pass::elaborate) fn iteration_dimension_mismatch(
    id: &Id,
    dim: &Dim,
    span: &Span,
    dim_previous: &Dim,
    span_previous: &Span,
) -> ElabError {
    let text_dim_actual = Print::to_string(dim);
    let text_dim_expect = Print::to_string(dim_previous);
    let labels = vec![
        Label::primary(span, ""),
        Label::secondary(
            span_previous,
            format!("other occurrence has dimension `{text_dim_expect}`"),
        ),
    ];
    cause(
        ITERATION_DIMENSION_MISMATCH,
        format!(
            "identifier `{}` has incompatible iteration dimensions: \
            `{text_dim_expect}` and `{text_dim_actual}`",
            id.node
        ),
        labels,
        Vec::new(),
    )
}

const ITERATION_IDENTIFIER_TYPE_MISMATCH: &str = "elab/iteration-identifier-type-mismatch";

/// Reports incompatible types attached to two occurrences of one identifier.
pub(in crate::pass::elaborate) fn iteration_identifier_type_mismatch(
    id: &Id,
    typ: &impl Print,
    typ_other: &impl Print,
) -> ElabError {
    cause(
        ITERATION_IDENTIFIER_TYPE_MISMATCH,
        format!(
            "identifier `{}` has incompatible types `{}` and `{}` during \
            dimension analysis",
            id.node,
            Print::to_string(typ),
            Print::to_string(typ_other)
        ),
        vec![Label::primary(&id.span, "")],
        Vec::new(),
    )
}

const ITERATION_ANNOTATION_INVALID: &str = "elab/iteration-annotation-invalid";

/// Reports IL that arrives at analysis with an existing iteration annotation.
pub(in crate::pass::elaborate) fn iteration_annotation_invalid(
    span: &Span,
    description: &str,
) -> ElabError {
    cause(
        ITERATION_ANNOTATION_INVALID,
        format!("iterated {description} should initially have no annotations"),
        vec![Label::primary(span, "")],
        Vec::new(),
    )
}

const ITERATION_EXPRESSION_EMPTY: &str = "elab/iteration-expression-empty";
const ITERATION_PREMISE_EMPTY: &str = "elab/iteration-premise-empty";

fn empty_iteration(code: &str, span: &Span) -> ElabError {
    cause(
        code,
        "iteration has no variable to iterate over",
        vec![Label::primary(span, "")],
        vec![
            concat!(
                "Each iteration consumes one `*` or `?` from a variable inside ",
                "it; no variable has an iteration left to consume.",
            )
            .to_owned(),
        ],
    )
}

/// Reports an iterated expression without a source variable.
pub(in crate::pass::elaborate) fn iteration_expression_empty(span: &Span) -> ElabError {
    empty_iteration(ITERATION_EXPRESSION_EMPTY, span)
}

/// Reports an iterated premise without a source variable.
pub(in crate::pass::elaborate) fn iteration_premise_empty(span: &Span) -> ElabError {
    empty_iteration(ITERATION_PREMISE_EMPTY, span)
}
