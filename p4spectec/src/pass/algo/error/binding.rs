//! Binding position and dimension diagnostics
//!
//! Binding environments describe new variables in deterministic name order.
//! Dimension failures relate the later occurrence to the first binding.

use crate::lang::{
    common::{
        Id,
        source::{Phrase, Span},
    },
    traits::print::Print,
};

use crate::lang::il::ast;

use crate::diagnostic::Label;

use crate::runtime::dim::Dim;

use crate::pass::algo::binding::bind::BEnv;

use super::{AlgoError, cause};

/// Describes the variables introduced by a binder pattern.
fn describe_bindings(benv: &BEnv) -> String {
    // Describe each name once in binding-environment order
    let ids: Vec<_> = benv
        .iter()
        .map(|(id, _)| format!("`{}`", id.node))
        .collect();
    match ids.as_slice() {
        [] => "no variables".into(),
        [id] => format!("variable {id}"),
        _ => format!("variables {}", ids.join(", ")),
    }
}

const EXPRESSION_VARIABLE_UNBOUND: &str = "algo/expression-variable-unbound";

/// Reports variables read before they are bound.
pub(crate) fn expression_variable_unbound(id: &Id) -> AlgoError {
    cause(
        EXPRESSION_VARIABLE_UNBOUND,
        format!("`{}` is not bound when this expression is evaluated", id.node),
        vec![Label::primary(&id.span, format!("`{}` is read here", id.node))],
        vec![
            "Bind the variable in an earlier input pattern or premise before reading it here."
                .into(),
        ],
    )
}

const RELATION_INPUT_VARIABLE_UNBOUND: &str = "algo/relation-input-variable-unbound";

/// Reports an unbound variable read in a relation input.
pub(crate) fn relation_input_variable_unbound(
    id: &Id,
    id_relation: &Id,
    idx_input: &Phrase<usize>,
) -> AlgoError {
    cause(
        RELATION_INPUT_VARIABLE_UNBOUND,
        format!("`{}` is not bound when this expression is evaluated", id.node),
        vec![
            Label::primary(&id.span, format!("`{}` is read here", id.node)),
            Label::secondary(
                &idx_input.span,
                format!(
                    "argument %{} of relation `{}` is evaluated as input; it cannot bind `{}`",
                    idx_input.node, id_relation.node, id.node
                ),
            ),
        ],
        vec![
            "Bind the variable in an earlier input pattern or premise before reading it here."
                .into(),
        ],
    )
}

const BINDING_NON_INVERTIBLE: &str = "algo/binding-non-invertible";

/// Reports new variables underneath an operation that cannot be inverted.
pub(crate) fn binding_non_invertible(span: &Span, construct: &str, benv: &BEnv) -> AlgoError {
    cause(BINDING_NON_INVERTIBLE,
        format!("non-invertible {construct} contains newly bound {}", describe_bindings(benv)),
        vec![Label::primary(span, "")],
        vec!["Each variable in a binding expression must correspond directly to a tuple element, variant case argument, struct field, or list element. Operators are not inverted, even when their inverse would be unique.".into()])
}

const BINDING_DIMENSION_MISMATCH: &str = "algo/binding-dimension-mismatch";

/// Relates incompatible parallel bindings of the same variable.
pub(crate) fn binding_dimension_mismatch(id: &Id, dim_l: &Dim, dim_r: &Dim) -> AlgoError {
    cause(BINDING_DIMENSION_MISMATCH,
        format!("parallel bindings for `{}` have incompatible dimensions: `{}` and `{}`", id.node, dim_l.to_string(), dim_r.to_string()),
        vec![Label::primary(&dim_r.typ.span, ""), Label::secondary(&dim_l.typ.span, "first bound here")],
        vec!["A variable can have only one type and iteration dimension within a binder pattern. These two positions give the variable different dimensions.".into()])
}

const EQUALITY_BINDING_INVALID: &str = "algo/equality-binding-invalid";

/// Reports an equality that introduces variables on both sides.
pub(crate) fn equality_binding_invalid(span: &Span, benv_l: &BEnv, benv_r: &BEnv) -> AlgoError {
    cause(EQUALITY_BINDING_INVALID,
        format!("both sides of an equality bind new variables: left side binds {}, right side binds {}", describe_bindings(benv_l), describe_bindings(benv_r)),
        vec![Label::primary(span, "")],
        vec!["An `=` premise reads as a comparison when both sides are already bound, or as a binder when one side is. With new variables on both sides it fits neither.".into()])
}

const ITERATION_LOOP_VARIABLE_MISSING: &str = "algo/iteration-loop-variable-missing";

/// Reports an iteration with no externally bound variable to range over.
pub(crate) fn iteration_loop_variable_missing(span: &Span, vars: &[ast::Var]) -> AlgoError {
    // Describe the variables whose dimensions cannot determine a loop length
    let vars: Vec<_> = vars
        .iter()
        .map(|var| format!("`{}`", var.to_string()))
        .collect();
    // Direct IL can also supply an iteration with no variables at all
    let message = match vars.as_slice() {
        [] => "iteration has no iter variable".into(),
        [var] => format!("iteration has no iter variable because {var} is newly bound"),
        _ => format!(
            "iteration has no iter variable because variables {} are newly bound",
            vars.join(", ")
        ),
    };
    cause(
        ITERATION_LOOP_VARIABLE_MISSING,
        message,
        vec![Label::primary(span, "")],
        vec![
            "An iteration needs a variable bound outside it whose values it can iterate over."
                .into(),
        ],
    )
}

const ITERATION_BINDING_INVALID: &str = "algo/iteration-binding-invalid";

/// Rejects pre-populated iteration binders in directly supplied IL.
pub(crate) fn iteration_binding_invalid(span: &Span) -> AlgoError {
    cause(
        ITERATION_BINDING_INVALID,
        "iterated premise has binding variables before binding analysis",
        vec![Label::primary(span, "")],
        vec![],
    )
}
