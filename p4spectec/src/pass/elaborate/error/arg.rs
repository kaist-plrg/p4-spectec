//! Function parameters, arguments, calls, and clause signature diagnostics
//!
//! Argument elaboration validates counts and kinds before signatures,
//! then uses these constructors with the offending use and declaration spans.

use crate::lang::common::{Id, source::Span};

use crate::lang::il::ast as il;

use crate::diagnostic::Label;

use super::{ElabError, cause};

const FUNCTION_PARAMETER_TYPE_PARAMETER_REPEATED: &str =
    "elab/function-parameter-type-parameter-repeated";

/// Reports a repeated type parameter inside a function parameter.
pub(in crate::pass::elaborate) fn function_parameter_type_parameter_repeated(
    tparam: &Id,
    span_previous: &Span,
) -> ElabError {
    let labels = vec![
        Label::primary(&tparam.span, ""),
        Label::secondary(span_previous, "first declared here"),
    ];
    cause(
        FUNCTION_PARAMETER_TYPE_PARAMETER_REPEATED,
        format!("type parameter `{}` is repeated", tparam.node),
        labels,
        Vec::new(),
    )
}

const FUNCTION_ARGUMENT_NAME_MISMATCH: &str = "elab/function-argument-name-mismatch";

/// Reports a defining function argument whose name differs from its parameter.
pub(in crate::pass::elaborate) fn function_argument_name_mismatch(
    id_arg: &Id,
    id_param: &Id,
) -> ElabError {
    let labels = vec![
        Label::primary(&id_arg.span, ""),
        Label::secondary(&id_param.span, "function parameter declared here"),
    ];
    cause(
        FUNCTION_ARGUMENT_NAME_MISMATCH,
        format!(
            "function argument `{}` must have the same name as declared \
            function parameter `{}`",
            id_arg.node, id_param.node
        ),
        labels,
        vec![
            concat!(
                "A function argument in a definition clause binds the name ",
                "declared by its function parameter.",
            )
            .to_owned(),
        ],
    )
}

const FUNCTION_ARGUMENT_TYPE_PARAMETER_ARITY_MISMATCH: &str =
    "elab/function-argument-type-parameter-arity-mismatch";
const FUNCTION_ARGUMENT_PARAMETER_ARITY_MISMATCH: &str =
    "elab/function-argument-parameter-arity-mismatch";
const FUNCTION_ARGUMENT_SIGNATURE_MISMATCH: &str = "elab/function-argument-signature-mismatch";

fn function_signature_labels(
    id_param: &Id,
    span_arg_declaration: &Span,
    span: &Span,
) -> Vec<crate::diagnostic::Label> {
    vec![
        Label::primary(span, ""),
        Label::secondary(&id_param.span, "function parameter declared here"),
        Label::secondary(span_arg_declaration, "passed function declared here"),
    ]
}

fn function_signature_note() -> Vec<String> {
    vec![
        concat!(
            "A passed function must have the same number of type ",
            "parameters, parameter types, and return type as its function ",
            "parameter.",
        )
        .to_owned(),
    ]
}

/// Reports a function argument with the wrong number of type parameters.
pub(in crate::pass::elaborate) fn function_argument_type_parameter_arity_mismatch(
    id_param: &Id,
    id_arg: &Id,
    tparams_len_expect: usize,
    tparams_len_actual: usize,
    span: &Span,
    span_arg_declaration: &Span,
) -> ElabError {
    let text_suffix = if tparams_len_expect == 1 { "" } else { "s" };
    cause(
        FUNCTION_ARGUMENT_TYPE_PARAMETER_ARITY_MISMATCH,
        format!(
            "function parameter `{}` has {tparams_len_expect} type \
            parameter{text_suffix}, but passed function `{}` has \
            {tparams_len_actual}",
            id_param.node, id_arg.node
        ),
        function_signature_labels(id_param, span_arg_declaration, span),
        function_signature_note(),
    )
}

/// Reports a function argument with the wrong number of parameters.
pub(in crate::pass::elaborate) fn function_argument_parameter_arity_mismatch(
    id_param: &Id,
    id_arg: &Id,
    params_len_expect: usize,
    params_len_actual: usize,
    span: &Span,
    span_arg_declaration: &Span,
) -> ElabError {
    let text_suffix = if params_len_expect == 1 { "" } else { "s" };
    cause(
        FUNCTION_ARGUMENT_PARAMETER_ARITY_MISMATCH,
        format!(
            "function parameter `{}` has {params_len_expect} \
            parameter{text_suffix}, but passed function `{}` has \
            {params_len_actual}",
            id_param.node, id_arg.node
        ),
        function_signature_labels(id_param, span_arg_declaration, span),
        function_signature_note(),
    )
}

/// Reports a function argument whose full signature is incompatible.
pub(in crate::pass::elaborate) fn function_argument_signature_mismatch(
    id_param: &Id,
    id_arg: &Id,
    span: &Span,
    span_arg_declaration: &Span,
) -> ElabError {
    cause(
        FUNCTION_ARGUMENT_SIGNATURE_MISMATCH,
        format!(
            "passed function `{}` must have the same signature as function \
            parameter `{}`",
            id_arg.node, id_param.node
        ),
        function_signature_labels(id_param, span_arg_declaration, span),
        function_signature_note(),
    )
}

const FUNCTION_ARGUMENT_KIND_MISMATCH: &str = "elab/function-argument-kind-mismatch";

/// Reports an expression/function argument-kind mismatch.
pub(in crate::pass::elaborate) fn function_argument_kind_mismatch(
    description_expect: &str,
    description_actual: &str,
    span: &Span,
    span_param: &Span,
) -> ElabError {
    let labels =
        vec![Label::primary(span, ""), Label::secondary(span_param, "parameter declared here")];
    cause(
        FUNCTION_ARGUMENT_KIND_MISMATCH,
        format!(
            "expected {description_expect} argument, but got \
            {description_actual} argument"
        ),
        labels,
        Vec::new(),
    )
}

const FUNCTION_CALL_ARGUMENT_ARITY_MISMATCH: &str = "elab/function-call-argument-arity-mismatch";

/// Reports an argument list whose count differs from its parameter list.
pub(in crate::pass::elaborate) fn function_call_argument_arity_mismatch(
    id: &Id,
    args_len_expect: usize,
    args_len_actual: usize,
    span: &Span,
    span_declaration: &Span,
) -> ElabError {
    let text_suffix = if args_len_expect == 1 { "" } else { "s" };
    let message = format!(
        "function `{}` expects {args_len_expect} argument{text_suffix}, but got {args_len_actual}",
        id.node,
    );
    let labels = vec![
        Label::primary(span, ""),
        Label::secondary(span_declaration, "function declared here"),
    ];
    cause(FUNCTION_CALL_ARGUMENT_ARITY_MISMATCH, message, labels, Vec::new())
}

const FUNCTION_CALL_TYPE_ARGUMENT_ARITY_MISMATCH: &str =
    "elab/function-call-type-argument-arity-mismatch";

/// Reports a call with the wrong number of explicit type arguments.
pub(in crate::pass::elaborate) fn function_call_type_argument_arity_mismatch(
    id: &Id,
    targs_len_expect: usize,
    targs_len_actual: usize,
    span: &Span,
    span_declaration: &Span,
) -> ElabError {
    let text_suffix = if targs_len_expect == 1 { "" } else { "s" };
    let labels = vec![
        Label::primary(span, ""),
        Label::secondary(span_declaration, "function declared here"),
    ];
    cause(
        FUNCTION_CALL_TYPE_ARGUMENT_ARITY_MISMATCH,
        format!(
            "function `{}` expects {targs_len_expect} type \
            argument{text_suffix}, but got {targs_len_actual}",
            id.node
        ),
        labels,
        Vec::new(),
    )
}

const FUNCTION_CLAUSE_ARGUMENT_ARITY_MISMATCH: &str =
    "elab/function-clause-argument-arity-mismatch";

/// Reports a function clause with the wrong number of arguments.
pub(in crate::pass::elaborate) fn function_clause_argument_arity_mismatch(
    id: &Id,
    args_len_expect: usize,
    args_len_actual: usize,
    span: &Span,
    span_declaration: &Span,
) -> ElabError {
    let text_suffix = if args_len_expect == 1 { "" } else { "s" };
    let labels = vec![
        Label::primary(span, ""),
        Label::secondary(span_declaration, "function declared here"),
    ];
    cause(
        FUNCTION_CLAUSE_ARGUMENT_ARITY_MISMATCH,
        format!(
            "function `{}` was declared with {args_len_expect} \
            parameter{text_suffix}, but this clause has {args_len_actual}",
            id.node
        ),
        labels,
        Vec::new(),
    )
}

const FUNCTION_CLAUSE_TYPE_PARAMETER_MISMATCH: &str =
    "elab/function-clause-type-parameter-mismatch";

/// Reports a clause whose type parameters differ from its declaration.
pub(in crate::pass::elaborate) fn function_clause_type_parameter_mismatch(
    id: &Id,
    tparams_expect: &[Id],
    tparams_actual: &[Id],
    span: &Span,
    span_declaration: &Span,
) -> ElabError {
    let describe = |tparams: &[Id]| {
        if tparams.is_empty() {
            "none".to_owned()
        } else {
            format!(
                "<{}>",
                tparams
                    .iter()
                    .map(|id| id.node.as_ref())
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        }
    };
    let labels = vec![
        Label::primary(span, ""),
        Label::secondary(span_declaration, "expected type parameters declared here"),
    ];
    cause(
        FUNCTION_CLAUSE_TYPE_PARAMETER_MISMATCH,
        format!(
            "function `{}` was declared with type parameters {}, but this \
            clause has type parameters {}",
            id.node,
            describe(tparams_expect),
            describe(tparams_actual)
        ),
        labels,
        vec![
            concat!(
                "A function clause must repeat the declared type parameters ",
                "with the same names and order.",
            )
            .to_owned(),
        ],
    )
}

/// Names a function argument using its zero-based position.
pub(super) fn function_argument_subject(idx: usize, id_func: &Id) -> String {
    format!("argument {idx} of function '${}'", id_func.node)
}

const FUNCTION_ARGUMENT_TYPE_MISMATCH: &str = "elab/function-argument-type-mismatch";

/// Reports a function argument that cannot be cast to its parameter type.
pub(super) fn function_argument_type_mismatch(
    idx: usize,
    id_func: &Id,
    typ_expect_il: &il::Typ,
    typ_infer_il: &il::Typ,
) -> ElabError {
    super::type_mismatch(
        FUNCTION_ARGUMENT_TYPE_MISMATCH,
        function_argument_subject(idx, id_func),
        typ_expect_il,
        typ_infer_il,
    )
}
