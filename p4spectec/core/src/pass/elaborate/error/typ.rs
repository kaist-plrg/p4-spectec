//! Type declaration, application, and structural diagnostics
//!
//! Type elaboration calls these constructors at the failing operation;
//! each constructor retains the declaration and use spans needed by rendering.

use crate::lang::{
    common::{Id, source::Span},
    traits::print::Print,
};

use crate::diagnostic::{Label, Report};

use crate::runtime::ops::typ::TypeError;

use super::{ElabError, cause, warning};

const ELABORATION_ALTERNATIVE_MISSING: &str = "elab/elaboration-alternative-missing";

/// Reports that every silent elaboration alternative was inapplicable.
pub(in crate::pass::elaborate) fn elaboration_alternative_missing() -> ElabError {
    cause(
        ELABORATION_ALTERNATIVE_MISSING,
        "no elaboration alternative matched",
        vec![Label::primary(&Span::default(), "")],
        Vec::new(),
    )
}

const TYPE_OPERATION_INVALID: &str = "elab/type-operation-invalid";

/// Maps a reusable runtime type failure at its elaboration operation.
pub(in crate::pass::elaborate) fn type_operation_invalid(
    description_operation: &str,
    error: TypeError,
) -> ElabError {
    let message = format!("{description_operation}: {}", error.kind);
    cause(TYPE_OPERATION_INVALID, message, vec![Label::primary(&error.span, "")], Vec::new())
}

const TYPE_SHAPE_MISMATCH: &str = "elab/type-shape-mismatch";

/// Reports that an expanded type has the wrong structural shape.
pub(in crate::pass::elaborate) fn type_shape_mismatch(
    description_shape: &str,
    span: &Span,
) -> ElabError {
    cause(
        TYPE_SHAPE_MISMATCH,
        format!("cannot destruct type as {description_shape}"),
        vec![Label::primary(span, "")],
        Vec::new(),
    )
}

const TYPE_ARGUMENT_ARITY_MISMATCH: &str = "elab/type-argument-arity-mismatch";

/// Reports a named type application with the wrong number of arguments.
pub(in crate::pass::elaborate) fn type_argument_arity_mismatch(
    id: &Id,
    targs_len_expect: usize,
    targs_len_actual: usize,
    span: &Span,
    span_declaration: Option<&Span>,
) -> ElabError {
    let text_suffix = if targs_len_expect == 1 { "" } else { "s" };
    let mut labels = vec![Label::primary(span, "")];
    if let Some(span_declaration) = span_declaration {
        labels.push(Label::secondary(span_declaration, "type declared here"));
    }
    cause(
        TYPE_ARGUMENT_ARITY_MISMATCH,
        format!(
            "type `{}` expects {targs_len_expect} type \
            argument{text_suffix}, but got {targs_len_actual}",
            id.node
        ),
        labels,
        Vec::new(),
    )
}

const TYPE_PARAMETER_REPEATED: &str = "elab/type-parameter-repeated";

/// Reports the first repeated type parameter in a syntax declaration.
pub(in crate::pass::elaborate) fn type_parameter_repeated(
    tparam: &Id,
    span_previous: &Span,
) -> ElabError {
    let labels = vec![
        Label::primary(&tparam.span, ""),
        Label::secondary(span_previous, "first declared here"),
    ];
    cause(
        TYPE_PARAMETER_REPEATED,
        format!("type parameter `{}` is repeated", tparam.node),
        labels,
        Vec::new(),
    )
}

const TYPE_DECLARATION_REPEATED: &str = "elab/type-declaration-repeated";

/// Reports a repeated type declaration and relates the first declaration.
pub(in crate::pass::elaborate) fn type_declaration_repeated(
    id: &Id,
    span_previous: &Span,
) -> ElabError {
    let labels =
        vec![Label::primary(&id.span, ""), Label::secondary(span_previous, "first declared here")];
    cause(
        TYPE_DECLARATION_REPEATED,
        format!("type `{}` was already declared", id.node),
        labels,
        Vec::new(),
    )
}

const TYPE_EXTERN_IDENTIFIER_INVALID: &str = "elab/type-extern-identifier-invalid";
const TYPE_SYNTAX_IDENTIFIER_INVALID: &str = "elab/type-syntax-identifier-invalid";
const TYPE_IDENTIFIER_INVALID: &str = "elab/type-identifier-invalid";
const TYPE_PARAMETER_IDENTIFIER_INVALID: &str = "elab/type-parameter-identifier-invalid";

fn invalid_identifier(code: &str, description_kind: &str, id: &Id) -> ElabError {
    cause(
        code,
        format!("{description_kind} identifier `{}` must not have a suffix", id.node),
        vec![Label::primary(&id.span, "")],
        Vec::new(),
    )
}

/// Reports an invalid extern type identifier.
pub(in crate::pass::elaborate) fn type_extern_identifier_invalid(id: &Id) -> ElabError {
    invalid_identifier(TYPE_EXTERN_IDENTIFIER_INVALID, "type", id)
}

/// Reports an invalid syntax declaration identifier.
pub(in crate::pass::elaborate) fn type_syntax_identifier_invalid(id: &Id) -> ElabError {
    invalid_identifier(TYPE_SYNTAX_IDENTIFIER_INVALID, "type", id)
}

/// Reports an invalid type definition identifier.
pub(in crate::pass::elaborate) fn type_identifier_invalid(id: &Id) -> ElabError {
    invalid_identifier(TYPE_IDENTIFIER_INVALID, "type", id)
}

/// Reports an invalid type parameter identifier.
pub(in crate::pass::elaborate) fn type_parameter_identifier_invalid(id: &Id) -> ElabError {
    invalid_identifier(TYPE_PARAMETER_IDENTIFIER_INVALID, "type parameter", id)
}

const TYPE_EXTENSION_STRUCT_UNSUPPORTED: &str = "elab/type-extension-struct-unsupported";
const TYPE_EXTENSION_PRIMITIVE_UNSUPPORTED: &str = "elab/type-extension-primitive-unsupported";
const TYPE_EXTENSION_PARAMETER_UNSUPPORTED: &str = "elab/type-extension-parameter-unsupported";
const TYPE_EXTENSION_EXTERN_UNSUPPORTED: &str = "elab/type-extension-extern-unsupported";
const TYPE_EXTENSION_INCOMPLETE: &str = "elab/type-extension-incomplete";

fn type_extension(
    code: &str,
    description_kind: &str,
    typ: &impl Print,
    span: &Span,
    label_related: Option<(&Span, &str)>,
    note: &str,
) -> ElabError {
    let mut labels = vec![Label::primary(span, "")];
    if let Some((span_related, message)) = label_related {
        labels.push(Label::secondary(span_related, message));
    }
    cause(
        code,
        format!("extension is not allowed for {description_kind} `{}`", Print::to_string(typ)),
        labels,
        vec![note.to_owned()],
    )
}

/// Reports extension of a completed non-variant struct or alias.
pub(in crate::pass::elaborate) fn type_extension_struct_unsupported(
    typ: &impl Print,
    span: &Span,
    span_definition: &Span,
) -> ElabError {
    type_extension(
        TYPE_EXTENSION_STRUCT_UNSUPPORTED,
        "struct type",
        typ,
        span,
        Some((span_definition, "originally defined here")),
        concat!("A case-line `| T` can extend a variant only with another ", "variant's cases.",),
    )
}

/// Reports extension of a primitive type.
pub(in crate::pass::elaborate) fn type_extension_primitive_unsupported(
    typ: &impl Print,
    span: &Span,
) -> ElabError {
    type_extension(
        TYPE_EXTENSION_PRIMITIVE_UNSUPPORTED,
        "primitive type",
        typ,
        span,
        None,
        "A primitive type has no variant cases to contribute.",
    )
}

/// Reports extension of a type parameter.
pub(in crate::pass::elaborate) fn type_extension_parameter_unsupported(
    typ: &impl Print,
    span: &Span,
    span_declaration: &Span,
) -> ElabError {
    type_extension(
        TYPE_EXTENSION_PARAMETER_UNSUPPORTED,
        "type parameter",
        typ,
        span,
        Some((span_declaration, "type parameter declared here")),
        "Type parameters have no known variant cases.",
    )
}

/// Reports extension of an extern type.
pub(in crate::pass::elaborate) fn type_extension_extern_unsupported(
    typ: &impl Print,
    span: &Span,
    span_declaration: &Span,
) -> ElabError {
    type_extension(
        TYPE_EXTENSION_EXTERN_UNSUPPORTED,
        "extern type",
        typ,
        span,
        Some((span_declaration, "extern type declared here")),
        "Extern types have no variant cases.",
    )
}

/// Reports extension of a forward-declared type without a body.
pub(in crate::pass::elaborate) fn type_extension_incomplete(
    typ: &impl Print,
    span: &Span,
    span_declaration: &Span,
) -> ElabError {
    type_extension(
        TYPE_EXTENSION_INCOMPLETE,
        "incomplete type",
        typ,
        span,
        Some((span_declaration, "originally declared here")),
        "The type has no body yet, so it has no variant cases to contribute.",
    )
}

const VARIANT_CASE_SHAPE_REPEATED: &str = "elab/variant-case-shape-repeated";

/// Reports two variant cases with the same mixfix shape.
pub(in crate::pass::elaborate) fn variant_case_shape_repeated(
    description_shape: &str,
    span: &Span,
    span_previous: &Span,
) -> ElabError {
    let labels = vec![
        Label::primary(span, ""),
        Label::secondary(span_previous, "earlier case with this shape"),
    ];
    cause(
        VARIANT_CASE_SHAPE_REPEATED,
        format!(
            "variant case shape `{description_shape}` conflicts with an \
            earlier case"
        ),
        labels,
        vec![
            concat!(
                "Variant cases must differ in literal tokens or argument ",
                "positions; argument types do not distinguish cases.",
            )
            .to_owned(),
        ],
    )
}

const TYPE_PARAMETER_MISMATCH: &str = "elab/type-parameter-mismatch";

/// Reports a definition whose type parameters differ from its declaration.
pub(in crate::pass::elaborate) fn type_parameter_mismatch(
    id: &Id,
    tparams_expect: &[Id],
    tparams_actual: &[Id],
    span: &Span,
    span_declaration: &Span,
) -> ElabError {
    fn describe(tparams: &[Id]) -> String {
        if tparams.is_empty() {
            "no type parameters".to_owned()
        } else {
            format!(
                "type parameters `<{}>`",
                tparams
                    .iter()
                    .map(|id| id.node.as_ref())
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        }
    }
    let labels = vec![
        Label::primary(span, ""),
        Label::secondary(span_declaration, "forward declaration here"),
    ];
    cause(
        TYPE_PARAMETER_MISMATCH,
        format!(
            "type `{}` was forward-declared with {}, but its definition has {}",
            id.node,
            describe(tparams_expect),
            describe(tparams_actual)
        ),
        labels,
        vec![
            concat!(
                "A type definition must repeat the declared type parameters ",
                "with the same names and order.",
            )
            .to_owned(),
        ],
    )
}

const TYPE_DEFINITION_EXTERN_UNSUPPORTED: &str = "elab/type-definition-extern-unsupported";

/// Reports an attempted body for an extern type.
pub(in crate::pass::elaborate) fn type_definition_extern_unsupported(
    id: &Id,
    span_declaration: &Span,
) -> ElabError {
    let labels = vec![
        Label::primary(&id.span, ""),
        Label::secondary(span_declaration, "extern type declared here"),
    ];
    cause(
        TYPE_DEFINITION_EXTERN_UNSUPPORTED,
        format!("extern type `{}` does not allow a definition", id.node),
        labels,
        Vec::new(),
    )
}

const TYPE_DEFINITION_MISSING: &str = "elab/type-definition-missing";

/// Warns that a forward-declared type has no definition.
pub(in crate::pass::elaborate) fn type_definition_missing(id: &Id, tparams: &[Id]) -> Report {
    let text_suffix = if tparams.is_empty() {
        String::new()
    } else {
        format!(
            "<{}>",
            tparams
                .iter()
                .map(|id| id.node.as_ref())
                .collect::<Vec<_>>()
                .join(", ")
        )
    };
    warning(
        TYPE_DEFINITION_MISSING,
        format!("type `{}{text_suffix}` was declared but not defined", id.node),
        vec![Label::primary(&id.span, "")],
        Vec::new(),
    )
}
