//! Declaration, lookup, and body population diagnostics
//!
//! Constructors accept the identifiers and spans retained by elaboration.
//! Repeated declarations label the later occurrence and the original binding;
//! missing bodies produce warnings without changing the semantic result.

use crate::lang::common::{Id, source::Span};

use crate::lang::il::ast as il;

use crate::diagnostic::{Label, Report};

use super::{ElabError, cause, warning};

// == Type

const TYPE_UNDEFINED: &str = "elab/type-undefined";

/// Reports a missing type at its use.
pub(crate) fn type_undefined(id: &Id) -> ElabError {
    let message = format!("type `{}` is undefined", id.node);
    cause(TYPE_UNDEFINED, message, vec![Label::primary(&id.span, "")], Vec::new())
}

const TYPE_DEFINITION_REPEATED: &str = "elab/type-definition-repeated";

/// Reports a repeated type with the first declaration's location.
pub(crate) fn type_definition_repeated(id: &Id, span_previous: &Span) -> ElabError {
    let labels =
        vec![Label::primary(&id.span, ""), Label::secondary(span_previous, "first declaration")];
    let message = format!("type `{}` was already defined", id.node);
    cause(TYPE_DEFINITION_REPEATED, message, labels, Vec::new())
}

// == Meta-variable

const META_VARIABLE_REPEATED: &str = "elab/meta-variable-repeated";

/// Reports a repeated meta-variable with the first declaration's location.
pub(crate) fn meta_variable_repeated(id: &Id, span_previous: &Span) -> ElabError {
    let labels =
        vec![Label::primary(&id.span, ""), Label::secondary(span_previous, "first declaration")];
    let message = format!("meta-variable `{}` was already defined", id.node);
    cause(META_VARIABLE_REPEATED, message, labels, Vec::new())
}

const META_VARIABLE_IDENTIFIER_INVALID: &str = "elab/meta-variable-identifier-invalid";

/// Reports a suffix on a meta-variable declaration identifier.
pub(crate) fn meta_variable_identifier_invalid(id: &Id) -> ElabError {
    let message = format!("meta-variable identifier `{}` must not have a suffix", id.node);
    cause(META_VARIABLE_IDENTIFIER_INVALID, message, vec![Label::primary(&id.span, "")], Vec::new())
}

const META_VARIABLE_TYPE_REPEATED: &str = "elab/meta-variable-type-repeated";

/// Reports the invalid use and relates the existing declaration.
pub(crate) fn meta_variable_type_repeated(id: &Id, span_previous: &Span) -> ElabError {
    let labels =
        vec![Label::primary(&id.span, ""), Label::secondary(span_previous, "first declaration")];
    let message = format!("meta-variable name `{}` is already used by a type", id.node);
    cause(META_VARIABLE_TYPE_REPEATED, message, labels, Vec::new())
}

// == Relation

const RELATION_UNDEFINED: &str = "elab/relation-undefined";

/// Reports a missing relation at its use.
pub(crate) fn relation_undefined(id: &Id) -> ElabError {
    let message = format!("relation `{}` is undefined", id.node);
    cause(RELATION_UNDEFINED, message, vec![Label::primary(&id.span, "")], Vec::new())
}

const RELATION_RULE_UNDEFINED: &str = "elab/relation-rule-undefined";

/// Reports a missing relation at its use.
pub(crate) fn relation_rule_undefined(id: &Id) -> ElabError {
    let message = format!("relation `{}` is undefined", id.node);
    cause(RELATION_RULE_UNDEFINED, message, vec![Label::primary(&id.span, "")], Vec::new())
}

const RELATION_REPEATED: &str = "elab/relation-repeated";

/// Reports a repeated relation with the first declaration's location.
pub(crate) fn relation_repeated(id: &Id, span_previous: &Span) -> ElabError {
    let labels =
        vec![Label::primary(&id.span, ""), Label::secondary(span_previous, "first declaration")];
    let message = format!("relation `{}` was already defined", id.node);
    cause(RELATION_REPEATED, message, labels, Vec::new())
}

const RELATION_EXTERN_REPEATED: &str = "elab/relation-extern-repeated";

/// Reports a repeated extern relation with the first declaration's location.
pub(crate) fn relation_extern_repeated(id: &Id, span_previous: &Span) -> ElabError {
    let labels =
        vec![Label::primary(&id.span, ""), Label::secondary(span_previous, "first declaration")];
    let message = format!("extern relation `{}` was already defined", id.node);
    cause(RELATION_EXTERN_REPEATED, message, labels, Vec::new())
}

const RELATION_EXTERN_RULE_UNSUPPORTED: &str = "elab/relation-extern-rule-unsupported";

/// Reports the invalid use and relates the existing declaration.
pub(crate) fn relation_extern_rule_unsupported(id: &Id, span_previous: &Span) -> ElabError {
    let labels = vec![
        Label::primary(&id.span, ""),
        Label::secondary(span_previous, "extern relation declared here"),
    ];
    let message = format!("extern relation `{}` does not allow rules", id.node);
    cause(RELATION_EXTERN_RULE_UNSUPPORTED, message, labels, Vec::new())
}

const RELATION_RULE_GROUP_REPEATED: &str = "elab/relation-rule-group-repeated";

/// Reports a repeated rule group with the first declaration's location.
pub(crate) fn relation_rule_group_repeated(id: &Id, span_previous: &Span) -> ElabError {
    let labels =
        vec![Label::primary(&id.span, ""), Label::secondary(span_previous, "first declaration")];
    let message = format!("rule group `{}` was already defined", id.node);
    cause(RELATION_RULE_GROUP_REPEATED, message, labels, Vec::new())
}

const RELATION_OTHERWISE_REPEATED: &str = "elab/relation-otherwise-repeated";

/// Reports both otherwise rule definitions in source order.
pub(crate) fn relation_otherwise_repeated(id: &Id, span: &Span, span_previous: &Span) -> ElabError {
    let message = format!("an `otherwise` rule for relation `{}` was already defined", id.node);
    let labels =
        vec![Label::primary(span, ""), Label::secondary(span_previous, "first otherwise rule")];
    cause(RELATION_OTHERWISE_REPEATED, message, labels, Vec::new())
}

const RELATION_RULE_MISSING: &str = "elab/relation-rule-missing";

/// Warns that a relation declaration has no rules.
pub(crate) fn relation_rule_missing(id: &Id, span: &Span) -> Report {
    let message = format!("relation `{}` has no rules defined", id.node);
    warning(RELATION_RULE_MISSING, message, vec![Label::primary(span, "")], Vec::new())
}

// == Function

const FUNCTION_UNDEFINED: &str = "elab/function-undefined";

/// Reports a missing function at its use.
pub(crate) fn function_undefined(id: &Id) -> ElabError {
    let message = format!("function `{}` is undefined", id.node);
    cause(FUNCTION_UNDEFINED, message, vec![Label::primary(&id.span, "")], Vec::new())
}

const FUNCTION_DECLARATION_REQUIRED: &str = "elab/function-declaration-required";

/// Requires a matching declaration before a function definition.
pub(crate) fn function_declaration_required(id: &Id) -> ElabError {
    let message = format!(
        "a definition of function `{}` requires a preceding matching \
        `dec` declaration",
        id.node
    );
    cause(FUNCTION_DECLARATION_REQUIRED, message, vec![Label::primary(&id.span, "")], Vec::new())
}

const FUNCTION_REPEATED: &str = "elab/function-repeated";

/// Reports a repeated function with the first declaration's location.
pub(crate) fn function_repeated(id: &Id, span_previous: &Span) -> ElabError {
    let labels =
        vec![Label::primary(&id.span, ""), Label::secondary(span_previous, "first declaration")];
    let message = format!("function `{}` was already defined", id.node);
    cause(FUNCTION_REPEATED, message, labels, Vec::new())
}

const FUNCTION_EXTERN_REPEATED: &str = "elab/function-extern-repeated";

/// Reports a repeated extern function with the first declaration's location.
pub(crate) fn function_extern_repeated(id: &Id, span_previous: &Span) -> ElabError {
    let labels =
        vec![Label::primary(&id.span, ""), Label::secondary(span_previous, "first declaration")];
    let message = format!("extern function `{}` was already defined", id.node);
    cause(FUNCTION_EXTERN_REPEATED, message, labels, Vec::new())
}

const FUNCTION_BUILTIN_REPEATED: &str = "elab/function-builtin-repeated";

/// Reports a repeated builtin function with the first declaration's location.
pub(crate) fn function_builtin_repeated(id: &Id, span_previous: &Span) -> ElabError {
    let labels =
        vec![Label::primary(&id.span, ""), Label::secondary(span_previous, "first declaration")];
    let message = format!("builtin function `{}` was already defined", id.node);
    cause(FUNCTION_BUILTIN_REPEATED, message, labels, Vec::new())
}

const FUNCTION_TYPE_PARAMETER_REPEATED: &str = "elab/function-type-parameter-repeated";

/// Reports a repeated type parameter with the first declaration's location.
pub(crate) fn function_type_parameter_repeated(id: &Id, span_previous: &Span) -> ElabError {
    let labels =
        vec![Label::primary(&id.span, ""), Label::secondary(span_previous, "first declaration")];
    let message = format!("type parameter `{}` was already defined", id.node);
    cause(FUNCTION_TYPE_PARAMETER_REPEATED, message, labels, Vec::new())
}

const FUNCTION_EXTERN_TYPE_PARAMETER_REPEATED: &str =
    "elab/function-extern-type-parameter-repeated";

/// Reports a repeated type parameter with the first declaration's location.
pub(crate) fn function_extern_type_parameter_repeated(id: &Id, span_previous: &Span) -> ElabError {
    let labels =
        vec![Label::primary(&id.span, ""), Label::secondary(span_previous, "first declaration")];
    let message = format!("type parameter `{}` was already defined", id.node);
    cause(FUNCTION_EXTERN_TYPE_PARAMETER_REPEATED, message, labels, Vec::new())
}

const FUNCTION_BUILTIN_TYPE_PARAMETER_REPEATED: &str =
    "elab/function-builtin-type-parameter-repeated";

/// Reports a repeated type parameter with the first declaration's location.
pub(crate) fn function_builtin_type_parameter_repeated(id: &Id, span_previous: &Span) -> ElabError {
    let labels =
        vec![Label::primary(&id.span, ""), Label::secondary(span_previous, "first declaration")];
    let message = format!("type parameter `{}` was already defined", id.node);
    cause(FUNCTION_BUILTIN_TYPE_PARAMETER_REPEATED, message, labels, Vec::new())
}

const FUNCTION_OTHERWISE_REPEATED: &str = "elab/function-otherwise-repeated";

/// Reports both otherwise clause definitions in source order.
pub(crate) fn function_otherwise_repeated(id: &Id, span: &Span, span_previous: &Span) -> ElabError {
    let message = format!("an `otherwise` clause for function `{}` was already defined", id.node);
    let labels =
        vec![Label::primary(span, ""), Label::secondary(span_previous, "first otherwise clause")];
    cause(FUNCTION_OTHERWISE_REPEATED, message, labels, Vec::new())
}

const FUNCTION_CLAUSE_MISSING: &str = "elab/function-clause-missing";

/// Warns that a function declaration has no clauses.
pub(crate) fn function_clause_missing(id: &Id, span: &Span) -> Report {
    let message = format!("function `{}` has no clauses defined", id.node);
    warning(FUNCTION_CLAUSE_MISSING, message, vec![Label::primary(span, "")], Vec::new())
}

// == Table

const FUNCTION_TABLE_UNDEFINED: &str = "elab/function-table-undefined";

/// Reports a missing table function at its use.
pub(crate) fn function_table_undefined(id: &Id) -> ElabError {
    let message = format!("table function `{}` is undefined", id.node);
    cause(FUNCTION_TABLE_UNDEFINED, message, vec![Label::primary(&id.span, "")], Vec::new())
}

const FUNCTION_TABLE_REQUIRED: &str = "elab/function-table-required";

/// Reports the invalid use and relates the existing declaration.
pub(crate) fn function_table_required(id: &Id, span_previous: &Span) -> ElabError {
    let labels = vec![
        Label::primary(&id.span, ""),
        Label::secondary(span_previous, "function declared here"),
    ];
    let message = format!("non-table function `{}` does not allow table rows", id.node);
    cause(FUNCTION_TABLE_REQUIRED, message, labels, Vec::new())
}

const FUNCTION_TABLE_REPEATED: &str = "elab/function-table-repeated";

/// Reports a repeated table function with the first declaration's location.
pub(crate) fn function_table_repeated(id: &Id, span_previous: &Span) -> ElabError {
    let labels =
        vec![Label::primary(&id.span, ""), Label::secondary(span_previous, "first declaration")];
    let message = format!("table function `{}` was already defined", id.node);
    cause(FUNCTION_TABLE_REPEATED, message, labels, Vec::new())
}

const TABLE_ROW_REPEATED: &str = "elab/table-row-repeated";

/// Reports the invalid use and relates the existing declaration.
pub(crate) fn table_row_repeated(id: &Id, span_previous: &Span) -> ElabError {
    let labels = vec![
        Label::primary(&id.span, ""),
        Label::secondary(span_previous, "first table definition"),
    ];
    let message = format!("table `{}` was already defined", id.node);
    cause(TABLE_ROW_REPEATED, message, labels, Vec::new())
}

const TABLE_PARAMETER_UNSUPPORTED: &str = "elab/table-parameter-unsupported";

/// Reports a table parameter that requires a function instead of a value.
pub(crate) fn table_parameter_unsupported(id: &Id, span: &Span) -> ElabError {
    let message = format!(
        "table parameter `{}` must be a value parameter, but it is a \
        function parameter",
        id.node
    );
    cause(TABLE_PARAMETER_UNSUPPORTED, message, vec![Label::primary(span, "")], Vec::new())
}

const TABLE_RETURN_TYPE_INVALID: &str = "elab/table-return-type-invalid";

/// Reports the non-boolean return type of a table declaration.
pub(crate) fn table_return_type_invalid(id: &Id, span: &Span, text_typ: &str) -> ElabError {
    let message =
        format!("table `{}` must return `bool`, but its return type is `{text_typ}`", id.node);
    cause(TABLE_RETURN_TYPE_INVALID, message, vec![Label::primary(span, "")], Vec::new())
}

const TABLE_ROW_MISSING: &str = "elab/table-row-missing";

/// Warns that a table declaration has no rows.
pub(crate) fn table_row_missing(id: &Id, span: &Span) -> Report {
    let message = format!("table `{}` has no rows defined", id.node);
    warning(TABLE_ROW_MISSING, message, vec![Label::primary(span, "")], Vec::new())
}

/// Names the return value of a function declaration.
pub(super) fn function_return_subject(id_func: &Id) -> String {
    format!("return value of function '${}'", id_func.node)
}

const FUNCTION_RETURN_TYPE_MISMATCH: &str = "elab/function-return-type-mismatch";

/// Reports a function body that cannot be cast to its declared return type.
pub(super) fn function_return_type_mismatch(
    id_func: &Id,
    typ_expect_il: &il::Typ,
    typ_infer_il: &il::Typ,
) -> ElabError {
    super::type_mismatch(
        FUNCTION_RETURN_TYPE_MISMATCH,
        function_return_subject(id_func),
        typ_expect_il,
        typ_infer_il,
    )
}
