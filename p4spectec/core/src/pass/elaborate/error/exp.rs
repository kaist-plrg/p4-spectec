//! Expression inference, checking, and hint-only syntax diagnostics
//!
//! Expression attempts create these terminal causes and keep them beneath
//! uncoded attempt frames until one alternative succeeds or elaboration finishes.

use crate::lang::{common::source::Span, traits::print::Print};

use crate::lang::el::ast as el;

use crate::lang::il::ast as il;

use crate::diagnostic::{Label, Report, ReportKind};

use super::super::expect::{ExpExpect, ExpExpectKind, StructExpect};

use super::{ElabError, cause};

const EXPRESSION_INFERENCE_INVALID: &str = "elab/expression-inference-invalid";

/// Reports a construct whose type cannot be inferred without context.
pub(in crate::pass::elaborate) fn expression_inference_invalid(
    span: &Span,
    description: &str,
) -> ElabError {
    cause(
        EXPRESSION_INFERENCE_INVALID,
        format!("cannot infer type of {description}"),
        vec![Label::primary(span, "")],
        Vec::new(),
    )
}

const OPERATOR_UNOP_TYPE_MISMATCH: &str = "elab/operator-unop-type-mismatch";
const OPERATOR_BINOP_TYPE_MISMATCH: &str = "elab/operator-binop-type-mismatch";
const OPERATOR_CMPOP_TYPE_MISMATCH: &str = "elab/operator-cmpop-type-mismatch";

/// Reports a unary operator with an unsupported operand type.
pub(in crate::pass::elaborate) fn operator_unop_type_mismatch(
    op: &il::UnOp,
    exp_il: &il::Exp,
) -> ElabError {
    let typ_il = crate::phrase!(node: exp_il.note.as_ref().clone(), span: exp_il.span);
    let text = typ_il.to_string();
    cause(
        OPERATOR_UNOP_TYPE_MISMATCH,
        format!("operator '{}' is not defined for '{text}'", op.to_string()),
        vec![Label::primary(&exp_il.span, format!("operand has type '{text}'"))],
        Vec::new(),
    )
}

/// Reports a binary operator with unsupported operand types.
pub(in crate::pass::elaborate) fn operator_binop_type_mismatch(
    op: &il::BinOp,
    exp_l_il: &il::Exp,
    exp_r_il: &il::Exp,
) -> ElabError {
    operator_pair_type_mismatch(OPERATOR_BINOP_TYPE_MISMATCH, op, exp_l_il, exp_r_il)
}

/// Reports a comparison operator with unsupported operand types.
pub(in crate::pass::elaborate) fn operator_cmpop_type_mismatch(
    op: &il::CmpOp,
    exp_l_il: &il::Exp,
    exp_r_il: &il::Exp,
) -> ElabError {
    operator_pair_type_mismatch(OPERATOR_CMPOP_TYPE_MISMATCH, op, exp_l_il, exp_r_il)
}

/// Labels both operands using their inferred types before candidate casts.
fn operator_pair_type_mismatch(
    code: &str,
    op: &impl Print,
    exp_l_il: &il::Exp,
    exp_r_il: &il::Exp,
) -> ElabError {
    let typ_l_il = crate::phrase!(node: exp_l_il.note.as_ref().clone(), span: exp_l_il.span);
    let typ_r_il = crate::phrase!(node: exp_r_il.note.as_ref().clone(), span: exp_r_il.span);
    let text_l = typ_l_il.to_string();
    let text_r = typ_r_il.to_string();
    cause(
        code,
        format!("operator '{}' is not defined for '{text_l}' and '{text_r}'", op.to_string()),
        vec![
            Label::primary(&exp_l_il.span, format!("left operand has type '{text_l}'")),
            Label::primary(&exp_r_il.span, format!("right operand has type '{text_r}'")),
        ],
        Vec::new(),
    )
}

const EXPRESSION_TYPE_MISMATCH: &str = "elab/expression-type-mismatch";

/// Reports an expression that does not satisfy its expected type relationship.
pub(in crate::pass::elaborate) fn expression_type_mismatch(
    span: &Span,
    message: impl Into<String>,
) -> ElabError {
    cause(EXPRESSION_TYPE_MISMATCH, message, vec![Label::primary(span, "")], Vec::new())
}

const EXPRESSION_TYPE_SHAPE_MISMATCH: &str = "elab/expression-type-shape-mismatch";

/// Reports an incompatible type shape at the expression or path using it.
pub(in crate::pass::elaborate) fn expression_type_shape_mismatch(
    span: &Span,
    typ_il: &il::Typ,
    description_shape: &str,
) -> ElabError {
    cause(
        EXPRESSION_TYPE_SHAPE_MISMATCH,
        format!("expected {description_shape} type, but found '{}'", typ_il.to_string()),
        vec![Label::primary(span, "")],
        Vec::new(),
    )
}

/// Reports an expression that cannot be checked against the expected type shape.
pub(in crate::pass::elaborate) fn expected_expression_mismatch(
    typ_expect_il: &il::Typ,
    exp: &el::Exp,
) -> ElabError {
    cause(
        EXPRESSION_TYPE_SHAPE_MISMATCH,
        format!("expected '{}', but found '{}'", typ_expect_il.to_string(), exp.to_string()),
        vec![Label::primary(&exp.span, "")],
        Vec::new(),
    )
}

const EXPRESSION_CAST_INVALID: &str = "elab/expression-cast-invalid";

/// Reports a failed implicit cast from an inferred to an expected type.
pub(in crate::pass::elaborate) fn expression_cast_invalid(
    typ_expect_il: &il::Typ,
    typ_infer_il: &il::Typ,
) -> ElabError {
    cause(
        EXPRESSION_CAST_INVALID,
        format!(
            "expected '{}', but found '{}'",
            typ_expect_il.to_string(),
            typ_infer_il.to_string()
        ),
        vec![Label::primary(&typ_infer_il.span, "")],
        Vec::new(),
    )
}

const EXPRESSION_ITERATION_MISMATCH: &str = "elab/expression-iteration-mismatch";

/// Reports an expression whose iteration differs from the expected iteration.
pub(in crate::pass::elaborate) fn expression_iteration_mismatch(
    span: &Span,
    message: impl Into<String>,
) -> ElabError {
    cause(EXPRESSION_ITERATION_MISMATCH, message, vec![Label::primary(span, "")], Vec::new())
}

const STRUCT_FIELD_ARITY_MISMATCH: &str = "elab/struct-field-arity-mismatch";
const STRUCT_FIELD_NAME_MISMATCH: &str = "elab/struct-field-name-mismatch";
const STRUCT_FIELD_TYPE_MISMATCH: &str = "elab/struct-field-type-mismatch";
const STRUCT_FIELD_UNDEFINED: &str = "elab/struct-field-undefined";
const TUPLE_ARITY_MISMATCH: &str = "elab/tuple-arity-mismatch";
const LIST_ELEMENT_TYPE_MISMATCH: &str = "elab/list-element-type-mismatch";

/// Reports a struct literal's field count with the declared field order.
pub(in crate::pass::elaborate) fn struct_field_arity_mismatch(
    expect: &StructExpect<'_>,
    fields_len: usize,
    span: &Span,
) -> ElabError {
    let fields_len_expect = expect.typ_fields_il.len();
    let text_suffix = if fields_len_expect == 1 { "" } else { "s" };
    cause(
        STRUCT_FIELD_ARITY_MISMATCH,
        format!("expected {fields_len_expect} struct field{text_suffix}, but found {fields_len}"),
        vec![
            Label::primary(span, ""),
            Label::secondary(&expect.span_declaration, "struct declared here"),
        ],
        vec![format!(
            "expected fields in declaration order: {}",
            field_names(expect.typ_fields_il.iter().map(|(_, field_il)| field_il))
        )],
    )
}

/// Reports the field names at the same position in the literal and declaration.
pub(in crate::pass::elaborate) fn struct_field_name_mismatch(
    atom_expect: &il::Atom,
    atom: &il::Atom,
) -> ElabError {
    cause(
        STRUCT_FIELD_NAME_MISMATCH,
        format!("expected field '{}', but found '{}'", atom_expect.to_string(), atom.to_string()),
        vec![
            Label::primary(&atom.span, "unexpected field"),
            Label::secondary(&atom_expect.span, "expected field declared here"),
        ],
        vec!["Struct fields must appear in declaration order.".to_owned()],
    )
}

/// Reports an unknown field with the available fields and their declaration.
pub(in crate::pass::elaborate) fn struct_field_undefined(
    typ_il: &il::Typ,
    atom: &il::Atom,
    typ_fields_il: &[il::TypField],
    span_declaration: &Span,
) -> ElabError {
    cause(
        STRUCT_FIELD_UNDEFINED,
        format!("type '{}' has no field '{}'", typ_il.to_string(), atom.to_string()),
        vec![
            Label::primary(&atom.span, "unknown field"),
            Label::secondary(span_declaration, "struct declared here"),
        ],
        vec![format!("available fields: {}", field_names(typ_fields_il))],
    )
}

fn field_names<'a>(typ_fields_il: impl IntoIterator<Item = &'a il::TypField>) -> String {
    typ_fields_il
        .into_iter()
        .map(|field_il| field_il.atom.to_string())
        .collect::<Vec<_>>()
        .join(", ")
}

fn field_subject(atom: &il::Atom) -> String {
    format!("field '{}'", atom.to_string())
}

/// Reports tuple component counts at the expression and expected type.
pub(in crate::pass::elaborate) fn tuple_arity_mismatch(
    len_expect: usize,
    len_actual: usize,
    span: &Span,
    expect: &ExpExpect<'_>,
) -> ElabError {
    let text_suffix = if len_expect == 1 { "" } else { "s" };
    cause(
        TUPLE_ARITY_MISMATCH,
        format!("expected {len_expect} tuple element{text_suffix}, but found {len_actual}"),
        vec![Label::primary(span, ""), expected_type_label(expect)],
        Vec::new(),
    )
}

/// Labels the first inferred list element and the first incompatible element.
pub(in crate::pass::elaborate) fn list_element_type_mismatch(
    idx: usize,
    typ_base_il: &il::Typ,
    typ_il: &il::Typ,
) -> ElabError {
    cause(
        LIST_ELEMENT_TYPE_MISMATCH,
        format!(
            "list element {idx} has type '{}', but element 0 has type '{}'",
            typ_il.to_string(),
            typ_base_il.to_string()
        ),
        vec![
            Label::primary(
                &typ_il.span,
                format!("element {idx} has type '{}'", typ_il.to_string()),
            ),
            Label::secondary(
                &typ_base_il.span,
                format!("element 0 has type '{}'", typ_base_il.to_string()),
            ),
        ],
        Vec::new(),
    )
}

const VARIANT_EXPRESSION_MATCH_REPEATED: &str = "elab/variant-expression-match-repeated";

/// Reports an expression that matches more than one variant case.
pub(in crate::pass::elaborate) fn variant_expression_match_repeated(span: &Span) -> ElabError {
    cause(
        VARIANT_EXPRESSION_MATCH_REPEATED,
        "expression matches multiple variant cases",
        vec![Label::primary(span, "")],
        Vec::new(),
    )
}

const HOLE_OUTSIDE_HINT_UNSUPPORTED: &str = "elab/hole-outside-hint-unsupported";
const FUSE_OUTSIDE_HINT_UNSUPPORTED: &str = "elab/fuse-outside-hint-unsupported";
const UNPAREN_OUTSIDE_HINT_UNSUPPORTED: &str = "elab/unparen-outside-hint-unsupported";
const LATEX_OUTSIDE_HINT_UNSUPPORTED: &str = "elab/latex-outside-hint-unsupported";

/// Reports a hole expression outside a hint.
pub(in crate::pass::elaborate) fn hole_outside_hint_unsupported(span: &Span) -> ElabError {
    cause(
        HOLE_OUTSIDE_HINT_UNSUPPORTED,
        "hole is not allowed outside a hint",
        vec![Label::primary(span, "")],
        vec![
            concat!(
                "A `%`, `%N`, `%%`, or `!%` marks an argument slot inside a ",
                "`hint(...)` expression.",
            )
            .to_owned(),
        ],
    )
}

/// Reports token concatenation outside a hint.
pub(in crate::pass::elaborate) fn fuse_outside_hint_unsupported(span: &Span) -> ElabError {
    cause(
        FUSE_OUTSIDE_HINT_UNSUPPORTED,
        "token concatenation `#` is not allowed outside a hint",
        vec![Label::primary(span, "")],
        vec!["The `#` operator joins rendered hint fragments without a space.".to_owned()],
    )
}

/// Reports unparenthesizing outside a hint.
pub(in crate::pass::elaborate) fn unparen_outside_hint_unsupported(span: &Span) -> ElabError {
    cause(
        UNPAREN_OUTSIDE_HINT_UNSUPPORTED,
        "unparenthesizing operator `##` is not allowed outside a hint",
        vec![Label::primary(span, "")],
        vec!["The `##` operator strips parentheses only while rendering a hint.".to_owned()],
    )
}

/// Reports a LaTeX literal outside a hint.
pub(in crate::pass::elaborate) fn latex_outside_hint_unsupported(span: &Span) -> ElabError {
    cause(
        LATEX_OUTSIDE_HINT_UNSUPPORTED,
        "LaTeX literals are not allowed outside a hint",
        vec![Label::primary(span, "")],
        vec!["A `%latex(\"...\")` literal embeds raw LaTeX only inside a hint.".to_owned()],
    )
}

/// Selects a type diagnostic from the check's declaration context.
pub(in crate::pass::elaborate) fn expected_type_mismatch(
    expect: &ExpExpect<'_>,
    typ_infer_il: &il::Typ,
) -> ElabError {
    match expect.kind {
        ExpExpectKind::Plain => expression_cast_invalid(expect.typ_il, typ_infer_il),
        ExpExpectKind::NotArg { not_kind, idx, .. } => {
            super::not::notation_argument_type_mismatch(idx, not_kind, expect.typ_il, typ_infer_il)
        }
        ExpExpectKind::FuncArg { id_func, idx, .. } => {
            super::arg::function_argument_type_mismatch(idx, id_func, expect.typ_il, typ_infer_il)
        }
        ExpExpectKind::FuncReturn { id_func, .. } => {
            super::decl::function_return_type_mismatch(id_func, expect.typ_il, typ_infer_il)
        }
        ExpExpectKind::StructField { atom, .. } => super::type_mismatch(
            STRUCT_FIELD_TYPE_MISMATCH,
            field_subject(atom),
            expect.typ_il,
            typ_infer_il,
        ),
    }
}

/// Links existing causes to the expected type's declaration without wrapping.
pub(in crate::pass::elaborate) fn annotate_expected_type(
    expect: &ExpExpect<'_>,
    reports: &mut [Report],
) {
    // Ordinary expression checks add no declaration label
    if matches!(expect.kind, ExpExpectKind::Plain) {
        return;
    }
    let label = expected_type_label(expect);
    // Preserve each cause and avoid repeated labels from recursive calls
    let mut reports_pending: Vec<_> = reports.iter_mut().collect();
    while let Some(report) = reports_pending.pop() {
        if let ReportKind::Cause(diagnostic) = &mut report.kind
            && !diagnostic.labels.contains(&label)
        {
            diagnostic.labels.push(label.clone());
        }
        reports_pending.extend(&mut report.children);
    }
}

/// Describes the declaration slot, or the expected type for an ordinary check.
fn expected_type_label(expect: &ExpExpect<'_>) -> Label {
    let (subject, typ_decl_il, span_declaration) = match expect.kind {
        ExpExpectKind::Plain => {
            return Label::secondary(
                &expect.typ_il.span,
                format!("expected type: {}", expect.typ_il.to_string()),
            );
        }
        ExpExpectKind::NotArg { not_kind, idx, typ_decl_il, span_declaration } => {
            (super::not::notation_argument_subject(idx, not_kind), typ_decl_il, span_declaration)
        }
        ExpExpectKind::FuncArg { id_func, idx, typ_decl_il, span_declaration } => {
            (super::arg::function_argument_subject(idx, id_func), typ_decl_il, span_declaration)
        }
        ExpExpectKind::FuncReturn { id_func, typ_decl_il, span_declaration } => {
            (super::decl::function_return_subject(id_func), typ_decl_il, span_declaration)
        }
        ExpExpectKind::StructField { atom, typ_decl_il, span_declaration } => {
            (field_subject(atom), typ_decl_il, span_declaration)
        }
    };
    Label::secondary(span_declaration, format!("{subject} expects '{}'", typ_decl_il.to_string()))
}
