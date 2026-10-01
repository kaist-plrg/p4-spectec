//! Table pattern shape, overlap, and coverage diagnostics
//!
//! Pattern failures keep the responsible row and related declarations.
//! Missing products are described using the existing notation printer.

use crate::lang::{
    common::{Id, source::Span},
    traits::{at::At, print::Print},
};

use crate::lang::il::ast;

use crate::lang::al;

use crate::diagnostic::Label;

use crate::pass::algo::binding::pattern::PatternSets;

use super::{AlgoError, cause};

const TABLE_BINDING_SHAPE_INVALID: &str = "algo/table-binding-shape-invalid";

/// Reports a table argument outside the shallow pattern language.
pub(crate) fn table_binding_shape_invalid(arg: &ast::Arg) -> AlgoError {
    cause(
        TABLE_BINDING_SHAPE_INVALID,
        format!(
            "table row pattern must be a variable or variant case, but got `{}`",
            arg.to_string()
        ),
        vec![Label::primary(&arg.span, "")],
        vec!["Either form may be upcast. Variant cases may contain only variables.".into()],
    )
}

const TABLE_BINDING_REPEATED: &str = "algo/table-binding-repeated";

/// Relates a repeated new binder to its first occurrence.
pub(crate) fn table_binding_repeated(id: &Id, span_first: &Span) -> AlgoError {
    cause(TABLE_BINDING_REPEATED, format!("table row pattern binds `{}` more than once", id.node),
        vec![Label::primary(&id.span, ""), Label::secondary(span_first, "first bound here")],
        vec!["Each table parameter contributes one independent match pattern. Reusing a variable would add an equality condition between positions, which table rows do not support.".into()])
}

const TABLE_PATTERN_TYPE_INVALID: &str = "algo/table-pattern-type-invalid";

/// Reports a pattern type whose declaration supplies no variant cases.
pub(crate) fn table_pattern_type_invalid(
    typ: &ast::Typ,
    span_decl_opt: Option<&Span>,
) -> AlgoError {
    // Keep the use primary and relate the declaration when it is named
    let mut labels = vec![Label::primary(&typ.span, "")];
    if let Some(span_decl) = span_decl_opt {
        labels.push(Label::secondary(span_decl, "type declared here"));
    }
    cause(
        TABLE_PATTERN_TYPE_INVALID,
        format!("table row patterns require a variant type, but got `{}`", typ.to_string()),
        labels,
        vec!["The declared cases determine which patterns the table rows must cover.".into()],
    )
}

const TABLE_PATTERN_OVERLAPPING: &str = "algo/table-pattern-overlapping";

/// Covers row patterns, falling back to the row for zero-argument IL.
fn table_pattern_span(row: &al::ast::TableRow) -> Span {
    if row.node.exps_signature.is_empty() { row.at() } else { row.node.exps_signature.at() }
}

/// Relates the later overlapping pattern to the earlier row.
pub(crate) fn table_pattern_overlapping(
    row: &al::ast::TableRow,
    row_earlier: &al::ast::TableRow,
) -> AlgoError {
    cause(
        TABLE_PATTERN_OVERLAPPING,
        "table row pattern overlaps an earlier row",
        vec![
            Label::primary(&table_pattern_span(row), ""),
            Label::secondary(&table_pattern_span(row_earlier), "earlier overlapping pattern"),
        ],
        vec![],
    )
}

/// Describes uncovered products without flattening argument alternatives.
fn describe_patterns(pattern_sets: &PatternSets) -> String {
    // Group notation alternatives within each argument position
    let patterns: Vec<_> = pattern_sets
        .iter()
        .map(|pattern_set| {
            let cases: Vec<_> = pattern_set.iter().map(Print::to_string).collect();
            match cases.as_slice() {
                [case] => case.clone(),
                _ => format!("{{{}}}", cases.join(" | ")),
            }
        })
        .collect();
    // Preserve the distinction between a single argument and a product
    match patterns.as_slice() {
        [pattern] => pattern.clone(),
        _ => format!("({})", patterns.join(", ")),
    }
}

const TABLE_PATTERN_INCOMPLETE: &str = "algo/table-pattern-incomplete";

/// Describes missing products and relates their declared cases.
pub(crate) fn table_pattern_incomplete(span: &Span, patterns: &[PatternSets]) -> AlgoError {
    // Preserve the deterministic order of uncovered pattern products
    let missing: Vec<_> = patterns
        .iter()
        .map(|patterns| format!("`{}`", describe_patterns(patterns)))
        .collect();
    // Relate each uncovered declaration once, in source order
    let mut spans: Vec<_> = patterns
        .iter()
        .flat_map(|pattern_sets| {
            pattern_sets
                .iter()
                .flat_map(|pattern_set| pattern_set.iter())
        })
        .map(At::at)
        .collect();
    spans.sort();
    spans.dedup();
    // Attach case declarations to the position following the final row
    let mut labels = vec![Label::primary(span, "")];
    labels.extend(
        spans
            .iter()
            .map(|span| Label::secondary(span, "case in uncovered pattern")),
    );
    cause(
        TABLE_PATTERN_INCOMPLETE,
        "table rows do not cover every declared case",
        labels,
        vec![format!("Uncovered patterns: {}.", missing.join(", "))],
    )
}

const TABLE_PATTERN_ARITY_MISMATCH: &str = "algo/table-pattern-arity-mismatch";

/// Reports inconsistent row widths in directly supplied IL.
pub(crate) fn table_pattern_arity_mismatch(
    span: &Span,
    expected: usize,
    actual: usize,
) -> AlgoError {
    cause(
        TABLE_PATTERN_ARITY_MISMATCH,
        format!("pattern arity mismatch: expected {expected}, got {actual}"),
        vec![Label::primary(span, "")],
        vec![],
    )
}

const TABLE_PARAMETER_INVALID: &str = "algo/table-parameter-invalid";

/// Reports a function parameter in a directly supplied IL table.
pub(crate) fn table_parameter_invalid(span: &Span) -> AlgoError {
    cause(
        TABLE_PARAMETER_INVALID,
        "table declaration contains a non-expression parameter",
        vec![Label::primary(span, "")],
        vec![],
    )
}
