//! Expected types and declaration context for elaboration
//!
//! Expression checks carry the current type separately from its declaration.
//! Unfolding an alias changes the current type while retaining that context.
//! Notation checks retain the complete declaration and its owner during recursion.

use crate::lang::common::{Id, source::Span};

use crate::lang::il::ast as il;

/// Carries the current expected type and the check that introduced it.
#[derive(Clone, Copy)]
pub(super) struct ExpExpect<'a> {
    /// Holds the type used by the current checking rule.
    pub typ_il: &'a il::Typ,
    /// Retains the declaration context through type unfolding.
    pub kind: ExpExpectKind<'a>,
}

/// Identifies the source of an expression's expected type.
#[derive(Clone, Copy)]
pub(super) enum ExpExpectKind<'a> {
    /// Checks an expression without a corresponding declaration slot.
    Plain,
    /// Counts notation argument slots from zero, excluding literal tokens.
    NotArg {
        not_kind: NotExpectKind<'a>,
        idx: usize,
        typ_decl_il: &'a il::Typ,
        span_declaration: &'a Span,
    },
    /// Counts function parameters from zero after call-site instantiation.
    FuncArg { id_func: &'a Id, idx: usize, typ_decl_il: &'a il::Typ, span_declaration: &'a Span },
    /// Checks a function body against the declared return type.
    FuncReturn { id_func: &'a Id, typ_decl_il: &'a il::Typ, span_declaration: &'a Span },
    /// Checks a struct field against its instantiated declaration type.
    StructField { atom: &'a il::Atom, typ_decl_il: &'a il::Typ, span_declaration: &'a Span },
}

impl<'a> ExpExpect<'a> {
    /// Creates an expectation without declaration context.
    pub(super) fn plain(typ_il: &'a il::Typ) -> Self {
        Self { typ_il, kind: ExpExpectKind::Plain }
    }

    /// Creates an expectation for a zero-based notation argument slot.
    pub(super) fn not_arg(not_kind: NotExpectKind<'a>, idx: usize, typ_il: &'a il::Typ) -> Self {
        Self {
            typ_il,
            kind: ExpExpectKind::NotArg {
                not_kind,
                idx,
                typ_decl_il: typ_il,
                span_declaration: &typ_il.span,
            },
        }
    }

    /// Creates an argument expectation with its pre-substitution declaration span.
    pub(super) fn func_arg(
        id_func: &'a Id,
        idx: usize,
        typ_il: &'a il::Typ,
        span_decl: &'a Span,
    ) -> Self {
        Self {
            typ_il,
            kind: ExpExpectKind::FuncArg {
                id_func,
                idx,
                typ_decl_il: typ_il,
                span_declaration: span_decl,
            },
        }
    }

    /// Creates an expectation for a function's declared return type.
    pub(super) fn func_return(id_func: &'a Id, typ_il: &'a il::Typ) -> Self {
        Self {
            typ_il,
            kind: ExpExpectKind::FuncReturn {
                id_func,
                typ_decl_il: typ_il,
                span_declaration: &typ_il.span,
            },
        }
    }

    /// Creates a struct field expectation with its pre-substitution type span.
    pub(super) fn struct_field(
        atom: &'a il::Atom,
        typ_il: &'a il::Typ,
        span_decl: &'a Span,
    ) -> Self {
        Self {
            typ_il,
            kind: ExpExpectKind::StructField {
                atom,
                typ_decl_il: typ_il,
                span_declaration: span_decl,
            },
        }
    }
}

/// Retains a complete notation declaration during subtree matching.
#[derive(Clone, Copy)]
pub(super) struct NotExpect<'a> {
    /// Holds the complete notation, independently of the current subtree.
    pub not_typ_il: &'a il::NotTyp,
    /// Identifies the declaration that owns the notation.
    pub kind: NotExpectKind<'a>,
}

/// Distinguishes relation notation from variant case notation.
#[derive(Clone, Copy)]
pub(super) enum NotExpectKind<'a> {
    /// Names the relation being checked.
    Rel(&'a Id),
    /// Checks a case of a variant type.
    Variant,
}

impl<'a> NotExpect<'a> {
    /// Creates an expectation for a relation's complete notation.
    pub(super) fn rel(id_rel: &'a Id, not_typ_il: &'a il::NotTyp) -> Self {
        Self { not_typ_il, kind: NotExpectKind::Rel(id_rel) }
    }

    /// Creates an expectation for a variant case's complete notation.
    pub(super) fn variant(not_typ_il: &'a il::NotTyp) -> Self {
        Self { not_typ_il, kind: NotExpectKind::Variant }
    }
}

/// Pairs instantiated struct fields with their original type locations.
pub(super) struct StructExpect<'a> {
    /// Holds the nominal type produced by the struct expression.
    pub typ_il: &'a il::Typ,
    /// Identifies the complete struct declaration.
    pub span_declaration: Span,
    /// Retains each type's declaration span beside its instantiated field.
    pub typ_fields_il: Vec<(Span, il::TypField)>,
}
