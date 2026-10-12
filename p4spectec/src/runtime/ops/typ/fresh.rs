//! Fresh type-variable generation local to one type operation
//!
//! Fresh names are `__FRESH<n>`, unique within one operation;
//! they never reach the source.

use crate::lang::common::source::Span;

use crate::lang::il::ast::{self, TypKind};

use crate::phrase;

/// Counter for fresh type variables.
#[derive(Default)]
pub(crate) struct Fresh {
    next: u64,
}

impl Fresh {
    /// Mints the next fresh type parameter and the type variable naming it.
    pub(crate) fn fresh(&mut self) -> (ast::TParam, ast::Typ) {
        let next = self.next;
        self.next += 1;
        let tparam = phrase!(node: format!("__FRESH{next}").into(), span: Span::default());
        let typ_kind = TypKind::Var(tparam.clone(), vec![]);
        let typ = phrase!(node: typ_kind, span: Span::default());
        (tparam, typ)
    }
}
