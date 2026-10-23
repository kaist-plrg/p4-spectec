//! Prepared references pairing identifiers and variables with a frame slot
//!
//! Preparation resolves every name of a callable to a `SlotIdx` in its frame;
//! the slot is execution detail, so equality and syntax comparison ignore it.

use std::{fmt, rc::Rc};

use crate::lang::{
    common::Id,
    data::typ::{self, TypKind},
    traits::{
        eq::SyntaxEq,
        print::{Print, Printer},
    },
};

use super::Var;

/// Position of a binding in a frame.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SlotIdx(pub(crate) usize);

/// Prepared identifier occurrence, always addressing the empty iterator path.
#[derive(Clone, Debug)]
pub struct IdSlot {
    /// The name as written.
    pub id: Id,
    /// Its frame slot.
    pub slot: SlotIdx,
}

impl PartialEq for IdSlot {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl SyntaxEq for IdSlot {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id)
    }
}

impl Print for IdSlot {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        self.id.print(printer)
    }
}

/// Prepared variable occurrence, addressing its iteration path.
#[derive(Clone, Debug)]
pub struct VarSlot {
    /// Its frame slot.
    pub slot: SlotIdx,
    /// The variable as written.
    pub var: Rc<Var>,
    /// Its type with the complete iteration path applied once.
    pub typ: Rc<TypKind>,
    /// The variable one iteration outward, prepared where this variable is iterated.
    outer: Option<Rc<VarSlot>>,
}

impl VarSlot {
    /// Prepares a variable's type under its iteration path.
    pub fn new(slot: SlotIdx, var: Var) -> Self {
        let typ = typ::make::iterate(var.typ.clone(), &var.iters).node.into();
        Self { slot, var: Rc::new(var), typ, outer: None }
    }

    /// The variable one iteration outward, prepared with its iteration.
    pub fn outer(&self) -> &VarSlot {
        self.outer
            .as_deref()
            .expect("iterated variable is prepared with its outer variable")
    }

    /// Records the variable one iteration outward.
    pub(crate) fn set_outer(&mut self, var_outer: VarSlot) {
        self.outer = Some(Rc::new(var_outer));
    }
}

impl PartialEq for VarSlot {
    fn eq(&self, slot_other: &Self) -> bool {
        self.var.id.node == slot_other.var.id.node && self.var.iters == slot_other.var.iters
    }
}

impl SyntaxEq for VarSlot {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.var.syntax_eq(&other.var)
    }

    fn slice_syntax_eq(vars_l: &[Self], vars_r: &[Self]) -> bool {
        // Sort both by name and iterations, then compare pairwise
        let mut vars_l = vars_l.iter().collect::<Vec<_>>();
        let mut vars_r = vars_r.iter().collect::<Vec<_>>();
        let cmp_var = |var_l: &&Self, var_r: &&Self| {
            var_l
                .var
                .id
                .node
                .cmp(&var_r.var.id.node)
                .then_with(|| var_l.var.iters.cmp(&var_r.var.iters))
        };
        vars_l.sort_by(cmp_var);
        vars_r.sort_by(cmp_var);
        vars_l.len() == vars_r.len()
            && vars_l
                .into_iter()
                .zip(vars_r)
                .all(|(var_l, var_r)| var_l.syntax_eq(var_r))
    }
}

impl Print for VarSlot {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        self.var.print(printer)
    }
}
