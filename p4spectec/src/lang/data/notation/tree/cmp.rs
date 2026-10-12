//! Structural comparison of tree notation
//!
//! Mixops compare structure and atom names, ignoring source spans.
//! Mixfix comparison interleaves atoms and arguments in notation order.

use std::cmp::Ordering;

use crate::lang::traits::{cmp::SyntaxCmp, eq::SyntaxEq};

use super::{Mixfix, Mixop};

// = Mixops

// - Structural comparison

impl Mixop {
    /// Orders two mixops by structure and atom names, lexicographically.
    ///
    /// Brackets compare the opening atom, the inner form, then the closing atom;
    /// infix compares the left form, the operator, then the right form;
    /// sequences compare the common prefix, then the length;
    /// different forms order by variant.
    /// Both walks visit equal prefixes, so they reach argument positions in step,
    /// and `compare_arg` orders the arguments at each position.
    pub(super) fn cmp_by(
        &self,
        mixop_other: &Self,
        mut compare_arg: impl FnMut(usize) -> Ordering,
    ) -> Ordering {
        let mut pos = 0;
        self.cmp_by_inner(mixop_other, &mut pos, &mut compare_arg)
    }

    /// Structural comparison, threading the position and the argument comparator.
    fn cmp_by_inner(
        &self,
        mixop_r: &Self,
        pos: &mut usize,
        compare_arg: &mut impl FnMut(usize) -> Ordering,
    ) -> Ordering {
        match (self, mixop_r) {
            // Arguments by the caller's comparator
            (Mixop::Arg, Mixop::Arg) => {
                let order = compare_arg(*pos);
                *pos += 1;
                order
            }
            // Atoms by name
            (Mixop::Atom(atom_l), Mixop::Atom(atom_r)) => atom_l.node.cmp(&atom_r.node),
            // Brackets: opening atom, inner form, closing atom
            (
                Mixop::Brack(atom_l_l, mixop_l, atom_l_r),
                Mixop::Brack(atom_r_l, mixop_r, atom_r_r),
            ) => atom_l_l
                .node
                .cmp(&atom_r_l.node)
                .then_with(|| mixop_l.cmp_by_inner(mixop_r, pos, compare_arg))
                .then_with(|| atom_l_r.node.cmp(&atom_r_r.node)),
            // Infix: left form, operator, right form
            (
                Mixop::Infix(mixop_l_l, atom_l, mixop_l_r),
                Mixop::Infix(mixop_r_l, atom_r, mixop_r_r),
            ) => mixop_l_l
                .cmp_by_inner(mixop_r_l, pos, compare_arg)
                .then_with(|| atom_l.node.cmp(&atom_r.node))
                .then_with(|| mixop_l_r.cmp_by_inner(mixop_r_r, pos, compare_arg)),
            // Sequences: common prefix first, then length
            (Mixop::Seq(mixops_l), Mixop::Seq(mixops_r)) => {
                for (mixop_l, mixop_r) in mixops_l.iter().zip(mixops_r) {
                    let order = mixop_l.cmp_by_inner(mixop_r, pos, compare_arg);
                    if order != Ordering::Equal {
                        return order;
                    }
                }
                mixops_l.len().cmp(&mixops_r.len())
            }
            // Different forms order by variant
            _ => self.tag().cmp(&mixop_r.tag()),
        }
    }
}

// - Equality and ordering

impl PartialEq for Mixop {
    fn eq(&self, mixop_other: &Self) -> bool {
        self.cmp(mixop_other).is_eq()
    }
}

impl Eq for Mixop {}

impl Ord for Mixop {
    fn cmp(&self, mixop_other: &Self) -> Ordering {
        self.cmp_by(mixop_other, |_| Ordering::Equal)
    }
}

impl PartialOrd for Mixop {
    fn partial_cmp(&self, mixop_other: &Self) -> Option<Ordering> {
        Some(self.cmp(mixop_other))
    }
}

// - Syntax comparison

impl SyntaxEq for Mixop {
    fn syntax_eq(&self, mixop_other: &Self) -> bool {
        self == mixop_other
    }
}

// = Mixfix forms

// - Structural comparison

impl<T> Mixfix<T> {
    /// Whether two mixfixes have the same structure and atom names.
    ///
    /// Atom spans and arguments are not compared.
    pub fn eq_mixop<U>(&self, mixfix_other: &Mixfix<U>) -> bool {
        self.mixop.as_ref() == mixfix_other.mixop.as_ref()
    }

    /// Orders two mixfixes as the walk of their mixops meets atoms and arguments.
    ///
    /// Atoms compare by name; `compare_arg` orders the arguments
    /// at each position both mixops reach.
    pub fn cmp_by<U>(
        &self,
        mixfix_other: &Mixfix<U>,
        mut compare_arg: impl FnMut(&T, &U) -> Ordering,
    ) -> Ordering {
        self.mixop.cmp_by(mixfix_other.mixop.as_ref(), |pos| {
            compare_arg(&self.args[pos], &mixfix_other.args[pos])
        })
    }
}

// - Syntax comparison

impl<T: SyntaxEq> SyntaxEq for Mixfix<T> {
    fn syntax_eq(&self, mixfix_other: &Self) -> bool {
        self.eq_mixop(mixfix_other)
            && self
                .args
                .iter()
                .zip(&mixfix_other.args)
                .all(|(arg_l, arg_r)| arg_l.syntax_eq(arg_r))
    }
}

impl<T: SyntaxCmp> SyntaxCmp for Mixfix<T> {
    fn syntax_cmp(&self, mixfix_other: &Self) -> Ordering {
        self.cmp_by(mixfix_other, SyntaxCmp::syntax_cmp)
    }
}
