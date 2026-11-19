//! Comparison of flat notation
//!
//! Canonical equality uses child canonical identities.
//! Structural comparison resolves handles and visits arguments in notation order.

use std::cmp::Ordering;

use crate::lang::{
    data::intern::{CanonEq, CanonInterner},
    traits::{cmp::SyntaxCmp, eq::SyntaxEq},
};

use super::{Mixfix, Mixop, MixopArena, MixopKind, MixopRef};

// = Mixops

// - Canonical equality

impl CanonEq for MixopKind {
    fn canon_eq(&self, interner: &CanonInterner<Self>, _: &(), kind_r: &Self) -> bool {
        // Children compare by canonical id, computed when they were interned
        let eq_mixop = |mixop_l: &Mixop, mixop_r: &Mixop| {
            interner.canon_id(*mixop_l) == interner.canon_id(*mixop_r)
        };
        match (self, kind_r) {
            (Self::Arg, Self::Arg) => true,
            (Self::Atom(atom_l), Self::Atom(atom_r)) => atom_l.node == atom_r.node,
            (
                Self::Brack(atom_l_l, mixop_l, atom_l_r),
                Self::Brack(atom_r_l, mixop_r, atom_r_r),
            ) => {
                atom_l_l.node == atom_r_l.node
                    && eq_mixop(mixop_l, mixop_r)
                    && atom_l_r.node == atom_r_r.node
            }
            (
                Self::Infix(mixop_l_l, atom_l, mixop_l_r),
                Self::Infix(mixop_r_l, atom_r, mixop_r_r),
            ) => {
                eq_mixop(mixop_l_l, mixop_r_l)
                    && atom_l.node == atom_r.node
                    && eq_mixop(mixop_l_r, mixop_r_r)
            }
            (Self::Seq(mixops_l), Self::Seq(mixops_r)) => {
                mixops_l.len() == mixops_r.len()
                    && mixops_l
                        .iter()
                        .zip(mixops_r)
                        .all(|(mixop_l, mixop_r)| eq_mixop(mixop_l, mixop_r))
            }
            _ => false,
        }
    }
}

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
    pub(in crate::lang::data::notation) fn cmp_by(
        self,
        arena_mixop: &MixopArena,
        mixop_r: Mixop,
        mut compare_arg: impl FnMut(usize) -> Ordering,
    ) -> Ordering {
        let mut pos = 0;
        self.cmp_by_inner(arena_mixop, mixop_r, &mut pos, &mut compare_arg)
    }

    /// Structural comparison, threading the position and the argument comparator.
    fn cmp_by_inner(
        self,
        arena_mixop: &MixopArena,
        mixop_r: Self,
        pos: &mut usize,
        compare_arg: &mut impl FnMut(usize) -> Ordering,
    ) -> Ordering {
        match (arena_mixop.kind(self), arena_mixop.kind(mixop_r)) {
            // Arguments by the caller's comparator
            (MixopKind::Arg, MixopKind::Arg) => {
                let order = compare_arg(*pos);
                *pos += 1;
                order
            }
            // Atoms by name
            (MixopKind::Atom(atom_l), MixopKind::Atom(atom_r)) => atom_l.node.cmp(&atom_r.node),
            // Brackets: opening atom, inner form, closing atom
            (
                MixopKind::Brack(atom_l_l, mixop_l, atom_l_r),
                MixopKind::Brack(atom_r_l, mixop_r, atom_r_r),
            ) => atom_l_l
                .node
                .cmp(&atom_r_l.node)
                .then_with(|| mixop_l.cmp_by_inner(arena_mixop, *mixop_r, pos, compare_arg))
                .then_with(|| atom_l_r.node.cmp(&atom_r_r.node)),
            // Infix: left form, operator, right form
            (
                MixopKind::Infix(mixop_l_l, atom_l, mixop_l_r),
                MixopKind::Infix(mixop_r_l, atom_r, mixop_r_r),
            ) => mixop_l_l
                .cmp_by_inner(arena_mixop, *mixop_r_l, pos, compare_arg)
                .then_with(|| atom_l.node.cmp(&atom_r.node))
                .then_with(|| mixop_l_r.cmp_by_inner(arena_mixop, *mixop_r_r, pos, compare_arg)),
            // Sequences: common prefix first, then length
            (MixopKind::Seq(mixops_l), MixopKind::Seq(mixops_r)) => {
                for (mixop_l, mixop_r) in mixops_l.iter().zip(mixops_r) {
                    let order = mixop_l.cmp_by_inner(arena_mixop, *mixop_r, pos, compare_arg);
                    if order != Ordering::Equal {
                        return order;
                    }
                }
                mixops_l.len().cmp(&mixops_r.len())
            }
            // Different forms order by variant
            (kind_l, kind_r) => kind_l.tag().cmp(&kind_r.tag()),
        }
    }
}

// - Syntax comparison

impl SyntaxEq for MixopRef<'_> {
    fn syntax_eq(&self, mixop_other: &Self) -> bool {
        assert!(
            std::ptr::eq(self.arena_mixop, mixop_other.arena_mixop),
            "mixops must belong to the same arena"
        );
        self.arena_mixop.canon_id(self.mixop) == self.arena_mixop.canon_id(mixop_other.mixop)
    }
}

impl SyntaxCmp for MixopRef<'_> {
    fn syntax_cmp(&self, mixop_other: &Self) -> Ordering {
        assert!(
            std::ptr::eq(self.arena_mixop, mixop_other.arena_mixop),
            "mixops must belong to the same arena"
        );
        self.mixop
            .cmp_by(self.arena_mixop, mixop_other.mixop, |_| Ordering::Equal)
    }
}

// = Mixfix forms

// - Structural comparison

impl<T> Mixfix<T> {
    /// Orders two cases as their expanded trees would order.
    ///
    /// Both cases must belong to `arena_mixop`;
    /// `compare_arg` orders the arguments at each position both reach.
    pub fn cmp_by<U>(
        &self,
        arena_mixop: &MixopArena,
        mixfix_other: &Mixfix<U>,
        mut compare_arg: impl FnMut(&T, &U) -> Ordering,
    ) -> Ordering {
        self.mixop.cmp_by(arena_mixop, mixfix_other.mixop, |pos| {
            compare_arg(&self.args[pos], &mixfix_other.args[pos])
        })
    }
}
