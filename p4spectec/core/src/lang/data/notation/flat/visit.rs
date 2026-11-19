//! Traversal of flat notation
//!
//! `visit` resolves child handles and supplies atoms
//! and numbered argument positions in reading order.

use super::super::items::Item;
use super::{Mixop, MixopArena, MixopKind};

// = Traversal

impl Mixop {
    /// Visits atoms and argument positions in reading order.
    pub(crate) fn visit<'a>(
        self,
        arena_mixop: &'a MixopArena,
        mut visit_item: impl FnMut(Item<'a, usize>),
    ) {
        let mut pos = 0;
        self.visit_inner(arena_mixop, &mut pos, &mut visit_item);
    }

    /// Visits a stored node, threading the argument position.
    fn visit_inner<'a>(
        self,
        arena_mixop: &'a MixopArena,
        pos: &mut usize,
        visit_item: &mut impl FnMut(Item<'a, usize>),
    ) {
        match arena_mixop.kind(self) {
            MixopKind::Arg => {
                visit_item(Item::Arg(*pos));
                *pos += 1;
            }
            MixopKind::Atom(atom) => visit_item(Item::Atom(atom)),
            MixopKind::Brack(atom_l, mixop, atom_r) => {
                visit_item(Item::Atom(atom_l));
                mixop.visit_inner(arena_mixop, pos, visit_item);
                visit_item(Item::Atom(atom_r));
            }
            MixopKind::Infix(mixop_l, atom, mixop_r) => {
                mixop_l.visit_inner(arena_mixop, pos, visit_item);
                visit_item(Item::Atom(atom));
                mixop_r.visit_inner(arena_mixop, pos, visit_item);
            }
            MixopKind::Seq(mixops) => {
                for mixop in mixops {
                    mixop.visit_inner(arena_mixop, pos, visit_item);
                }
            }
        }
    }
}
