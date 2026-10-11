//! Traversal of flat notation
//!
//! `visit` resolves child handles and supplies atoms
//! and numbered argument positions in reading order.

use super::super::visit::VisitItem;
use super::{Mixop, MixopArena, MixopKind};

// = Traversal

impl Mixop {
    /// Visits atoms and argument positions in reading order.
    pub(crate) fn visit<'a>(
        self,
        arena_mixop: &'a MixopArena,
        mut visit_item: impl FnMut(VisitItem<'a>),
    ) {
        let mut pos = 0;
        self.visit_inner(arena_mixop, &mut pos, &mut visit_item);
    }

    /// Visits a stored node, threading the argument position.
    fn visit_inner<'a>(
        self,
        arena_mixop: &'a MixopArena,
        pos: &mut usize,
        visit_item: &mut impl FnMut(VisitItem<'a>),
    ) {
        match arena_mixop.kind(self) {
            MixopKind::Arg => {
                visit_item(VisitItem::Arg(*pos));
                *pos += 1;
            }
            MixopKind::Atom(atom) => visit_item(VisitItem::Atom(atom)),
            MixopKind::Brack(atom_l, mixop, atom_r) => {
                visit_item(VisitItem::Atom(atom_l));
                mixop.visit_inner(arena_mixop, pos, visit_item);
                visit_item(VisitItem::Atom(atom_r));
            }
            MixopKind::Infix(mixop_l, atom, mixop_r) => {
                mixop_l.visit_inner(arena_mixop, pos, visit_item);
                visit_item(VisitItem::Atom(atom));
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
