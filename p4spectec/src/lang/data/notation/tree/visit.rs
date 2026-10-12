//! Traversal of tree notation
//!
//! `visit` supplies atoms and numbered argument positions in reading order.

use super::super::items::Item;
use super::Mixop;

// = Traversal

impl Mixop {
    /// Visits atoms and argument positions in reading order.
    pub(super) fn visit<'a>(&'a self, mut visit_item: impl FnMut(Item<'a, usize>)) {
        let mut pos = 0;
        self.visit_inner(&mut pos, &mut visit_item);
    }

    /// Visits one mixop, threading the position.
    fn visit_inner<'a>(&'a self, pos: &mut usize, visit_item: &mut impl FnMut(Item<'a, usize>)) {
        match self {
            Mixop::Arg => {
                visit_item(Item::Arg(*pos));
                *pos += 1;
            }
            Mixop::Atom(atom) => visit_item(Item::Atom(atom)),
            Mixop::Brack(atom_l, mixop, atom_r) => {
                visit_item(Item::Atom(atom_l));
                mixop.visit_inner(pos, visit_item);
                visit_item(Item::Atom(atom_r));
            }
            Mixop::Infix(mixop_l, atom, mixop_r) => {
                mixop_l.visit_inner(pos, visit_item);
                visit_item(Item::Atom(atom));
                mixop_r.visit_inner(pos, visit_item);
            }
            Mixop::Seq(mixops) => {
                for mixop in mixops {
                    mixop.visit_inner(pos, visit_item);
                }
            }
        }
    }
}
