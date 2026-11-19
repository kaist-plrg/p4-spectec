//! Expanding flat notation into an owned tree
//!
//! `into_tree` copies atoms and expands child handles in notation order.
//! The arena retains its stored contents.

use super::super::tree;
use super::{Mixop, MixopArena, MixopKind};

impl Mixop {
    /// Copies atoms and expands child handles in notation order.
    pub fn into_tree(self, arena_mixop: &MixopArena) -> tree::Mixop {
        match arena_mixop.kind(self) {
            MixopKind::Arg => tree::Mixop::Arg,
            MixopKind::Atom(atom) => tree::Mixop::Atom(atom.clone()),
            MixopKind::Brack(atom_l, mixop, atom_r) => tree::Mixop::Brack(
                atom_l.clone(),
                Box::new(mixop.into_tree(arena_mixop)),
                atom_r.clone(),
            ),
            MixopKind::Infix(mixop_l, atom, mixop_r) => tree::Mixop::Infix(
                Box::new(mixop_l.into_tree(arena_mixop)),
                atom.clone(),
                Box::new(mixop_r.into_tree(arena_mixop)),
            ),
            MixopKind::Seq(mixops) => tree::Mixop::Seq(
                mixops
                    .iter()
                    .map(|mixop| mixop.into_tree(arena_mixop))
                    .collect(),
            ),
        }
    }
}
