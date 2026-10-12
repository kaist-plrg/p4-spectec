//! Interning tree notation as flat nodes
//!
//! `into_flat` moves atoms; `to_flat` copies atoms from a borrowed tree.
//! Both intern children before their parent.

use super::super::{MixopArena, MixopError, flat};
use super::Mixop;

impl Mixop {
    /// Moves atoms into the arena, interning children before their parent.
    pub fn into_flat(self, arena_mixop: &mut MixopArena) -> Result<flat::Mixop, MixopError> {
        let kind = match self {
            Self::Arg => flat::MixopKind::Arg,
            Self::Atom(atom) => flat::MixopKind::Atom(atom),
            Self::Brack(atom_l, mixop, atom_r) => {
                let mixop = mixop.into_flat(arena_mixop)?;
                flat::MixopKind::Brack(atom_l, mixop, atom_r)
            }
            Self::Infix(mixop_l, atom, mixop_r) => {
                let mixop_l = mixop_l.into_flat(arena_mixop)?;
                let mixop_r = mixop_r.into_flat(arena_mixop)?;
                flat::MixopKind::Infix(mixop_l, atom, mixop_r)
            }
            Self::Seq(mixops) => flat::MixopKind::Seq(
                mixops
                    .into_iter()
                    .map(|mixop| mixop.into_flat(arena_mixop))
                    .collect::<Result<_, _>>()?,
            ),
        };
        arena_mixop.intern_kind(kind)
    }

    /// Copies atoms into the arena, interning children before their parent.
    pub(in crate::lang::data::notation) fn to_flat(
        &self,
        arena_mixop: &mut MixopArena,
    ) -> Result<flat::Mixop, MixopError> {
        let kind = match self {
            Self::Arg => flat::MixopKind::Arg,
            Self::Atom(atom) => flat::MixopKind::Atom(atom.clone()),
            Self::Brack(atom_l, mixop, atom_r) => {
                let mixop = mixop.to_flat(arena_mixop)?;
                flat::MixopKind::Brack(atom_l.clone(), mixop, atom_r.clone())
            }
            Self::Infix(mixop_l, atom, mixop_r) => {
                let mixop_l = mixop_l.to_flat(arena_mixop)?;
                let mixop_r = mixop_r.to_flat(arena_mixop)?;
                flat::MixopKind::Infix(mixop_l, atom.clone(), mixop_r)
            }
            Self::Seq(mixops) => flat::MixopKind::Seq(
                mixops
                    .iter()
                    .map(|mixop| mixop.to_flat(arena_mixop))
                    .collect::<Result<_, _>>()?,
            ),
        };
        arena_mixop.intern_kind(kind)
    }
}
