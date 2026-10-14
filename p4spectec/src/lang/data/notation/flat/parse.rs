//! Cached notation parsing directly into flat mixops
//!
//! EL types supply argument positions and atoms without a tree mixop.
//! Each arena retains successful string conversions with its own handles.

use crate::frontend::{error::FrontendError, parse::parse_typ};
use crate::lang::el::ast::{NotTyp, NotTypKind, Typ};

use super::super::MixopError;
use super::{Mixop, MixopArena, MixopKind};

/// A failure parsing or interning a notation string.
#[derive(Debug, thiserror::Error)]
pub(crate) enum ParseMixopError {
    /// The string is not a valid notation type.
    #[error(transparent)]
    Syntax(#[from] FrontendError),
    /// The arena cannot store another notation node.
    #[error(transparent)]
    Intern(#[from] MixopError),
}

/// Converts a notation representation into an interned flat mixop.
pub(crate) trait ToMixop {
    /// The failures supported by this representation.
    type Error;

    /// Interns the notation into the supplied arena.
    fn to_mixop(&self, arena_mixop: &mut MixopArena) -> Result<Mixop, Self::Error>;
}

impl ToMixop for str {
    type Error = ParseMixopError;

    /// Reuses a notation handle from this arena or parses and interns it once.
    fn to_mixop(&self, arena_mixop: &mut MixopArena) -> Result<Mixop, Self::Error> {
        // Cached handles belong to the arena that owns this table
        if let Some(mixop) = arena_mixop.parsed.get(self) {
            return Ok(*mixop);
        }
        // Retain only successful conversions, after all children are interned
        let mixop = parse_typ(self)?.to_mixop(arena_mixop)?;
        arena_mixop.parsed.insert(self.to_owned(), mixop);
        Ok(mixop)
    }
}

impl ToMixop for Typ {
    type Error = MixopError;

    /// Extracts flat notation from EL, replacing each plain type with a hole.
    fn to_mixop(&self, arena_mixop: &mut MixopArena) -> Result<Mixop, Self::Error> {
        match self {
            Self::Plain(_) => arena_mixop.intern_kind(MixopKind::Arg),
            Self::Notation(not_typ_el) => not_typ_el.to_mixop(arena_mixop),
        }
    }
}

impl ToMixop for NotTyp {
    type Error = MixopError;

    /// Interns EL notation children first, retaining atom spans and structure.
    fn to_mixop(&self, arena_mixop: &mut MixopArena) -> Result<Mixop, Self::Error> {
        let kind = match &self.node {
            NotTypKind::Atom(atom) => MixopKind::Atom(atom.clone()),
            NotTypKind::Seq(typs_el) => MixopKind::Seq(
                typs_el
                    .iter()
                    .map(|typ_el| typ_el.to_mixop(arena_mixop))
                    .collect::<Result<_, _>>()?,
            ),
            NotTypKind::Infix(typ_l_el, atom, typ_r_el) => {
                let mixop_l = typ_l_el.to_mixop(arena_mixop)?;
                let mixop_r = typ_r_el.to_mixop(arena_mixop)?;
                MixopKind::Infix(mixop_l, atom.clone(), mixop_r)
            }
            NotTypKind::Brack(atom_l, typ_inner_el, atom_r) => {
                let mixop_inner = typ_inner_el.to_mixop(arena_mixop)?;
                MixopKind::Brack(atom_l.clone(), mixop_inner, atom_r.clone())
            }
        };
        arena_mixop.intern_kind(kind)
    }
}
