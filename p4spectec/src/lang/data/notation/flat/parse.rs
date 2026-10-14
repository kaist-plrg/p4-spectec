//! Cached notation parsing directly into flat mixops
//!
//! EL types supply argument positions and atoms without a tree mixop.
//! Each arena retains successful string conversions with its own handles.
//! Atom matrices recover notation structure through the same parser.

use crate::frontend::{error::FrontendError, parse::parse_typ};
use crate::lang::{
    el::ast::{NotTyp, NotTypKind, Typ},
    traits::print::Print,
};

use super::super::{AtomPhrase, MixopError};
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

/// A failure converting an atom matrix into a flat mixop.
#[derive(Debug, thiserror::Error)]
pub(crate) enum MatrixMixopError {
    /// An atom matrix must contain at least one boundary row.
    #[error("empty atom matrix")]
    EmptyMatrix,
    /// The reconstructed notation has a different number of argument positions.
    #[error("atom matrix and mixop arity disagree")]
    ArityMismatch,
    /// Parsing or interning the reconstructed notation failed.
    #[error(transparent)]
    Parse(#[from] ParseMixopError),
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

impl ToMixop for [Vec<AtomPhrase>] {
    type Error = MatrixMixopError;

    /// Reconstructs notation from atoms between argument positions and interns it.
    fn to_mixop(&self, arena_mixop: &mut MixopArena) -> Result<Mixop, Self::Error> {
        // Even zero-argument notation needs one boundary row
        if self.is_empty() {
            return Err(MatrixMixopError::EmptyMatrix);
        }

        // [[]] represents empty notation, which has no source text to parse
        if self.len() == 1 && self[0].is_empty() {
            return arena_mixop
                .intern_kind(MixopKind::Seq(vec![]))
                .map_err(ParseMixopError::from)
                .map_err(MatrixMixopError::from);
        }

        // Insert a placeholder type at each hole: [[TEXT], []] becomes TEXT nat
        let text_mixop = self
            .iter()
            .map(|atoms| {
                atoms
                    .iter()
                    .map(Print::to_string)
                    .collect::<Vec<_>>()
                    .join(" ")
            })
            .collect::<Vec<_>>()
            .join(" nat ");

        // Recover Seq/Infix/Brack structure through the grammar and intern it
        let mixop = text_mixop.to_mixop(arena_mixop)?;

        // Parsing must preserve exactly one hole between adjacent rows
        if arena_mixop.arity(mixop) + 1 != self.len() {
            return Err(MatrixMixopError::ArityMismatch);
        }
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
