//! Cached notation parsing directly into flat mixops
//!
//! EL types supply argument positions and atoms without a tree mixop.
//! Runtime arenas own their parsed handles; read-only string matching uses
//! a separate pattern arena and compares structures across arenas.

use std::cell::RefCell;

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

/// Parses notation once per spelling and destination arena.
pub(crate) fn mixop_cached(
    arena_mixop: &mut MixopArena,
    text: &str,
) -> Result<Mixop, ParseMixopError> {
    // Reuse only handles owned by this destination
    if let Some(mixop) = arena_mixop.mixops_by_text.get(text) {
        return Ok(*mixop);
    }
    // Cache successful parses, including dynamically decoded spellings
    let typ_el = parse_typ(text)?;
    let mixop = typ_el.to_mixop(arena_mixop)?;
    arena_mixop.mixops_by_text.insert(text.to_owned(), mixop);
    Ok(mixop)
}

/// Compares a runtime notation against a cached fixed spelling without mutation.
pub(crate) fn matches_string(arena_mixop: &MixopArena, mixop: Mixop, text: &'static str) -> bool {
    thread_local! {
        static PATTERNS: RefCell<MixopArena> = RefCell::new(MixopArena::new());
    }
    PATTERNS.with(|patterns| {
        let mut arena_pattern = patterns.borrow_mut();
        let mixop_pattern = mixop_cached(&mut arena_pattern, text)
            .expect("value pattern contains a valid SpecTec mixop");
        mixop.syntax_eq_in(arena_mixop, mixop_pattern, &arena_pattern)
    })
}

impl Typ {
    /// Extracts flat notation from EL, replacing each plain type with a hole.
    pub(crate) fn to_mixop(&self, arena_mixop: &mut MixopArena) -> Result<Mixop, MixopError> {
        match self {
            Self::Plain(_) => arena_mixop.intern_kind(MixopKind::Arg),
            Self::Notation(not_typ_el) => not_typ_el.to_mixop(arena_mixop),
        }
    }
}

impl NotTyp {
    /// Interns EL notation children first, retaining atom spans and structure.
    pub(crate) fn to_mixop(&self, arena_mixop: &mut MixopArena) -> Result<Mixop, MixopError> {
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
