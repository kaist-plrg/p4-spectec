//! Cached notation parsing directly into flat mixops
//!
//! EL types supply argument positions and atoms without a tree mixop.
//! The parser caches EL types independently of runtime storage.
//! Each caller interns notation into its own arena and compares canonical ids.

use std::{cell::RefCell, collections::HashMap, rc::Rc};

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

thread_local! {
    /// Parsed EL types without arena-owned handles.
    static TYPES: RefCell<HashMap<String, Rc<Typ>>> = RefCell::new(HashMap::new());
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

    /// Reuses a parsed EL type and interns its notation into the supplied arena.
    fn to_mixop(&self, arena_mixop: &mut MixopArena) -> Result<Mixop, Self::Error> {
        // Cache successful parses without retaining runtime arena handles
        let typ_el = TYPES.with(|types| -> Result<Rc<Typ>, FrontendError> {
            if let Some(typ_el) = types.borrow().get(self) {
                return Ok(Rc::clone(typ_el));
            }
            let typ_el = Rc::new(parse_typ(self)?);
            types
                .borrow_mut()
                .insert(self.to_owned(), Rc::clone(&typ_el));
            Ok(typ_el)
        })?;
        // Intern into the caller's arena on both cache hits and misses
        Ok(typ_el.to_mixop(arena_mixop)?)
    }
}

/// Compares a runtime notation against a fixed spelling in the same arena.
pub(crate) fn matches_string(
    arena_mixop: &mut MixopArena,
    mixop: Mixop,
    text: &'static str,
) -> bool {
    let mixop_pattern = text
        .to_mixop(arena_mixop)
        .expect("value pattern contains a valid SpecTec mixop");
    arena_mixop.canon_id(mixop) == arena_mixop.canon_id(mixop_pattern)
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
