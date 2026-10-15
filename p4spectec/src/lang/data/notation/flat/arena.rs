//! Append-only storage for notation mixops
//!
//! A mixop handle is valid only in the arena that issued it.
//! `intern` stores a tree node by node, children first,
//! and records each new mixop's number of argument positions.

use std::collections::HashMap;

use foldhash::fast::RandomState;

use crate::frontend::parse::parse_typ;
use crate::lang::data::intern::{CanonId, CanonInterner};

use super::super::{MixopError, tree};
use super::parse::{ParseMixopError, ToMixop};
use super::{Mixop, MixopKind};

// = Arena storage

/// Storage for notation mixops, shared by every value built from them.
#[derive(Debug, Default)]
pub struct MixopArena {
    /// Mixops, with canonical identities that ignore atom spans.
    mixops: CanonInterner<MixopKind>,
    /// Argument positions of each mixop, by handle index.
    arities: Vec<u32>,
    /// Successfully parsed notation strings, valid only in this arena.
    mixops_parsed: HashMap<String, Mixop, RandomState>,
}

impl MixopArena {
    // - Construction

    /// An empty arena.
    pub fn new() -> Self {
        Self::default()
    }

    // - Interning

    /// Parses and interns notation, reusing this arena's cached handle.
    pub(crate) fn parse(&mut self, text: &str) -> Result<Mixop, ParseMixopError> {
        // Reuse this arena's cached handle
        if let Some(mixop) = self.mixops_parsed.get(text) {
            return Ok(*mixop);
        }

        // Cache only successful conversions
        let mixop = parse_typ(text)?.to_mixop(self)?;
        self.mixops_parsed.insert(text.to_owned(), mixop);
        Ok(mixop)
    }

    /// Interns a tree node by node, children first, copying atoms.
    pub fn intern(&mut self, mixop: &tree::Mixop) -> Result<Mixop, MixopError> {
        mixop.to_flat(self)
    }

    /// Interns a node whose children already belong to this arena.
    pub(crate) fn intern_kind(&mut self, kind: MixopKind) -> Result<Mixop, MixopError> {
        // Child canonical identities are available when the parent is hashed
        let mixop = self.mixops.intern(&(), kind)?;
        // A new mixop sums its children's positions, which are recorded
        if mixop.index() as usize == self.arities.len() {
            let arity = self.kind(mixop).arity(self);
            self.arities.push(u32::try_from(arity)?);
        }
        Ok(mixop)
    }

    // - Lookup

    /// The node behind a mixop handle.
    pub fn kind(&self, mixop: Mixop) -> &MixopKind {
        self.mixops.get(mixop)
    }

    /// The number of argument positions of a mixop.
    pub fn arity(&self, mixop: Mixop) -> usize {
        self.arities[mixop.index() as usize] as usize
    }

    /// The canonical identity of a mixop, ignoring atom spans.
    pub fn canon_id(&self, mixop: Mixop) -> CanonId<MixopKind> {
        self.mixops.canon_id(mixop)
    }
}
