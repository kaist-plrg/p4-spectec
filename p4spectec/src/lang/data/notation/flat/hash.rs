//! Hashing flat notation
//!
//! Hashes contents in variant order, using canonical child identities where stored.

use std::hash::{Hash, Hasher};

use crate::lang::data::intern::{CanonHash, CanonInterner};

use super::MixopKind;

impl CanonHash for MixopKind {
    fn canon_hash<H: Hasher>(&self, interner: &CanonInterner<Self>, _: &(), hasher: &mut H) {
        self.tag().hash(hasher);
        match self {
            Self::Arg => {}
            Self::Atom(atom) => atom.node.hash(hasher),
            Self::Brack(atom_l, mixop, atom_r) => {
                atom_l.node.hash(hasher);
                interner.canon_id(*mixop).hash(hasher);
                atom_r.node.hash(hasher);
            }
            Self::Infix(mixop_l, atom, mixop_r) => {
                interner.canon_id(*mixop_l).hash(hasher);
                atom.node.hash(hasher);
                interner.canon_id(*mixop_r).hash(hasher);
            }
            Self::Seq(mixops) => {
                mixops.len().hash(hasher);
                for mixop in mixops {
                    interner.canon_id(*mixop).hash(hasher);
                }
            }
        }
    }
}
