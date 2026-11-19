//! Hashing tree notation
//!
//! Hashes contents in variant order, using canonical child identities where stored.

use std::hash::{Hash, Hasher};

use super::Mixop;

impl Hash for Mixop {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        // Hash the form first so different variants rarely collide
        self.tag().hash(hasher);
        match self {
            Self::Arg => {}
            Self::Atom(atom) => atom.node.hash(hasher),
            Self::Brack(atom_l, mixop, atom_r) => {
                atom_l.node.hash(hasher);
                mixop.hash(hasher);
                atom_r.node.hash(hasher);
            }
            Self::Infix(mixop_l, atom, mixop_r) => {
                mixop_l.hash(hasher);
                atom.node.hash(hasher);
                mixop_r.hash(hasher);
            }
            Self::Seq(mixops) => mixops.hash(hasher),
        }
    }
}
