//! Hashing flat values
//!
//! Hashes contents in variant order, using canonical child identities where stored.

use std::hash::{Hash, Hasher};

use crate::lang::data::{
    intern::{CanonHash, CanonInterner},
    notation::MixopArena,
};

use super::{ValueField, ValueKind};

impl CanonHash<MixopArena> for ValueKind {
    fn canon_hash<H: Hasher>(
        &self,
        interner: &CanonInterner<Self>,
        arena_mixop: &MixopArena,
        hasher: &mut H,
    ) {
        std::mem::discriminant(self).hash(hasher);
        match self {
            ValueKind::Bool(value) => value.hash(hasher),
            ValueKind::Num(value) => value.hash(hasher),
            ValueKind::Text(value) => value.hash(hasher),
            ValueKind::Struct(value_fields) => {
                value_fields.len().hash(hasher);
                for ValueField { atom, value } in value_fields {
                    atom.node.hash(hasher);
                    interner.canon_id(value.node).hash(hasher);
                }
            }
            ValueKind::Case(value_case) => {
                arena_mixop.canon_id(*value_case.mixop()).hash(hasher);
                value_case.args().len().hash(hasher);
                for value in value_case.args() {
                    interner.canon_id(value.node).hash(hasher);
                }
            }
            ValueKind::Tuple(values) | ValueKind::List(values) => {
                values.len().hash(hasher);
                for value in values {
                    interner.canon_id(value.node).hash(hasher);
                }
            }
            ValueKind::Opt(value) => value
                .map(|value| interner.canon_id(value.node))
                .hash(hasher),
            ValueKind::Func(id) => id.node.hash(hasher),
            ValueKind::Extern(json) => json.hash(hasher),
        }
    }
}
