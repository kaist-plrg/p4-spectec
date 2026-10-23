//! Hashing flat values
//!
//! Hashes contents in variant order, using canonical child identities where stored.

use std::hash::{Hash, Hasher};

use crate::lang::data::{
    intern::{CanonHash, CanonLookup},
    notation::flat::MixopKind,
};

use super::{Value, ValueField, ValueKind};

impl<Deps: CanonLookup<MixopKind>> CanonHash<Deps> for ValueKind {
    fn canon_hash<H: Hasher>(
        &self,
        interner: &impl CanonLookup<Self>,
        arena_mixop: &Deps,
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

/// Exact hashes also serve borrowed constructor queries.
impl Hash for ValueKind {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.tag().hash(hasher);
        match self {
            Self::Bool(value) => value.hash(hasher),
            Self::Num(value) => value.hash(hasher),
            Self::Text(value) => value.hash(hasher),
            Self::Struct(fields) => {
                fields.len().hash(hasher);
                for field in fields {
                    field.atom.hash(hasher);
                    exact_value(&field.value, hasher);
                }
            }
            Self::Case(case) => {
                case.mixop().hash(hasher);
                exact_values(case.args(), hasher);
            }
            Self::Tuple(values) | Self::List(values) => exact_values(values, hasher),
            Self::Opt(value) => {
                value.is_some().hash(hasher);
                if let Some(value) = value {
                    exact_value(value, hasher);
                }
            }
            Self::Func(id) => id.hash(hasher),
            Self::Extern(value) => value.hash(hasher),
        }
    }
}

/// Hashes all three exact handles in two machine-word writes.
pub(crate) fn exact_value<H: Hasher>(value: &Value, hasher: &mut H) {
    hasher.write_u64(u64::from(value.node.index()) | (u64::from(value.note.index()) << 32));
    value.span.hash(hasher);
}

pub(crate) fn exact_values<H: Hasher>(values: &[Value], hasher: &mut H) {
    values.len().hash(hasher);
    for value in values {
        exact_value(value, hasher);
    }
}
