//! Exact value storage with canonical classes computed only when compared.

use super::ValueKind;
use crate::lang::data::{
    intern::{CanonEq, CanonHash, CanonId, CanonLookup, Interned, Interner},
    notation::{
        MixopArena,
        flat::{Mixop, MixopKind},
    },
};
use foldhash::fast::RandomState;
use hashbrown::{HashMap, HashTable};
use std::{
    cell::{Cell, RefCell},
    hash::{BuildHasher, Hasher},
    num::TryFromIntError,
};

#[derive(Debug, Default)]
struct MixopIds(HashMap<Mixop, CanonId<MixopKind>, RandomState>);
impl CanonLookup<MixopKind> for MixopIds {
    fn canon_id(&self, id: Mixop) -> CanonId<MixopKind> {
        self.0[&id]
    }
}
#[derive(Debug)]
struct Entry {
    hash: u64,
    representative: Interned<ValueKind>,
}
#[derive(Debug, Default)]
pub(super) struct Values {
    storage: Interner<ValueKind>,
    canon: Vec<Cell<Option<CanonId<ValueKind>>>>,
    table: RefCell<HashTable<Entry>>,
    hasher: RandomState,
    mixops: MixopIds,
}
impl Values {
    #[inline]
    pub(super) fn get(&self, id: Interned<ValueKind>) -> &ValueKind {
        self.storage.get(id)
    }
    pub(super) fn intern(
        &mut self,
        arena_mixop: &MixopArena,
        kind: ValueKind,
    ) -> Result<Interned<ValueKind>, TryFromIntError> {
        let id = self.storage.intern(kind)?;
        self.register(arena_mixop, id);
        Ok(id)
    }
    fn register(&mut self, arena_mixop: &MixopArena, id: Interned<ValueKind>) {
        if id.index() as usize == self.canon.len() {
            if let ValueKind::Case(value_case) = self.storage.get(id) {
                let mixop = *value_case.mixop();
                self.mixops
                    .0
                    .entry(mixop)
                    .or_insert_with(|| arena_mixop.canon_id(mixop));
            }
            self.canon.push(Cell::new(None));
        }
    }
    pub(super) fn composite(
        &mut self,
        arena_mixop: &MixopArena,
        shape: super::projection::Shape,
        values: &[super::Value],
    ) -> Result<Interned<ValueKind>, TryFromIntError> {
        use super::projection::Shape;
        let query = super::composite::Composite { shape, values };
        let id = self.storage.intern_with(&query, || match shape {
            Shape::Tuple => ValueKind::Tuple(values.to_vec()),
            Shape::List => ValueKind::List(values.to_vec()),
            Shape::Case(mixop) => ValueKind::Case(
                super::ValueCase::new(arena_mixop, mixop, values.to_vec())
                    .expect("prepared constructor arity"),
            ),
        })?;
        self.register(arena_mixop, id);
        Ok(id)
    }
    fn compute(&self, id: Interned<ValueKind>) {
        let value = self.storage.get(id);
        let mut hasher = self.hasher.build_hasher();
        value.canon_hash(self, &self.mixops, &mut hasher);
        let hash = hasher.finish();
        let found = self
            .table
            .borrow()
            .find(hash, |entry| {
                entry.hash == hash
                    && value.canon_eq(self, &self.mixops, self.storage.get(entry.representative))
            })
            .map(|entry| entry.representative);
        let representative = found.unwrap_or(id);
        if found.is_none() {
            self.table
                .borrow_mut()
                .insert_unique(hash, Entry { hash, representative }, |entry| entry.hash);
        }
        self.canon[id.index() as usize].set(Some(CanonId::from_representative(representative)));
    }
}
impl CanonLookup<ValueKind> for Values {
    #[inline]
    fn canon_id(&self, id: Interned<ValueKind>) -> CanonId<ValueKind> {
        if let Some(id) = self.canon[id.index() as usize].get() {
            return id;
        }
        self.ensure(id);
        self.canon[id.index() as usize]
            .get()
            .expect("canonical class was computed")
    }
}
impl Values {
    /// Visits children before parents without recursing through deep list trees.
    fn ensure(&self, id: Interned<ValueKind>) {
        let mut pending = vec![(id, false)];
        while let Some((id, ready)) = pending.pop() {
            if self.canon[id.index() as usize].get().is_some() {
                continue;
            }
            if ready {
                self.compute(id);
                continue;
            }
            pending.push((id, true));
            let mut child = |value: &super::Value| {
                if self.canon[value.node.index() as usize].get().is_none() {
                    pending.push((value.node, false));
                }
            };
            match self.storage.get(id) {
                ValueKind::Tuple(values) | ValueKind::List(values) => {
                    for value in values {
                        child(value);
                    }
                }
                ValueKind::Case(value_case) => {
                    for value in value_case.args() {
                        child(value);
                    }
                }
                ValueKind::Struct(fields) => {
                    for field in fields {
                        child(&field.value);
                    }
                }
                ValueKind::Opt(Some(value)) => child(value),
                _ => {}
            }
        }
    }
}
