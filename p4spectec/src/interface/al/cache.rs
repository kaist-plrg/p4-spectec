//! Successful AL representation conversions within one arena
//!
//! Each host boundary owns its cache and passes it through recursive conversions.
//! Keys compare payloads without type notes or source locations;
//! a hit retains the annotations of the first converted value.
//! The two directions are independent and retain entries until cleared.
//! Each table stops admitting new results at a fixed entry limit.

use std::{collections::HashMap, hash::Hash};

use foldhash::fast::RandomState;

use crate::lang::data::{
    intern::CanonId,
    notation::flat::{Mixop, MixopKind},
    typ::Typ,
    value::flat::{Value, ValueKind},
};

const NUM_ENTRIES_MAX: usize = 4096;

/// Retains conversions up to a fixed limit when caching is enabled.
pub(super) struct Table<K, V> {
    enabled: bool,
    entries: HashMap<K, V, RandomState>,
}

impl<K, V> Table<K, V> {
    fn new(enabled: bool) -> Self {
        Self { enabled, entries: HashMap::default() }
    }

    fn clear(&mut self) {
        self.entries.clear();
    }
}

impl<K: Eq + Hash, V> Table<K, V> {
    /// Looks up a retained conversion when caching is enabled.
    pub(super) fn get(&self, key: &K) -> Option<&V> {
        if !self.enabled {
            return None;
        }
        self.entries.get(key)
    }

    /// Retains a conversion when enabled and below the entry limit.
    pub(super) fn insert(&mut self, key: K, value: V) {
        if !self.enabled || self.entries.len() >= NUM_ENTRIES_MAX {
            return;
        }
        self.entries.insert(key, value);
    }
}

/// Conversion results owned by one host boundary in one arena.
///
/// Clear before resetting or replacing the arena; canonical ids and stored
/// handles are only valid in the arena that produced them.
/// Full tables retain existing results and skip new insertions.
pub struct Cache {
    pub(super) values_encode: Table<CanonId<ValueKind>, Value>,
    pub(super) values_decode: Table<CanonId<ValueKind>, Value>,
    pub(super) mixops_encode: Table<CanonId<MixopKind>, Value>,
    pub(super) mixops_decode: Table<CanonId<ValueKind>, Mixop>,
    pub(super) typs_decode: Table<CanonId<ValueKind>, Typ>,
}

impl Cache {
    /// Creates empty tables with the requested caching mode.
    pub fn new(enabled: bool) -> Self {
        Self {
            values_encode: Table::new(enabled),
            values_decode: Table::new(enabled),
            mixops_encode: Table::new(enabled),
            mixops_decode: Table::new(enabled),
            typs_decode: Table::new(enabled),
        }
    }

    /// Drops all retained results while preserving the caching mode.
    pub fn clear(&mut self) {
        self.values_encode.clear();
        self.values_decode.clear();
        self.mixops_encode.clear();
        self.mixops_decode.clear();
        self.typs_decode.clear();
    }
}
