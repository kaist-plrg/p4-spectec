//! Bounded caching with clock replacement
//!
//! Hits and insertions mark an entry as referenced.
//! When full, the clock hand clears reference bits until it reaches
//! an unreferenced entry, then replaces that entry and advances.
//! Slots stay in place so the key-to-slot map remains valid.
//! A zero-capacity cache stores nothing and always misses.

use std::{collections::HashMap, hash::Hash};

use foldhash::fast::RandomState;

/// A retained value and its second-chance reference bit.
struct Entry<K, V> {
    key: K,
    value: V,
    referenced: bool,
}

/// Stores up to a fixed number of entries using clock replacement.
pub struct ClockCache<K, V> {
    capacity: usize,
    idxs: HashMap<K, usize, RandomState>,
    entries: Vec<Entry<K, V>>,
    idx_clock: usize,
}

impl<K, V> ClockCache<K, V> {
    /// Creates an empty cache; a zero capacity disables storage.
    pub fn new(capacity: usize) -> Self {
        Self { capacity, idxs: HashMap::default(), entries: Vec::new(), idx_clock: 0 }
    }

    /// Drops retained entries and resets the clock, preserving capacity.
    pub fn clear(&mut self) {
        self.idxs.clear();
        self.entries.clear();
        self.idx_clock = 0;
    }
}

impl<K: Copy + Eq + Hash, V> ClockCache<K, V> {
    /// Looks up a value and gives its entry a second chance.
    pub fn get(&mut self, key: &K) -> Option<&V> {
        let idx = *self.idxs.get(key)?;
        let entry = &mut self.entries[idx];
        entry.referenced = true;
        Some(&entry.value)
    }

    /// Inserts or updates a value, replacing a clock-selected entry when full.
    pub fn insert(&mut self, key: K, value: V) {
        // Leave disabled caches empty
        if self.capacity == 0 {
            return;
        }
        // Update existing keys without moving the clock hand
        if let Some(&idx) = self.idxs.get(&key) {
            let entry = &mut self.entries[idx];
            entry.value = value;
            entry.referenced = true;
            return;
        }
        // Fill unused slots before selecting an eviction candidate
        let entry = Entry { key, value, referenced: true };
        if self.entries.len() < self.capacity {
            let idx = self.entries.len();
            self.entries.push(entry);
            self.idxs.insert(key, idx);
            return;
        }
        // Give referenced entries a second chance during the sweep
        while self.entries[self.idx_clock].referenced {
            self.entries[self.idx_clock].referenced = false;
            self.idx_clock = (self.idx_clock + 1) % self.entries.len();
        }
        // Replace the selected slot and continue from its successor next time
        let idx = self.idx_clock;
        self.idxs.remove(&self.entries[idx].key);
        self.entries[idx] = entry;
        self.idxs.insert(key, idx);
        self.idx_clock = (idx + 1) % self.entries.len();
    }
}
