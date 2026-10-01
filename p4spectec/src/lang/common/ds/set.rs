//! Sets that compare syntax keys without source or analysis metadata
//!
//! `PhraseSet` is a persistent ordered set over `ByKey`;
//! inserting an equivalent key keeps the first one stored.
//! `IdSet` is the set of identifiers the `FreeIds` traversals collect.

use imbl::{GenericOrdSet, shared_ptr::RcK};

use crate::lang::{common::Id, traits::cmp::SyntaxCmp};

use super::collections::ByKey;

/// The persistent set, shared through `Rc`.
type PersistentOrdSet<K> = GenericOrdSet<K, RcK>;

/// An ordered set that compares keys through `SyntaxCmp`
#[repr(transparent)]
#[derive(Clone, Debug)]
pub struct PhraseSet<K: SyntaxCmp> {
    /// Keys ordered by syntax.
    entries: PersistentOrdSet<ByKey<K>>,
}

impl<K: SyntaxCmp> PhraseSet<K> {
    /// Constructs an empty set.
    pub fn new() -> Self {
        Self { entries: PersistentOrdSet::new() }
    }

    /// Returns whether the set contains no keys.
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Returns the number of keys.
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Inserts a key; an equivalent stored key is kept instead.
    pub fn insert(&mut self, key: K) -> bool
    where
        K: Clone,
    {
        let key = ByKey(key);
        // The stored key wins over an equivalent newcomer
        if self.entries.contains(&key) { false } else { self.entries.insert(key).is_none() }
    }

    /// Moves every key from `set_other` into this set.
    pub fn append(&mut self, set_other: Self)
    where
        K: Clone,
    {
        // Only keys not already present move over, keeping ours
        let entries_other = set_other.entries.relative_complement(self.entries.clone());
        self.entries = self.entries.clone().union(entries_other);
    }

    /// Returns the union with `set_other`
    pub fn union(mut self, set_other: Self) -> Self
    where
        K: Clone,
    {
        self.append(set_other);
        self
    }

    /// Returns the intersection with `set_other`
    pub fn intersection(&self, set_other: &Self) -> Self
    where
        K: Clone,
    {
        let entries = set_other.entries.clone().intersection(self.entries.clone());
        Self { entries }
    }

    /// Returns the difference from `set_other`
    pub fn difference(&self, set_other: &Self) -> Self
    where
        K: Clone,
    {
        let entries = self
            .entries
            .clone()
            .relative_complement(set_other.entries.clone());
        Self { entries }
    }

    /// Iterates over stored keys in collection order.
    pub fn iter(&self) -> impl Iterator<Item = &K> {
        self.entries.iter().map(|key| &key.0)
    }
}

impl PhraseSet<Id> {
    /// Returns whether an equivalent key is present.
    pub fn contains(&self, key: &Id) -> bool {
        self.entries.contains(&key.node)
    }

    /// Removes and returns the stored key equivalent to `key`
    pub fn take(&mut self, key: &Id) -> Option<Id> {
        self.entries.remove(&key.node).map(|key| key.0)
    }
}

impl<K: SyntaxCmp> Default for PhraseSet<K> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K: SyntaxCmp> PartialEq for PhraseSet<K> {
    fn eq(&self, set_other: &Self) -> bool {
        self.entries == set_other.entries
    }
}

impl<K: SyntaxCmp> Eq for PhraseSet<K> {}

impl<K: SyntaxCmp + Clone> Extend<K> for PhraseSet<K> {
    fn extend<T: IntoIterator<Item = K>>(&mut self, keys: T) {
        for key in keys {
            self.insert(key);
        }
    }
}

impl<K: SyntaxCmp + Clone> FromIterator<K> for PhraseSet<K> {
    fn from_iter<T: IntoIterator<Item = K>>(keys: T) -> Self {
        let mut set = Self::new();
        set.extend(keys);
        set
    }
}

impl<K: SyntaxCmp + Clone, const N: usize> From<[K; N]> for PhraseSet<K> {
    fn from(keys: [K; N]) -> Self {
        keys.into_iter().collect()
    }
}

/// Set of source-annotated identifiers.
pub type IdSet = PhraseSet<Id>;

/// Set of source-annotated type identifiers.
pub type TIdSet = IdSet;
