//! Exact storage with a second, canonical identity
//!
//! First, exactly equal items share a stored entry.
//! Then canonical comparison groups entries by meaning:
//! ("x", span_a) and ("x", span_b) keep distinct handles
//! but share a canonical ID when that comparison ignores spans.
//! `intern` also takes the interners an item depends on (`deps`),
//! so an item can read identities held by another interner,
//! such as a value reading its notation's.

use std::{
    fmt,
    hash::{BuildHasher, Hash, Hasher},
    num::TryFromIntError,
};

use foldhash::fast::RandomState;
use hashbrown::HashTable;

use super::{idx::Interned, simple::Interner};

// = Canonical comparison

/// Compares canonical meaning using child identities and the interners
/// an item depends on.
pub trait CanonEq<Deps: ?Sized = ()>: Sized {
    /// Whether two items mean the same.
    ///
    /// Children in this interner compare by canonical id;
    /// identities held by other interners are read through `deps`.
    fn canon_eq(&self, interner: &CanonInterner<Self>, deps: &Deps, other: &Self) -> bool;
}

/// Hashes canonical meaning; canonically equal items must have equal hashes.
pub trait CanonHash<Deps: ?Sized = ()>: Sized {
    /// Hashes the meaning.
    ///
    /// Children in this interner hash by canonical id;
    /// identities held by other interners are read through `deps`.
    fn canon_hash<H: Hasher>(&self, interner: &CanonInterner<Self>, deps: &Deps, hasher: &mut H);
}

// = Canonical identities

/// A canonical identity valid only in the interner that issued it.
#[repr(transparent)]
pub struct CanonId<T>(Interned<T>);

// - Copying

impl<T> Copy for CanonId<T> {}

impl<T> Clone for CanonId<T> {
    fn clone(&self) -> Self {
        *self
    }
}

// - Printing

impl<T> fmt::Debug for CanonId<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_tuple("CanonId").field(&self.0.index).finish()
    }
}

// - Equality

impl<T> PartialEq for CanonId<T> {
    fn eq(&self, id_other: &Self) -> bool {
        self.0 == id_other.0
    }
}

impl<T> Eq for CanonId<T> {}

// - Hashing

impl<T> Hash for CanonId<T> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.0.hash(hasher);
    }
}

// = Canonical interning

/// A canonical class: its hash and the first stored item that has it.
#[derive(Debug)]
struct CanonEntry<T> {
    hash: u64,
    representative: Interned<T>,
}

/// Preserves exact items while sharing identities under a coarser equality.
#[derive(Debug)]
pub struct CanonInterner<T> {
    /// Exact storage.
    storage: Interner<T>,
    /// Canonical id of each stored item, by handle index.
    canon: Vec<CanonId<T>>,
    /// Canonical classes by canonical hash.
    canon_table: HashTable<CanonEntry<T>>,
    /// Hasher for canonical hashes.
    canon_hasher: RandomState,
}

// - Construction

impl<T> Default for CanonInterner<T> {
    fn default() -> Self {
        Self {
            storage: Interner::new(),
            canon: Vec::new(),
            canon_table: HashTable::new(),
            canon_hasher: RandomState::default(),
        }
    }
}

impl<T> CanonInterner<T> {
    /// An empty interner.
    pub fn new() -> Self {
        Self::default()
    }

    // - Lookup

    /// The item behind a handle.
    pub fn get(&self, id: Interned<T>) -> &T {
        self.storage.get(id)
    }

    /// The canonical identity of a stored item.
    pub fn canon_id(&self, id: Interned<T>) -> CanonId<T> {
        self.canon[id.index as usize]
    }
}

// - Interning

impl<T: Eq + Hash> CanonInterner<T> {
    /// Interns exactly, then assigns the canonical identity.
    ///
    /// Exact equality must imply canonical equality;
    /// referenced children must already have canonical identities here.
    /// Every call on one interner must pass `deps`
    /// that keep the identities read through them unchanged.
    pub fn intern<Deps: ?Sized>(
        &mut self,
        deps: &Deps,
        item: T,
    ) -> Result<Interned<T>, TryFromIntError>
    where
        T: CanonEq<Deps> + CanonHash<Deps>,
    {
        // An exact duplicate already has its canonical id
        let id = self.storage.intern(item)?;
        if (id.index as usize) < self.canon.len() {
            return Ok(id);
        }
        // Look for an existing class with the same meaning
        let item = self.storage.get(id);
        let mut hasher = self.canon_hasher.build_hasher();
        item.canon_hash(self, deps, &mut hasher);
        let hash = hasher.finish();
        let id_canon = self
            .canon_table
            .find(hash, |entry| {
                entry.hash == hash && item.canon_eq(self, deps, self.get(entry.representative))
            })
            .map(|entry| CanonId(entry.representative));
        // Join the class found, or found a new one represented by this item
        self.canon.push(id_canon.unwrap_or(CanonId(id)));
        if id_canon.is_none() {
            self.canon_table.insert_unique(
                hash,
                CanonEntry { hash, representative: id },
                |entry| entry.hash,
            );
        }
        Ok(id)
    }
}
