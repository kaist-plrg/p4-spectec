//! Interning by Rc allocation identity
//!
//! An Rc and its clone share a handle;
//! a fresh Rc with equal contents gets a distinct handle.
//! Retaining each allocation prevents its address from being reused
//! while the interner is alive.

use std::{
    collections::{HashMap, hash_map::Entry},
    marker::PhantomData,
    num::TryFromIntError,
    rc::Rc,
};

use foldhash::fast::RandomState;

use super::idx::Interned;

// = Interning storage

/// Shares Rc allocations by address, retaining them until the interner drops.
#[derive(Debug)]
pub struct RcInterner<T> {
    /// Allocations by handle index.
    items: Vec<Rc<T>>,
    /// Handle of each allocation address.
    table: HashMap<*const T, Interned<T>, RandomState>,
}

// - Construction

impl<T> Default for RcInterner<T> {
    fn default() -> Self {
        Self { items: Vec::new(), table: HashMap::default() }
    }
}

impl<T> RcInterner<T> {
    /// An empty interner.
    pub fn new() -> Self {
        Self::default()
    }

    // - Lookup

    /// The allocation behind a handle.
    pub fn get(&self, id: Interned<T>) -> &Rc<T> {
        &self.items[id.index as usize]
    }

    // - Interning

    /// Reuses an allocation without hashing or comparing its contents.
    pub fn intern(&mut self, item: Rc<T>) -> Result<Interned<T>, TryFromIntError> {
        // Seen this allocation: same handle
        match self.table.entry(Rc::as_ptr(&item)) {
            Entry::Occupied(entry) => Ok(*entry.get()),
            // New allocation: next index, retaining the Rc
            Entry::Vacant(entry) => {
                let index = u32::try_from(self.items.len())?;
                let id = Interned { index, marker: PhantomData };
                self.items.push(item);
                entry.insert(id);
                Ok(id)
            }
        }
    }
}
