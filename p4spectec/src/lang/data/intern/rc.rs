//! Exact interning of shared allocations
//!
//! Equal contents share one retained Rc even when allocated separately.
//! `Interner` supplies exact comparison and stable indices;
//! the public handle names the pointee rather than the owning Rc.

use std::{hash::Hash, marker::PhantomData, num::TryFromIntError, rc::Rc};

use super::{idx::Interned, simple::Interner};

/// Shares exactly equal Rc contents, retaining one allocation per entry.
#[derive(Debug)]
pub struct RcInterner<T> {
    storage: Interner<Rc<T>>,
}

impl<T> Default for RcInterner<T> {
    fn default() -> Self {
        Self { storage: Interner::new() }
    }
}

impl<T> RcInterner<T> {
    /// Creates an empty interner.
    pub fn new() -> Self {
        Self::default()
    }

    /// Borrows the shared allocation behind a handle.
    pub fn get(&self, id: Interned<T>) -> &Rc<T> {
        self.storage
            .get(Interned { index: id.index, marker: PhantomData })
    }
}

impl<T: Eq + Hash> RcInterner<T> {
    /// Reuses equal contents or retains the supplied allocation at a new index.
    pub fn intern(&mut self, item: Rc<T>) -> Result<Interned<T>, TryFromIntError> {
        let id = self.storage.intern(item)?;
        Ok(Interned { index: id.index, marker: PhantomData })
    }
}
