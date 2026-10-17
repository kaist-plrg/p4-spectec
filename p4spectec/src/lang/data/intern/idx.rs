//! Typed interner handles
//!
//! A handle is an index into one interner's storage;
//! the phantom type keeps handles of different item types apart
//! without depending on any trait of the item type.

use std::{
    fmt,
    hash::{Hash, Hasher},
    marker::PhantomData,
    num::{NonZeroU32, TryFromIntError},
};

// = Typed handles

/// A compact handle valid only in the interner that issued it.
#[repr(transparent)]
pub struct Interned<T> {
    /// One-based position; zero is reserved for absent handles.
    index: NonZeroU32,
    /// The item type, without borrowing or owning one.
    pub(super) marker: PhantomData<fn() -> T>,
}

// - Copying

// Handle operations depend only on the index, not on traits of T
impl<T> Copy for Interned<T> {}

impl<T> Clone for Interned<T> {
    fn clone(&self) -> Self {
        *self
    }
}

// - Printing

impl<T> fmt::Debug for Interned<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_tuple("Interned").field(&self.index()).finish()
    }
}

// - Equality

impl<T> PartialEq for Interned<T> {
    fn eq(&self, id_other: &Self) -> bool {
        self.index == id_other.index
    }
}

impl<T> Eq for Interned<T> {}

// - Hashing

impl<T> Hash for Interned<T> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.index().hash(hasher);
    }
}

impl<T> Interned<T> {
    /// The raw index, for arena-relative encoding.
    pub(crate) fn index(self) -> u32 {
        self.index.get() - 1
    }

    /// Validates a storage length before assigning its next handle.
    pub(super) fn from_len(len: usize) -> Result<Self, TryFromIntError> {
        let one_based = u32::try_from((len as u128) + 1)?;
        Ok(Self {
            index: NonZeroU32::new(one_based).expect("one-based index"),
            marker: PhantomData,
        })
    }

    /// Rejects the reserved index in external arena-relative data.
    pub(crate) fn try_from_index(index: u32) -> Result<Self, TryFromIntError> {
        Self::from_len(index as usize)
    }

    /// A handle from a raw index; the caller vouches for its interner.
    pub(crate) fn from_index(index: u32) -> Self {
        Self {
            index: NonZeroU32::new(index.checked_add(1).expect("interner handle limit"))
                .expect("one-based index"),
            marker: PhantomData,
        }
    }
}
