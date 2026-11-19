//! Typed handles and exact or canonical interning storage
//!
//! `Interned<T>` is a `u32` handle valid in the interner that issued it.
//! `Interner` shares exactly equal items;
//! `CanonInterner` stores exactly and adds a coarser canonical identity.

// = Implementations

mod canon;
mod idx;
mod simple;

// = Public interface

pub use canon::{CanonEq, CanonHash, CanonId, CanonInterner};
pub use idx::Interned;
pub use simple::Interner;
