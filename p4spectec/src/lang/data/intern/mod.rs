//! Typed handles and structural, physical, or canonical interning storage
//!
//! `Interned<T>` is a `u32` handle valid in the interner that issued it.
//! `Interner` shares exactly equal items, `RcInterner` shares equal Rc contents,
//! `CanonInterner` stores exactly and adds a coarser canonical identity.

// = Implementations

mod canon;
mod idx;
mod rc;
mod simple;

// = Public interface

pub use canon::{CanonEq, CanonHash, CanonId, CanonInterner};
pub use idx::Interned;
pub use rc::RcInterner;
pub use simple::Interner;
