//! Data shared by the language representations
//!
//! Identifiers, iteration markers, source spans, primitive values,
//! mixfix notation, and the syntax-keyed collections built on them.

pub mod ds;
pub mod ids;
pub mod iter;
pub mod notation;
pub mod prim;
pub mod source;

pub use ids::{id::Id, tid::TId};
pub use iter::Iter;
