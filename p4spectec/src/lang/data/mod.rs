//! Data shared by the internal language and its successors
//!
//! IL, AL, SL, and PL share one type language, one value representation,
//! and one notion of variable; EL has its own.
//! `intern` holds the handle-based storage these representations build on,
//! and `notation` the mixfix forms of notation types, expressions, and values.
//! `arena` stores a specification's notation shapes beside a run's values.
//! `frame` maps a prepared unit's names to slots and holds its values,
//! and `callable` pairs prepared syntax with the layout of its frames.

pub mod arena;
pub mod callable;
pub mod encoding;
pub mod frame;
pub mod intern;
pub mod notation;
pub mod typ;
pub mod value;
pub mod var;
