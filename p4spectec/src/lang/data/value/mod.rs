//! Values of IL and later stages, as arena handles and as trees
//!
//! `flat::Value` holds body, type, and span handles into one `Arena`.
//! `tree::Value` owns those contents and annotations.
//! Each representation defines its own `Value` and `ValueKind`.
//! `flat::ValueCase` pairs a mixop handle with its arguments;
//! `tree::ValueCase` owns its filled notation.
//! `flat::make` allocates values of each kind with their type,
//! `flat::get` projects a kind back out or fails with `ValueError`.

mod error;
pub mod flat;
pub mod tree;

pub use error::ValueError;
pub use flat::ValueArena;

// = Value tags

/// The kind of a value without its payload, for errors and ordering.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ValueTag {
    Bool,
    Num,
    Text,
    Struct,
    Case,
    Tuple,
    Opt,
    List,
    Func,
    Extern,
}
