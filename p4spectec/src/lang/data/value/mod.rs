//! Values of IL and later stages, as arena handles
//!
//! `flat::Value` holds body, type, and span handles into one `Arena`.
//! `flat::ValueCase` pairs a mixop handle with its arguments.
//! `flat::make` allocates values of each kind with their type,
//! `flat::get` projects a kind back out or fails with `ValueError`.
//! Extern values carry a host's Rust data, described in `host`.

mod error;
pub mod flat;
pub mod host;

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
