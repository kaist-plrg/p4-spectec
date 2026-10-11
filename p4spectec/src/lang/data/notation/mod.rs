//! Notation of IL and later stages, as trees and interned nodes
//!
//! `tree::Mixop` owns an argument-free notation such as `% |- % : %`.
//! `flat::Mixop` refers to a `flat::MixopKind` in a `MixopArena`.
//! `Mixfix` pairs either form with its arguments in notation order.
//! Each representation owns its comparisons and traversals;
//! `print` renders their atoms and argument positions.
//! EL uses only atoms, which stay in `common::notation`.

use crate::lang::common::{notation::atom::Atom, source::Phrase};

mod error;
pub mod flat;
mod free;
mod get;
mod map;
mod print;
pub mod tree;
pub(crate) mod visit;

pub use error::{ArityMismatch, MixopError};
pub use flat::MixopArena;

// = Notation tags

/// The kind of a mixop without its payload, for ordering and hashing.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MixopTag {
    Arg,
    Atom,
    Brack,
    Infix,
    Seq,
}

// = Notation forms

/// An atom with its source location.
pub type AtomPhrase = Phrase<Atom>;

/// A mixop with one argument per position, in notation order.
///
/// `% |- % : %` with arguments `[C, e, t]` represents `C |- e : t`.
/// Private fields keep the argument count equal to the mixop's arity.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Mixfix<M, T> {
    /// The form, with argument positions.
    mixop: M,
    /// Arguments in notation order, one per position.
    args: Vec<T>,
}
