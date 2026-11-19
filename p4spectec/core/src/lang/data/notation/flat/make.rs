//! Constructors for flat mixfix forms
//!
//! `new` checks the argument count; `fill` supplies each position.

use std::cmp::Ordering;

use super::super::ArityMismatch;
use super::{Mixfix, Mixop, MixopArena};

// - General

impl<T> Mixfix<T> {
    /// Pairs a mixop with its arguments.
    ///
    /// Fails unless there is exactly one argument per position;
    /// `mixop` must belong to `arena_mixop`.
    pub fn new(
        arena_mixop: &MixopArena,
        mixop: Mixop,
        args: Vec<T>,
    ) -> Result<Self, ArityMismatch> {
        let arity = arena_mixop.arity(mixop);
        match args.len().cmp(&arity) {
            Ordering::Less => Err(ArityMismatch::ArgumentCountTooFew),
            Ordering::Greater => Err(ArityMismatch::ArgumentCountTooMany),
            Ordering::Equal => Ok(Self { mixop, args }),
        }
    }

    /// Fills each position of a mixop, calling `fill_arg` once per position.
    pub fn fill(arena_mixop: &MixopArena, mixop: Mixop, fill_arg: impl FnMut(usize) -> T) -> Self {
        let arity = arena_mixop.arity(mixop);
        let args = (0..arity).map(fill_arg).collect();
        Self { mixop, args }
    }
}
