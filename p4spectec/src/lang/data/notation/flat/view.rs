//! Borrowed views of flat mixops
//!
//! `MixopRef` pairs a mixop with the arena used to compare its contents.

use super::{Mixop, MixopArena};

// = Borrowed views

/// A mixop together with its arena, for comparison.
///
/// Comparisons require both views to refer to the same arena.
#[derive(Clone, Copy, Debug)]
pub struct MixopRef<'a> {
    pub(super) arena_mixop: &'a MixopArena,
    pub(super) mixop: Mixop,
}

// = Construction

impl Mixop {
    /// Pairs this mixop with its arena for comparison.
    pub fn view(self, arena_mixop: &MixopArena) -> MixopRef<'_> {
        MixopRef { arena_mixop, mixop: self }
    }
}
