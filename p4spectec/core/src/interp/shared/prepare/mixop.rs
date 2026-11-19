//! Intern source mixops for execution
//!
//! Mixop preparation needs only the specification's notation arena.

use std::rc::Rc;

use crate::lang::data::notation::{MixopArena, flat, tree};

use super::Prepare;

/// Stores the specification's interned mixops.
pub type PrepareContext = MixopArena;

impl Prepare<PrepareContext> for Rc<tree::Mixop> {
    type Output = flat::Mixop;

    fn prepare(self, arena_mixop: &mut PrepareContext) -> Self::Output {
        arena_mixop
            .intern(&self)
            .expect("specification mixops fit in 32-bit shape handles")
    }
}
