//! Free identifiers of notation forms
//!
//! A mixop contains atoms and argument positions, but no identifiers.
//! A mixfix delegates free-identifier collection to its arguments.

use crate::lang::{common::ds::set::IdSet, traits::free::FreeIds};

use super::{Mixfix, tree::Mixop};

// - Mixops

impl FreeIds for Mixop {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

// - Mixfix forms

impl<M, T: FreeIds> FreeIds for Mixfix<M, T> {
    fn free_ids_into(&self, free: &mut IdSet) {
        self.args.as_slice().free_ids_into(free);
    }
}
