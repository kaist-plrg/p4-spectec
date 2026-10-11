//! Successful call results and invocation-local effect tracking
//!
//! Memo keys belong to the runner's arena.
//! Public entries clear the memo tables;
//! active effect frames survive reentry and propagate taint back to callers.

use std::hash::{Hash, Hasher};

use foldhash::fast::RandomState;
use hashbrown::{Equivalent, HashMap};

use crate::lang::data::{
    intern::CanonId,
    value::{Value, ValueArena, ValueKind},
};

// = Call identity

/// A call identified by its name and canonical argument values.
#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) struct CallKey {
    name: String,
    values: Vec<CanonId<ValueKind>>,
}

impl CallKey {
    /// Builds the key; type arguments and annotations never distinguish calls.
    pub(crate) fn new(arena: &ValueArena, name: &str, values: &[Value]) -> Self {
        Self {
            name: name.to_owned(),
            values: values.iter().map(|value| arena.canon_id(value)).collect(),
        }
    }
}

/// A borrowed call lookup that canonicalizes arguments without allocating.
struct CallQuery<'a> {
    arena: &'a ValueArena,
    name: &'a str,
    values: &'a [Value],
}

impl Hash for CallQuery<'_> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        // Match CallKey's derived hash, including the argument slice length
        self.name.hash(hasher);
        self.values.len().hash(hasher);
        for value in self.values {
            self.arena.canon_id(value).hash(hasher);
        }
    }
}

impl Equivalent<CallKey> for CallQuery<'_> {
    fn equivalent(&self, key: &CallKey) -> bool {
        self.name == key.name
            && self.values.len() == key.values.len()
            && self
                .values
                .iter()
                .zip(&key.values)
                .all(|(value, id)| self.arena.canon_id(value) == *id)
    }
}

// = Call cache

/// Memoized call results and the stack of active invocation effect frames.
#[derive(Default)]
pub struct Cache {
    /// Function results by call.
    pub(crate) funcs: HashMap<CallKey, Value, RandomState>,
    /// Relation outputs by call.
    pub(crate) rels: HashMap<CallKey, Vec<Value>, RandomState>,
    /// One flag per active invocation: whether it had a side effect so far.
    effects: Vec<bool>,
}

impl Cache {
    /// Looks up a function without allocating an owned call key.
    pub(crate) fn find_func(
        &self,
        arena: &ValueArena,
        name: &str,
        values: &[Value],
    ) -> Option<&Value> {
        self.funcs.get(&CallQuery { arena, name, values })
    }

    /// Looks up a relation without allocating an owned call key.
    pub(crate) fn find_rel(
        &self,
        arena: &ValueArena,
        name: &str,
        values: &[Value],
    ) -> Option<&Vec<Value>> {
        self.rels.get(&CallQuery { arena, name, values })
    }

    // - Lifecycle

    /// Drops memoized results; effect frames survive.
    pub(crate) fn clear(&mut self) {
        self.funcs.clear();
        self.rels.clear();
    }

    // - Invocation effects

    /// Opens an invocation frame.
    pub(crate) fn begin(&mut self) {
        self.effects.push(false);
    }

    /// Records a side effect on the innermost frame.
    pub(crate) fn mark_effect(&mut self, side_effected: bool) {
        if let Some(effect) = self.effects.last_mut() {
            *effect |= side_effected;
        }
    }

    /// Finishes an invocation and reports whether it remained pure.
    pub(crate) fn end(&mut self) -> bool {
        // Taint propagates to the caller's frame
        let side_effected = self.effects.pop().expect("active invocation frame");
        self.mark_effect(side_effected);
        !side_effected
    }
}
