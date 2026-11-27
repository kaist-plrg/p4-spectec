//! Memoized successful pure calls
//!
//! Memo keys belong to the shared execution arena.
//! Evaluation entries retain memo tables within one program.
//! Clear or reset drops the tables; reset runners before resetting arena values.
//! RunnerContext records effects independently of memoization.

use std::{
    hash::{Hash, Hasher},
    rc::Rc,
};

use foldhash::fast::RandomState;
use hashbrown::{Equivalent, HashMap};
use smallvec::SmallVec;

use crate::lang::data::{
    arena::Arena,
    intern::CanonId,
    typ::Typ,
    value::flat::{Value, ValueKind},
};

// = Call identity

/// A call identified by its name, type arguments, and canonical argument values.
#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) struct CallKey {
    name: Rc<str>,
    targs: Vec<Typ>,
    values: SmallVec<[CanonId<ValueKind>; 4]>,
}

impl CallKey {
    /// Builds a key with the resolved type arguments.
    pub(crate) fn new(arena: &Arena, name: &Rc<str>, targs: &[Typ], values: &[Value]) -> Self {
        Self {
            name: Rc::clone(name),
            targs: targs.to_vec(),
            values: values
                .iter()
                .map(|value| arena.value.canon_id(value.node))
                .collect(),
        }
    }
}

/// A borrowed call lookup that canonicalizes arguments without allocating.
struct CallQuery<'a> {
    arena: &'a Arena,
    name: &'a str,
    targs: &'a [Typ],
    values: &'a [Value],
}

impl Hash for CallQuery<'_> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        // Match CallKey's derived hash, including the argument slice length
        self.name.hash(hasher);
        self.targs.hash(hasher);
        self.values.len().hash(hasher);
        for value in self.values {
            self.arena.value.canon_id(value.node).hash(hasher);
        }
    }
}

impl Equivalent<CallKey> for CallQuery<'_> {
    fn equivalent(&self, key: &CallKey) -> bool {
        self.name == key.name.as_ref()
            && self.targs == key.targs
            && self.values.len() == key.values.len()
            && self
                .values
                .iter()
                .zip(&key.values)
                .all(|(value, id)| self.arena.value.canon_id(value.node) == *id)
    }
}

// = Call cache

/// Memoized pure call results.
#[derive(Default)]
pub struct Cache {
    /// Function results by call.
    pub(crate) funcs: HashMap<CallKey, Value, RandomState>,
    /// Relation outputs by call.
    pub(crate) rels: HashMap<CallKey, Vec<Value>, RandomState>,
}

impl Cache {
    /// Looks up a function without allocating an owned call key.
    pub(crate) fn find_func(
        &self,
        arena: &Arena,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Option<&Value> {
        self.funcs.get(&CallQuery { arena, name, targs, values })
    }

    /// Looks up a relation without allocating an owned call key.
    pub(crate) fn find_rel(
        &self,
        arena: &Arena,
        name: &str,
        values: &[Value],
    ) -> Option<&Vec<Value>> {
        self.rels
            .get(&CallQuery { arena, name, targs: &[], values })
    }

    // - Lifecycle

    /// Drops memoized results.
    pub(crate) fn clear(&mut self) {
        self.funcs.clear();
        self.rels.clear();
    }
}
