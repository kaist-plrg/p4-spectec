//! Memoized successful pure calls
//!
//! Memo keys belong to the shared execution arena.
//! Public entries clear the memo tables.
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
    value::flat::{Value, ValueKind},
};

// = Call identity

/// A call identified by its name and canonical argument values.
#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) struct CallKey {
    name: Rc<str>,
    values: SmallVec<[CanonId<ValueKind>; 4]>,
}

impl CallKey {
    /// Builds the key; type arguments and annotations never distinguish calls.
    pub(crate) fn new(arena: &Arena, name: &Rc<str>, values: &[Value]) -> Self {
        Self {
            name: Rc::clone(name),
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
    values: &'a [Value],
}

impl Hash for CallQuery<'_> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        // Match CallKey's derived hash, including the argument slice length
        self.name.hash(hasher);
        self.values.len().hash(hasher);
        for value in self.values {
            self.arena.value.canon_id(value.node).hash(hasher);
        }
    }
}

impl Equivalent<CallKey> for CallQuery<'_> {
    fn equivalent(&self, key: &CallKey) -> bool {
        self.name == key.name.as_ref()
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
    pub(crate) fn find_func(&self, arena: &Arena, name: &str, values: &[Value]) -> Option<&Value> {
        self.funcs.get(&CallQuery { arena, name, values })
    }

    /// Looks up a relation without allocating an owned call key.
    pub(crate) fn find_rel(
        &self,
        arena: &Arena,
        name: &str,
        values: &[Value],
    ) -> Option<&Vec<Value>> {
        self.rels.get(&CallQuery { arena, name, values })
    }

    // - Lifecycle

    /// Drops memoized results.
    pub(crate) fn clear(&mut self) {
        self.funcs.clear();
        self.rels.clear();
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::lang::{common::source::Span, data::value::flat::make};

    use super::*;

    #[test]
    fn borrowed_queries_match_owned_keys_at_every_arity() {
        let mut arena = Arena::new();
        let mut cache = Cache::default();
        let name: Rc<str> = "f".into();
        let values = (0..9)
            .map(|n| make::nat(&mut arena, (n as u64).into(), Span::default()).unwrap())
            .collect::<Vec<_>>();
        // Exercise both short and longer argument lists with distinct prefixes
        for len in 0..values.len() {
            cache
                .funcs
                .insert(CallKey::new(&arena, &name, &values[..len]), values[len]);
        }
        // Lookups use equal text independently of the owning identifier
        let name_query = String::from("f");
        for len in 0..values.len() {
            assert_eq!(cache.find_func(&arena, &name_query, &values[..len]), Some(&values[len]));
        }
        assert!(cache.find_func(&arena, "g", &values[..1]).is_none());
    }
}
