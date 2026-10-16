//! Memoized successful pure calls
//!
//! Memo keys belong to the shared execution arena.
//! Evaluation entries retain memo tables within one program.
//! Clear or reset drops the tables; reset runners before resetting arena values.
//! RunnerContext records effects independently of memoization.

use std::{
    hash::{BuildHasher, Hash, Hasher},
    rc::Rc,
};

use foldhash::fast::RandomState;
use hashbrown::{Equivalent, HashTable, hash_table::Entry};
use smallvec::SmallVec;

use crate::lang::{
    data::{
        arena::Arena,
        intern::CanonId,
        typ::{FuncTyp, Typ, TypKind},
        value::flat::{Value, ValueKind},
    },
    traits::eq::SyntaxEq,
};

// = Call identity

/// Type arguments of a call, compared and hashed without their spans.
#[derive(Debug)]
struct TypeArgs(Box<[Typ]>);

impl PartialEq for TypeArgs {
    fn eq(&self, other: &Self) -> bool {
        self.0.syntax_eq(&other.0)
    }
}
impl Eq for TypeArgs {}

/// Hashes types as `SyntaxEq` compares them, ignoring every span.
fn hash_typs<H: Hasher>(typs: &[Typ], hasher: &mut H) {
    typs.len().hash(hasher);
    for typ in typs {
        hash_typ(&typ.node, hasher);
    }
}

fn hash_typ<H: Hasher>(typ: &TypKind, hasher: &mut H) {
    std::mem::discriminant(typ).hash(hasher);
    match typ {
        // Primitives carry no further structure
        TypKind::Bool | TypKind::Text => {}
        // Numbers by their kind
        TypKind::Num(num_typ) => num_typ.hash(hasher),
        // Named types by name and arguments
        TypKind::Var(id, targs) => {
            id.node.hash(hasher);
            hash_typs(targs, hasher);
        }
        // Tuples by their components
        TypKind::Tuple(typs) => hash_typs(typs, hasher),
        // Iterations by the element type and iterator
        TypKind::Iter(typ, iter) => {
            hash_typ(&typ.node, hasher);
            iter.hash(hasher);
        }
        // Functions by type parameters, parameters, and result
        TypKind::Func(FuncTyp { tparams, typs_params, typ_ret }) => {
            tparams.len().hash(hasher);
            for tparam in tparams {
                tparam.node.hash(hasher);
            }
            hash_typs(typs_params, hasher);
            hash_typ(&typ_ret.node, hasher);
        }
    }
}

/// A call identified by its name, type arguments, and canonical argument values.
///
/// The key keeps the hash its lookup computed, so storing it never rehashes.
#[derive(Debug, PartialEq, Eq)]
pub(crate) struct CallKey {
    hash: u64,
    name: Rc<str>,
    targs: TypeArgs,
    values: SmallVec<[CanonId<ValueKind>; 4]>,
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
        // Value annotations never distinguish calls
        self.name.hash(hasher);
        hash_typs(self.targs, hasher);
        self.values.len().hash(hasher);
        for value in self.values {
            self.arena.value.canon_id(value.node).hash(hasher);
        }
    }
}

impl Equivalent<CallKey> for CallQuery<'_> {
    fn equivalent(&self, key: &CallKey) -> bool {
        self.name == key.name.as_ref()
            && self.targs.syntax_eq(&key.targs.0)
            && self.values.len() == key.values.len()
            && self
                .values
                .iter()
                .zip(&key.values)
                .all(|(value, id)| self.arena.value.canon_id(value.node) == *id)
    }
}

impl CallQuery<'_> {
    /// Owns the queried call under the hash already computed for it.
    fn into_key(self, name: &Rc<str>, hash: u64) -> CallKey {
        CallKey {
            hash,
            name: Rc::clone(name),
            targs: TypeArgs(self.targs.iter().cloned().collect()),
            values: self
                .values
                .iter()
                .map(|value| self.arena.value.canon_id(value.node))
                .collect(),
        }
    }
}

/// Results by call, each stored under its key's retained hash.
pub(crate) struct Memo<V>(HashTable<(CallKey, V)>);

impl<V> Default for Memo<V> {
    fn default() -> Self {
        Self(HashTable::new())
    }
}

impl<V> Memo<V> {
    /// Finds the result of an equivalent call with the given hash.
    fn get(&self, query: &CallQuery<'_>, hash: u64) -> Option<&V> {
        self.0
            .find(hash, |(key, _)| key.hash == hash && query.equivalent(key))
            .map(|(_, value)| value)
    }

    /// Stores a result, replacing an equal call's earlier one.
    pub(crate) fn insert(&mut self, key: CallKey, value: V) {
        match self
            .0
            .entry(key.hash, |(other, _)| *other == key, |(key, _)| key.hash)
        {
            Entry::Occupied(mut entry) => entry.get_mut().1 = value,
            Entry::Vacant(entry) => {
                entry.insert((key, value));
            }
        }
    }

    fn clear(&mut self) {
        self.0.clear();
    }
}

// = Call cache

/// Memoized pure call results.
#[derive(Default)]
pub struct Cache {
    hasher: RandomState,
    /// Function results by call.
    pub(crate) funcs: Memo<Value>,
    /// Relation outputs by call.
    pub(crate) rels: Memo<Vec<Value>>,
}

impl Cache {
    /// Finds a function result, or the key that will memoize it.
    pub(crate) fn lookup_func(
        &self,
        arena: &Arena,
        name: &Rc<str>,
        targs: &[Typ],
        values: &[Value],
    ) -> Result<Value, CallKey> {
        let query = CallQuery { arena, name, targs, values };
        let hash = self.hasher.hash_one(&query);
        match self.funcs.get(&query, hash) {
            Some(value) => Ok(*value),
            None => Err(query.into_key(name, hash)),
        }
    }

    /// Finds relation outputs, or the key that will memoize them.
    pub(crate) fn lookup_rel(
        &self,
        arena: &Arena,
        name: &Rc<str>,
        values: &[Value],
    ) -> Result<Vec<Value>, CallKey> {
        let query = CallQuery { arena, name, targs: &[], values };
        let hash = self.hasher.hash_one(&query);
        match self.rels.get(&query, hash) {
            Some(values) => Ok(values.clone()),
            None => Err(query.into_key(name, hash)),
        }
    }

    // - Lifecycle

    /// Drops memoized results.
    pub(crate) fn clear(&mut self) {
        self.funcs.clear();
        self.rels.clear();
    }
}
