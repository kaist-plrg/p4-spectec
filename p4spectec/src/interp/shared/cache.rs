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
impl Hash for TypeArgs {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        hash_typs(&self.0, hasher);
    }
}

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
#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) struct CallKey {
    name: Rc<str>,
    targs: TypeArgs,
    values: SmallVec<[CanonId<ValueKind>; 4]>,
}

impl CallKey {
    /// Builds the key; value annotations never distinguish calls.
    pub(crate) fn new(arena: &Arena, name: &Rc<str>, targs: &[Typ], values: &[Value]) -> Self {
        Self {
            name: Rc::clone(name),
            targs: TypeArgs(targs.iter().cloned().collect()),
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
