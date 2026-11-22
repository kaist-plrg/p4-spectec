//! Memoized successful pure calls
//!
//! Memo keys belong to the shared execution arena.
//! Evaluation entries retain memo tables within one program.
//! Clear or reset drops the tables; reset runners before resetting arena values.
//! RunnerContext records effects independently of memoization.

use std::rc::Rc;

use foldhash::fast::RandomState;
use hashbrown::HashMap;
use smallvec::SmallVec;

use crate::lang::data::{
    arena::Arena,
    intern::{CanonId, Interned},
    typ::{Typ, TypKind},
    value::flat::{Value, ValueKind},
};

// = Call identity

/// A call identified by its name, type arguments and annotated values.
#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) struct CallKey {
    name: Rc<str>,
    targs: Vec<Typ>,
    values: SmallVec<[ValueKey; 4]>,
}

/// A canonical body and the type annotations of all values within it.
#[derive(Debug, PartialEq, Eq, Hash)]
struct ValueKey {
    body: CanonId<ValueKind>,
    annotations: Vec<Interned<Rc<TypKind>>>,
}

impl ValueKey {
    fn new(arena: &Arena, value: Value) -> Self {
        let mut annotations = Vec::new();
        let mut pending = vec![value];
        while let Some(value) = pending.pop() {
            annotations.push(value.note);
            match arena.value.kind(value.node) {
                ValueKind::Struct(fields) => {
                    pending.extend(fields.iter().rev().map(|field| field.value));
                }
                ValueKind::Case(case) => pending.extend(case.args().iter().rev().copied()),
                ValueKind::Tuple(values) | ValueKind::List(values) => {
                    pending.extend(values.iter().rev().copied());
                }
                ValueKind::Opt(Some(value)) => pending.push(*value),
                ValueKind::Bool(_)
                | ValueKind::Num(_)
                | ValueKind::Text(_)
                | ValueKind::Opt(None)
                | ValueKind::Func(_)
                | ValueKind::Extern(_) => {}
            }
        }
        Self { body: arena.value.canon_id(value.node), annotations }
    }
}

impl CallKey {
    /// Builds the same key for lookup and insertion.
    pub(crate) fn new(arena: &Arena, name: &Rc<str>, targs: &[Typ], values: &[Value]) -> Self {
        Self {
            name: Rc::clone(name),
            targs: targs.to_vec(),
            values: values
                .iter()
                .map(|value| ValueKey::new(arena, *value))
                .collect(),
        }
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
    /// Looks up a function using its type arguments and annotated inputs.
    pub(crate) fn find_func(
        &self,
        arena: &Arena,
        name: &Rc<str>,
        targs: &[Typ],
        values: &[Value],
    ) -> Option<&Value> {
        self.funcs.get(&CallKey::new(arena, name, targs, values))
    }

    /// Looks up a relation using its annotated inputs.
    pub(crate) fn find_rel(
        &self,
        arena: &Arena,
        name: &Rc<str>,
        values: &[Value],
    ) -> Option<&Vec<Value>> {
        self.rels.get(&CallKey::new(arena, name, &[], values))
    }

    // - Lifecycle

    /// Drops memoized results.
    pub(crate) fn clear(&mut self) {
        self.funcs.clear();
        self.rels.clear();
    }
}
