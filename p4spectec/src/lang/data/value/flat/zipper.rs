//! Unzipping lists into the outer values of their variables, and back
//!
//! ```text
//! (a, b)* <- [(1, 2), (3, 4)]   unzip   a* = [1, 3], b* = [2, 4]
//! (b, a)*                       zip     [(2, 1), (4, 3)]
//! ```
//!
//! `ValueArena::unzip` follows a `Decomp` into every element;
//! `ValueArena::zip` builds every element by a `Comp`.

use std::rc::Rc;

use foldhash::fast::RandomState;
use hashbrown::{Equivalent, HashMap};

use crate::lang::data::{
    intern::Interned,
    notation::{MixopArena, flat::Mixop},
    typ::TypKind,
};

use super::super::ValueError;
use super::{Value, ValueArena, ValueCase, ValueKind};

// = Decomposition and composition

/// Per variable, in order, the steps from an element to that variable.
///
/// The pattern `(a, (b, c))` over `a`, `b`, and `c` decomposes into
/// `[[Tuple(0)], [Tuple(1), Tuple(0)], [Tuple(1), Tuple(1)]]`.
pub type Decomp = Vec<Vec<Step>>;

/// One step from a value to its child at a fixed position.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Step {
    /// A tuple component by index: `Tuple(1)` takes `(1, 2)` to `2`.
    Tuple(usize),
    /// A case argument by index: `Case(1)` takes `x ':' 2` to `2`.
    Case(usize),
    /// A struct field by index: `Struct(0)` takes `{F 1, G 2}` to `1`.
    Struct(usize),
    /// A list literal element by index: `List(0)` takes `[1, 2]` to `1`.
    List(usize),
    /// A present option's payload: `Opt` takes `?(1)` to `1`.
    Opt,
}

/// The constructor of every zipped element and the variable of each argument.
///
/// The expression `(b, a)*` over `a` and `b` composes into
/// the shape `Tuple` with the variable indices `[1, 0]`.
#[derive(Clone, Debug, PartialEq)]
pub struct Comp {
    /// The constructor.
    pub shape: Shape,
    /// Per argument, in order, the index of its variable.
    pub idxs: Vec<usize>,
}

/// The constructor of every element built by zipping.
///
/// `(a, b)*` builds tuples, `[a, b]*` list literals,
/// and `(a ':' b)*` cases of `:`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Shape {
    /// A tuple of the arguments.
    Tuple,
    /// A list literal of the arguments.
    List,
    /// A case of the mixop with the arguments.
    Case(Mixop),
}

// = Cache

/// Cached unzips and zips of the owning arena.
///
/// They remember values without their types,
/// since the same unzip or zip gives the same contents to every caller;
/// each call attaches its own types to what it reads back.
#[derive(Debug, Default)]
pub(super) struct ZipCache {
    /// Outer values, by source list and decomposition.
    unzips: HashMap<UnzipKey, Vec<Interned<ValueKind>>, RandomState>,
    /// Lists, by constructor, element type, and outer values in argument order.
    zips: HashMap<ZipKey, Interned<ValueKind>, RandomState>,
}

// - Unzip keys

#[derive(Debug, PartialEq, Eq, Hash)]
struct UnzipKey {
    source: Interned<ValueKind>,
    decomp: Decomp,
}

/// An `UnzipKey` borrowing its decomposition, for lookups.
#[derive(Hash)]
struct UnzipQuery<'a> {
    source: Interned<ValueKind>,
    decomp: &'a [Vec<Step>],
}

impl Equivalent<UnzipKey> for UnzipQuery<'_> {
    fn equivalent(&self, key: &UnzipKey) -> bool {
        self.source == key.source && self.decomp == key.decomp.as_slice()
    }
}

// - Zip keys

#[derive(Debug, PartialEq, Eq, Hash)]
struct ZipKey {
    shape: Shape,
    note_elem: Interned<Rc<TypKind>>,
    nodes_arg: Vec<Interned<ValueKind>>,
}

/// A `ZipKey` borrowing its outer values, for lookups.
#[derive(Hash)]
struct ZipQuery<'a> {
    shape: Shape,
    note_elem: Interned<Rc<TypKind>>,
    nodes_arg: &'a [Interned<ValueKind>],
}

impl Equivalent<ZipKey> for ZipQuery<'_> {
    fn equivalent(&self, key: &ZipKey) -> bool {
        self.shape == key.shape
            && self.note_elem == key.note_elem
            && self.nodes_arg == key.nodes_arg
    }
}

// = Unzipping

impl ValueArena {
    /// Unzips a list into the outer value of each decomposed variable.
    ///
    /// ```text
    /// source  [(1, 2), (3, 4)]
    /// decomp  [[Tuple(0)], [Tuple(1)]]
    /// typs    nat*, nat*
    /// result  [1, 3] : nat*, [2, 4] : nat*
    /// ```
    pub(crate) fn unzip(
        &mut self,
        arena_mixop: &MixopArena,
        source: Interned<ValueKind>,
        decomp: &Decomp,
        typs: impl Iterator<Item = Rc<TypKind>>,
    ) -> Result<Vec<Value>, ValueError> {
        let span = Self::span_default();
        // Type the outer values of the variables: nat*, nat*
        let notes: Vec<_> = typs
            .map(|typ| self.intern_typ(typ))
            .collect::<Result<_, _>>()?;
        // A cached unzip returns its outer values, typed for this call
        if let Some(nodes_outer) = self.zipper.unzips.get(&UnzipQuery { source, decomp }) {
            return Ok(nodes_outer
                .iter()
                .zip(&notes)
                .map(|(node, note)| Value { node: *node, note: *note, span })
                .collect());
        }
        // Follow each variable's steps into every element: 1, 3 and 2, 4
        let ValueKind::List(elems) = self.kind(source) else {
            unreachable!("unzipped source must be a list");
        };
        let values_by_var: Vec<Vec<Value>> = decomp
            .iter()
            .map(|steps| {
                elems
                    .iter()
                    .map(|elem| self.project(*elem, steps))
                    .collect()
            })
            .collect();
        // Store each variable's values as one list: [1, 3] and [2, 4]
        let mut nodes_outer = Vec::with_capacity(values_by_var.len());
        for values in values_by_var {
            nodes_outer.push(self.intern_kind(arena_mixop, ValueKind::List(values))?);
        }
        // Remember the lists without their types
        self.zipper
            .unzips
            .insert(UnzipKey { source, decomp: decomp.clone() }, nodes_outer.clone());
        // Type each list for this call: [1, 3] : nat*, [2, 4] : nat*
        Ok(nodes_outer
            .into_iter()
            .zip(notes)
            .map(|(node, note)| Value { node, note, span })
            .collect())
    }

    /// Follows the steps from an element down to one of its children.
    ///
    /// The steps `[Tuple(1), Tuple(0)]` take `(1, (2, 3))` to `2`.
    fn project(&self, mut value: Value, steps: &[Step]) -> Value {
        for step in steps {
            value = match (step, self.kind(value.node)) {
                // A tuple or list literal by index
                (Step::Tuple(idx), ValueKind::Tuple(values))
                | (Step::List(idx), ValueKind::List(values)) => values[*idx],
                // A case by argument index
                (Step::Case(idx), ValueKind::Case(value_case)) => value_case.args()[*idx],
                // A struct by field index
                (Step::Struct(idx), ValueKind::Struct(value_fields)) => value_fields[*idx].value,
                // An option by its payload
                (Step::Opt, ValueKind::Opt(Some(value))) => *value,
                // Decomposition follows the pattern the value matched
                _ => unreachable!("assignment pattern must match the value"),
            };
        }
        value
    }
}

// = Zipping

impl ValueArena {
    /// Zips the outer values of the variables into a list of composed elements.
    ///
    /// ```text
    /// values_outer  a* = [1, 3], b* = [2, 4]   in variable order
    /// comp          Tuple with idxs [1, 0]
    /// values_arg    b* = [2, 4], a* = [1, 3]   in argument order
    /// result        [(2, 1), (4, 3)] : (nat, nat)*
    /// ```
    pub(crate) fn zip(
        &mut self,
        arena_mixop: &MixopArena,
        comp: &Comp,
        typ_elem: Rc<TypKind>,
        typ_list: Rc<TypKind>,
        values_outer: &[Value],
    ) -> Result<Value, ValueError> {
        let shape = comp.shape;
        // Type the elements and the list: (nat, nat) and (nat, nat)*
        let note_elem = self.intern_typ(typ_elem)?;
        let note_list = self.intern_typ(typ_list)?;
        // Each argument takes its variable's outer value: [2, 4], [1, 3]
        let nodes_arg: Vec<_> = comp
            .idxs
            .iter()
            .map(|idx| values_outer[*idx].node)
            .collect();
        // Reuse a cached list or build its elements: [(2, 1), (4, 3)]
        let query = ZipQuery { shape, note_elem, nodes_arg: &nodes_arg };
        let node = match self.zipper.zips.get(&query) {
            Some(node) => *node,
            None => {
                let node = self.zip_elems(arena_mixop, shape, note_elem, &nodes_arg)?;
                // Remember the list without its type
                self.zipper
                    .zips
                    .insert(ZipKey { shape, note_elem, nodes_arg }, node);
                node
            }
        };
        // Type the list for this call: [(2, 1), (4, 3)] : (nat, nat)*
        Ok(Value { node, note: note_list, span: Self::span_default() })
    }

    /// Builds each element from the outer values' entries at its index.
    ///
    /// ```text
    /// nodes_arg  [2, 4], [1, 3]   by Tuple
    /// index 0    (2, 1)
    /// index 1    (4, 3)
    /// ```
    fn zip_elems(
        &mut self,
        arena_mixop: &MixopArena,
        shape: Shape,
        note_elem: Interned<Rc<TypKind>>,
        nodes_arg: &[Interned<ValueKind>],
    ) -> Result<Interned<ValueKind>, ValueError> {
        let span = Self::span_default();
        // Every outer value has the same length: 2
        let len = self.len_outer(nodes_arg);
        let mut elems = Vec::with_capacity(len);
        for idx in 0..len {
            // The entries at this index become the arguments: 2 and 1 at 0
            let values: Vec<_> = nodes_arg
                .iter()
                .map(|node_arg| {
                    let ValueKind::List(values) = self.kind(*node_arg) else {
                        unreachable!("outer value must be a list");
                    };
                    values[idx]
                })
                .collect();
            // Build the element by the constructor: (2, 1) by Tuple
            let kind = match shape {
                Shape::Tuple => ValueKind::Tuple(values),
                Shape::List => ValueKind::List(values),
                Shape::Case(mixop) => ValueKind::Case(
                    ValueCase::new(arena_mixop, mixop, values).expect("prepared constructor arity"),
                ),
            };
            let node = self.intern_kind(arena_mixop, kind)?;
            elems.push(Value { node, note: note_elem, span });
        }
        // Store the elements as one list: [(2, 1), (4, 3)]
        let node = self.intern_kind(arena_mixop, ValueKind::List(elems))?;
        Ok(node)
    }

    /// The length shared by the outer values, zero without any.
    fn len_outer(&self, nodes: &[Interned<ValueKind>]) -> usize {
        nodes.first().map_or(0, |node| {
            let ValueKind::List(values) = self.kind(*node) else {
                unreachable!("outer value must be a list");
            };
            values.len()
        })
    }
}
