//! Unzipping lists into columns and zipping columns into lists
//!
//! `ValueArena::unzip` takes `[(1, 2), (3, 4)]` apart
//! into the columns `[1, 3]` and `[2, 4]`
//! by following a `Decomp` into every element;
//! `ValueArena::zip` builds `[(1, 2), (3, 4)]` back from those columns
//! by applying a `Comp` to the entries at each index.
//! Both read only immutable bodies, so their results are cached,
//! and zipping the columns unzipped from a list reuses that list's elements.

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
pub(crate) type Decomp = Vec<Vec<Step>>;

/// One step from a value to its child at a fixed position.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum Step {
    /// The component at an index of a tuple.
    Tuple(usize),
    /// The argument at an index of a case.
    Case(usize),
    /// The field at an index of a struct.
    Struct(usize),
    /// The element at an index of a list literal.
    List(usize),
    /// The payload of a present option.
    Opt,
}

/// The constructor of every zipped element and the variable of each argument.
///
/// The expression `(b, a)*` over `a` and `b` composes into
/// the shape `Tuple` with the variable indices `[1, 0]`.
pub(crate) struct Comp {
    /// The constructor.
    pub(crate) shape: Shape,
    /// Per argument, in order, the index of its variable.
    pub(crate) idxs: Vec<usize>,
}

/// The constructor of every element built by zipping.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum Shape {
    /// A tuple of the arguments.
    Tuple,
    /// A list literal of the arguments.
    List,
    /// A case of the mixop with the arguments.
    Case(Mixop),
}

// = Cache

/// Cached unzips and zips, keyed by immutable bodies of the owning arena.
#[derive(Debug, Default)]
pub(super) struct ZipCache {
    /// Columns, by source list and decomposition.
    unzips: HashMap<UnzipKey, Vec<Interned<ValueKind>>, RandomState>,
    /// Lists, by constructor, annotations, and columns in argument order.
    zips: HashMap<ZipKey, Value, RandomState>,
    /// Source lists, by the columns unzipped from them in argument order.
    origins: HashMap<Vec<Interned<ValueKind>>, Interned<ValueKind>, RandomState>,
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
    note_list: Interned<Rc<TypKind>>,
    columns: Vec<Interned<ValueKind>>,
}

/// A `ZipKey` borrowing its columns, for lookups.
#[derive(Hash)]
struct ZipQuery<'a> {
    shape: Shape,
    note_elem: Interned<Rc<TypKind>>,
    note_list: Interned<Rc<TypKind>>,
    columns: &'a [Interned<ValueKind>],
}

impl Equivalent<ZipKey> for ZipQuery<'_> {
    fn equivalent(&self, key: &ZipKey) -> bool {
        self.shape == key.shape
            && self.note_elem == key.note_elem
            && self.note_list == key.note_list
            && self.columns == key.columns
    }
}

// = Unzipping

impl ValueArena {
    /// Unzips a list into one column per variable of the decomposition.
    ///
    /// The list `[(1, 2), (3, 4)]`
    /// with the decomposition `[[Tuple(0)], [Tuple(1)]]`
    /// unzips into the columns `[1, 3]` and `[2, 4]`, typed by `typs` in order.
    pub(crate) fn unzip(
        &mut self,
        arena_mixop: &MixopArena,
        source: Interned<ValueKind>,
        decomp: &Decomp,
        typs: impl Iterator<Item = Rc<TypKind>>,
    ) -> Result<Vec<Value>, ValueError> {
        let span = Self::span_default();
        let notes: Vec<_> = typs
            .map(|typ| self.intern_typ(typ))
            .collect::<Result<_, _>>()?;
        // A cached unzip returns its columns
        if let Some(nodes) = self.zipper.unzips.get(&UnzipQuery { source, decomp }) {
            return Ok(nodes
                .iter()
                .zip(&notes)
                .map(|(node, note)| Value { node: *node, note: *note, span })
                .collect());
        }
        // Each column follows its variable's steps into every element
        let ValueKind::List(elems) = self.kind(source) else {
            unreachable!("unzipped source must be a list");
        };
        let columns: Vec<Vec<Value>> = decomp
            .iter()
            .map(|steps| {
                elems
                    .iter()
                    .map(|elem| self.project(*elem, steps))
                    .collect()
            })
            .collect();
        let mut nodes = Vec::with_capacity(columns.len());
        for column in columns {
            nodes.push(self.intern_kind(arena_mixop, ValueKind::List(column))?);
        }
        // Columns of the arguments in order can zip back to the source
        let in_order = decomp.iter().enumerate().all(|(idx, steps)| {
            matches!(
                steps.as_slice(),
                [Step::Tuple(pos) | Step::Case(pos) | Step::List(pos)] if *pos == idx
            )
        });
        if !decomp.is_empty() && in_order {
            self.zipper.origins.insert(nodes.clone(), source);
        }
        self.zipper
            .unzips
            .insert(UnzipKey { source, decomp: decomp.clone() }, nodes.clone());
        Ok(nodes
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
    /// Zips one column per variable into a list of the composed elements.
    ///
    /// The columns `[1, 3]` for `a` and `[2, 4]` for `b`
    /// with the composition of `(b, a)*` zip into `[(2, 1), (4, 3)]`,
    /// typed by `typ_elem` per element and `typ_list` as a whole.
    pub(crate) fn zip(
        &mut self,
        arena_mixop: &MixopArena,
        comp: &Comp,
        typ_elem: Rc<TypKind>,
        typ_list: Rc<TypKind>,
        columns_by_var: &[Interned<ValueKind>],
    ) -> Result<Value, ValueError> {
        let shape = comp.shape;
        let note_elem = self.intern_typ(typ_elem)?;
        let note_list = self.intern_typ(typ_list)?;
        // Each argument takes its variable's column
        let columns: Vec<_> = comp.idxs.iter().map(|idx| columns_by_var[*idx]).collect();
        // A cached zip returns its list
        let query = ZipQuery { shape, note_elem, note_list, columns: &columns };
        if let Some(value) = self.zipper.zips.get(&query) {
            return Ok(*value);
        }
        // Reuse the source the columns were unzipped from, or build new elements
        let node = match self.zip_origin(arena_mixop, shape, note_elem, &columns)? {
            Some(node) => node,
            None => self.zip_elems(arena_mixop, shape, note_elem, &columns)?,
        };
        let value = Value { node, note: note_list, span: Self::span_default() };
        self.zipper
            .zips
            .insert(ZipKey { shape, note_elem, note_list, columns }, value);
        Ok(value)
    }

    /// Reuses the elements of the list that the columns were unzipped from.
    ///
    /// After `(a, b)* <- xs` unzips `xs` into `a*` and `b*`,
    /// zipping `a*` and `b*` by `(a, b)*` returns the elements of `xs`.
    fn zip_origin(
        &mut self,
        arena_mixop: &MixopArena,
        shape: Shape,
        note_elem: Interned<Rc<TypKind>>,
        columns: &[Interned<ValueKind>],
    ) -> Result<Option<Interned<ValueKind>>, ValueError> {
        let Some(source) = self.zipper.origins.get(columns).copied() else {
            return Ok(None);
        };
        let ValueKind::List(elems) = self.kind(source) else {
            unreachable!("unzipped source must be a list");
        };
        // The source elements must be built by the same constructor at full length
        let built = elems.len() == self.column_len(columns)
            && elems
                .iter()
                .all(|elem| match (shape, self.kind(elem.node)) {
                    (Shape::Tuple, ValueKind::Tuple(values))
                    | (Shape::List, ValueKind::List(values)) => values.len() == columns.len(),
                    (Shape::Case(mixop), ValueKind::Case(value_case)) => {
                        *value_case.mixop() == mixop && value_case.args().len() == columns.len()
                    }
                    _ => false,
                });
        if !built {
            return Ok(None);
        }
        // The source serves as is when its elements carry the zipped annotations
        let span = Self::span_default();
        if elems
            .iter()
            .all(|elem| elem.note == note_elem && elem.span == span)
        {
            return Ok(Some(source));
        }
        // Otherwise the same bodies are annotated again
        let elems = elems
            .iter()
            .map(|elem| Value { node: elem.node, note: note_elem, span })
            .collect();
        Ok(Some(self.intern_kind(arena_mixop, ValueKind::List(elems))?))
    }

    /// Builds each element from the columns' entries at its index.
    ///
    /// The columns `[2, 4]` and `[1, 3]` by `Tuple` build `[(2, 1), (4, 3)]`.
    fn zip_elems(
        &mut self,
        arena_mixop: &MixopArena,
        shape: Shape,
        note_elem: Interned<Rc<TypKind>>,
        columns: &[Interned<ValueKind>],
    ) -> Result<Interned<ValueKind>, ValueError> {
        let span = Self::span_default();
        let len = self.column_len(columns);
        let mut elems = Vec::with_capacity(len);
        for idx in 0..len {
            // The entries at this index become the arguments
            let values: Vec<_> = columns
                .iter()
                .map(|column| {
                    let ValueKind::List(values) = self.kind(*column) else {
                        unreachable!("zipped column must be a list");
                    };
                    values[idx]
                })
                .collect();
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
        let node = self.intern_kind(arena_mixop, ValueKind::List(elems))?;
        // Unzipping the new list by argument position yields the columns again
        let decomp = (0..columns.len())
            .map(|idx| {
                vec![match shape {
                    Shape::Tuple => Step::Tuple(idx),
                    Shape::List => Step::List(idx),
                    Shape::Case(_) => Step::Case(idx),
                }]
            })
            .collect();
        self.zipper
            .unzips
            .insert(UnzipKey { source: node, decomp }, columns.to_vec());
        Ok(node)
    }

    /// The length shared by the zipped columns, zero without any column.
    fn column_len(&self, columns: &[Interned<ValueKind>]) -> usize {
        columns.first().map_or(0, |column| {
            let ValueKind::List(values) = self.kind(*column) else {
                unreachable!("zipped column must be a list");
            };
            values.len()
        })
    }
}
