//! Unzipping lists into columns and zipping columns into lists
//!
//! `ValueArena::unzip` takes the child at a fixed path out of every element;
//! `ValueArena::zip` builds each element from the columns' entries.
//! Both read only immutable bodies, so their results are cached,
//! and zipping columns unzipped from a list returns that list's elements.

use super::super::ValueError;
use super::{Value, ValueArena, ValueKind};
use crate::lang::data::{intern::Interned, notation::MixopArena, typ::TypKind};
use foldhash::fast::RandomState;
use hashbrown::{Equivalent, HashMap};
use std::rc::Rc;

/// One structural child selection in a fixed assignment pattern.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum Step {
    Tuple(usize),
    Case(usize),
    Struct(usize),
    List(usize),
    Opt,
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct UnzipKey {
    source: Interned<ValueKind>,
    paths: Vec<Vec<Step>>,
}

#[derive(Hash)]
struct UnzipQuery<'a> {
    source: Interned<ValueKind>,
    paths: &'a [&'a [Step]],
}

impl Equivalent<UnzipKey> for UnzipQuery<'_> {
    fn equivalent(&self, key: &UnzipKey) -> bool {
        self.source == key.source
            && self.paths.len() == key.paths.len()
            && self
                .paths
                .iter()
                .zip(&key.paths)
                .all(|(path, stored)| *path == stored.as_slice())
    }
}

/// Unzipped columns and zipped lists of the owning value arena.
#[derive(Debug, Default)]
pub(super) struct ZipCache {
    unzips: HashMap<UnzipKey, Vec<Interned<ValueKind>>, RandomState>,
    origins: HashMap<Vec<Interned<ValueKind>>, Interned<ValueKind>, RandomState>,
    zips: HashMap<ZipKey, Value, RandomState>,
}

impl ValueArena {
    /// Selects a child through a validated assignment pattern.
    fn project(&self, mut value: Value, path: &[Step]) -> Value {
        for step in path {
            value = match (step, self.kind(value.node)) {
                (Step::Tuple(idx), ValueKind::Tuple(values))
                | (Step::List(idx), ValueKind::List(values)) => values[*idx],
                (Step::Case(idx), ValueKind::Case(value_case)) => value_case.args()[*idx],
                (Step::Struct(idx), ValueKind::Struct(value_fields)) => value_fields[*idx].value,
                (Step::Opt, ValueKind::Opt(Some(value))) => *value,
                _ => unreachable!("assignment pattern must match the value"),
            };
        }
        value
    }

    /// Collects exact child columns once for each immutable source and pattern.
    pub(crate) fn unzip(
        &mut self,
        arena_mixop: &MixopArena,
        source: Interned<ValueKind>,
        paths: &[&[Step]],
        typs: impl Iterator<Item = Rc<TypKind>>,
    ) -> Result<Vec<Value>, ValueError> {
        let types: Vec<_> = typs
            .map(|typ| self.intern_typ(typ))
            .collect::<Result<_, _>>()?;
        if let Some(values) = self.zipper.unzips.get(&UnzipQuery { source, paths }) {
            return Ok(values
                .iter()
                .zip(&types)
                .map(|(node, note)| Value { node: *node, note: *note, span: Self::span_default() })
                .collect());
        }
        let ValueKind::List(values) = self.kind(source) else {
            unreachable!("iteration assignment value must be a list");
        };
        let columns: Vec<Vec<Value>> = paths
            .iter()
            .map(|path| {
                values
                    .iter()
                    .map(|value| self.project(*value, path))
                    .collect()
            })
            .collect();
        let mut values = Vec::with_capacity(columns.len());
        for (column, note) in columns.into_iter().zip(&types) {
            let node = self.intern_kind(arena_mixop, ValueKind::List(column))?;
            values.push(Value { node, note: *note, span: Self::span_default() });
        }
        let nodes: Vec<_> = values.iter().map(|value| value.node).collect();
        if !paths.is_empty() && paths.iter().enumerate().all(|(idx, path)| matches!(*path, [Step::Tuple(pos) | Step::Case(pos) | Step::List(pos)] if *pos == idx)) {
            self.zipper.origins.insert(nodes.clone(), source);
        }
        self.zipper.unzips.insert(
            UnzipKey { source, paths: paths.iter().map(|path| path.to_vec()).collect() },
            nodes,
        );
        Ok(values)
    }
}

/// The constructor of every element built by zipping.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum Shape {
    Tuple,
    List,
    Case(crate::lang::data::notation::flat::Mixop),
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct ZipKey {
    shape: Shape,
    note_elem: Interned<Rc<TypKind>>,
    note_list: Interned<Rc<TypKind>>,
    columns: Vec<Interned<ValueKind>>,
}

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

impl ValueArena {
    /// Builds a list whose elements take their arguments from the columns.
    pub(crate) fn zip(
        &mut self,
        arena_mixop: &MixopArena,
        shape: Shape,
        typ_elem: Rc<TypKind>,
        typ_list: Rc<TypKind>,
        columns: &[Interned<ValueKind>],
    ) -> Result<Value, ValueError> {
        let note_elem = self.intern_typ(typ_elem)?;
        let note_list = self.intern_typ(typ_list)?;
        let query = ZipQuery { shape, note_elem, note_list, columns };
        if let Some(value) = self.zipper.zips.get(&query) {
            return Ok(*value);
        }
        let len = columns.first().map_or(0, |column| {
            let ValueKind::List(values) = self.kind(*column) else { unreachable!("list column") };
            values.len()
        });
        let span = Self::span_default();
        // Columns unzipped from a list zip back to that list's elements
        let origin = self.zipper.origins.get(columns).copied();
        if let Some(source) = origin {
            let ValueKind::List(elems) = self.kind(source) else {
                unreachable!("projection source")
            };
            let matches = elems.len() == len
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
            if matches {
                let node = if elems
                    .iter()
                    .all(|elem| elem.note == note_elem && elem.span == span)
                {
                    source
                } else {
                    let elems = elems
                        .iter()
                        .map(|elem| Value { node: elem.node, note: note_elem, span })
                        .collect();
                    self.intern_kind(arena_mixop, ValueKind::List(elems))?
                };
                let value = Value { node, note: note_list, span };
                self.zipper.zips.insert(
                    ZipKey { shape, note_elem, note_list, columns: columns.to_vec() },
                    value,
                );
                return Ok(value);
            }
        }
        let mut elems = Vec::with_capacity(len);
        for idx in 0..len {
            let values: Vec<_> = columns
                .iter()
                .map(|column| {
                    let ValueKind::List(values) = self.kind(*column) else {
                        unreachable!("list column")
                    };
                    values[idx]
                })
                .collect();
            let kind = match shape {
                Shape::Tuple => ValueKind::Tuple(values),
                Shape::List => ValueKind::List(values),
                Shape::Case(mixop) => ValueKind::Case(
                    crate::lang::data::notation::flat::Mixfix::new(arena_mixop, mixop, values)
                        .expect("prepared constructor arity"),
                ),
            };
            let node = self.intern_kind(arena_mixop, kind)?;
            elems.push(Value { node, note: note_elem, span });
        }
        let node = self.intern_kind(arena_mixop, ValueKind::List(elems))?;
        let value = Value { node, note: note_list, span };
        // Unzipping the new list yields the input columns
        let paths = (0..columns.len())
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
            .insert(UnzipKey { source: node, paths }, columns.to_vec());
        self.zipper
            .zips
            .insert(ZipKey { shape, note_elem, note_list, columns: columns.to_vec() }, value);
        Ok(value)
    }
}
