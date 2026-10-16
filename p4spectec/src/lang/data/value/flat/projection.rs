//! Reuse of exact structural projections from immutable list rows
//!
//! A projection depends only on its source body, child paths and output types.
//! Cached columns retain every child's value, type and source-location handle.

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
struct Key {
    source: Interned<ValueKind>,
    paths: Vec<Vec<Step>>,
}

#[derive(Hash)]
struct Query<'a> {
    source: Interned<ValueKind>,
    paths: &'a [&'a [Step]],
}

impl Equivalent<Key> for Query<'_> {
    fn equivalent(&self, key: &Key) -> bool {
        self.source == key.source
            && self.paths.len() == key.paths.len()
            && self
                .paths
                .iter()
                .zip(&key.paths)
                .all(|(path, stored)| *path == stored.as_slice())
    }
}

/// Columns derived from bodies in the owning value arena.
#[derive(Debug, Default)]
pub(super) struct Columns {
    entries: HashMap<Key, Vec<Interned<ValueKind>>, RandomState>,
    origins: HashMap<Vec<Interned<ValueKind>>, Interned<ValueKind>, RandomState>,
    maps: HashMap<MapKey, Value, RandomState>,
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
    pub(crate) fn project_rows(
        &mut self,
        arena_mixop: &MixopArena,
        source: Interned<ValueKind>,
        paths: &[&[Step]],
        typs: impl Iterator<Item = Rc<TypKind>>,
    ) -> Result<Vec<Value>, ValueError> {
        let types: Vec<_> = typs
            .map(|typ| self.intern_typ(typ))
            .collect::<Result<_, _>>()?;
        if let Some(values) = self.columns.entries.get(&Query { source, paths }) {
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
            self.columns.origins.insert(nodes.clone(), source);
        }
        self.columns
            .entries
            .insert(Key { source, paths: paths.iter().map(|path| path.to_vec()).collect() }, nodes);
        Ok(values)
    }
}

/// A fixed row constructor whose arguments are selected input columns.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum Shape {
    Tuple,
    List,
    Case(crate::lang::data::notation::flat::Mixop),
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct MapKey {
    shape: Shape,
    note_row: Interned<Rc<TypKind>>,
    note_list: Interned<Rc<TypKind>>,
    columns: Vec<Interned<ValueKind>>,
}

#[derive(Hash)]
struct MapQuery<'a> {
    shape: Shape,
    note_row: Interned<Rc<TypKind>>,
    note_list: Interned<Rc<TypKind>>,
    columns: &'a [Interned<ValueKind>],
}

impl Equivalent<MapKey> for MapQuery<'_> {
    fn equivalent(&self, key: &MapKey) -> bool {
        self.shape == key.shape
            && self.note_row == key.note_row
            && self.note_list == key.note_list
            && self.columns == key.columns
    }
}

impl ValueArena {
    /// Rebuilds rows from exact columns without per-row interpreter contexts.
    pub(crate) fn map_columns(
        &mut self,
        arena_mixop: &MixopArena,
        shape: Shape,
        typ_row: Rc<TypKind>,
        typ_list: Rc<TypKind>,
        columns: &[Interned<ValueKind>],
    ) -> Result<Value, ValueError> {
        let note_row = self.intern_typ(typ_row)?;
        let note_list = self.intern_typ(typ_list)?;
        let query = MapQuery { shape, note_row, note_list, columns };
        if let Some(value) = self.columns.maps.get(&query) {
            return Ok(*value);
        }
        let len = columns.first().map_or(0, |column| {
            let ValueKind::List(values) = self.kind(*column) else { unreachable!("list column") };
            values.len()
        });
        let span = Self::span_default();
        // An inverse projection can reuse each original row body exactly
        let origin = self.columns.origins.get(columns).copied();
        if let Some(source) = origin {
            let ValueKind::List(rows) = self.kind(source) else {
                unreachable!("projection source")
            };
            let matches = rows.len() == len
                && rows.iter().all(|row| match (shape, self.kind(row.node)) {
                    (Shape::Tuple, ValueKind::Tuple(values))
                    | (Shape::List, ValueKind::List(values)) => values.len() == columns.len(),
                    (Shape::Case(mixop), ValueKind::Case(value_case)) => {
                        *value_case.mixop() == mixop && value_case.args().len() == columns.len()
                    }
                    _ => false,
                });
            if matches {
                let node = if rows
                    .iter()
                    .all(|row| row.note == note_row && row.span == span)
                {
                    source
                } else {
                    let rows = rows
                        .iter()
                        .map(|row| Value { node: row.node, note: note_row, span })
                        .collect();
                    self.intern_kind(arena_mixop, ValueKind::List(rows))?
                };
                let value = Value { node, note: note_list, span };
                self.columns.maps.insert(
                    MapKey { shape, note_row, note_list, columns: columns.to_vec() },
                    value,
                );
                return Ok(value);
            }
        }
        let mut rows = Vec::with_capacity(len);
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
            rows.push(Value { node, note: note_row, span });
        }
        let node = self.intern_kind(arena_mixop, ValueKind::List(rows))?;
        let value = Value { node, note: note_list, span };
        // Decomposing these freshly constructed rows yields the input columns
        let paths = (0..columns.len())
            .map(|idx| {
                vec![match shape {
                    Shape::Tuple => Step::Tuple(idx),
                    Shape::List => Step::List(idx),
                    Shape::Case(_) => Step::Case(idx),
                }]
            })
            .collect();
        self.columns
            .entries
            .insert(Key { source: node, paths }, columns.to_vec());
        self.columns
            .maps
            .insert(MapKey { shape, note_row, note_list, columns: columns.to_vec() }, value);
        Ok(value)
    }
}
