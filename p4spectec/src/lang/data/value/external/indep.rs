//! Arena-independent value trees
//!
//! A tree contains its child values and annotations, without arena handles,
//! so it can be written to JSON and read into any arena.

use std::rc::Rc;

use serde::{Deserialize, Serialize};

use crate::util::json::json;

use crate::lang::{
    common::{
        Id,
        notation::{atom::Atom, mixfix::Mixfix},
        prim::num::Number,
        source::{NotePhrase, Phrase},
    },
    data::typ::TypKind,
};

use super::super::{Value as ArenaValue, ValueArena, ValueError, ValueKind as ArenaValueKind};

// == Types

/// A value tree with its type; children are trees, not handles.
pub type Value = NotePhrase<ValueKind, TypKind>;

/// A value body whose children are trees.
#[derive(Debug, Serialize, Deserialize)]
pub enum ValueKind {
    Bool(bool),
    Num(Number),
    Text(String),
    Struct(Vec<(Phrase<Atom>, Value)>),
    Case(Mixfix<Box<Value>>),
    Tuple(Vec<Value>),
    Opt(Option<Box<Value>>),
    List(Vec<Value>),
    Func(Id),
    Extern(json),
}

// == Arena conversion

/// Copies an arena value into a tree, including its type and source span.
pub fn from_arena(arena: &ValueArena, value: &ArenaValue) -> Value {
    Value {
        node: ValueKind::from_arena(arena, arena.kind(value)),
        note: arena.typ(value).as_ref().clone(),
        span: arena.span(value).clone(),
    }
}

/// Interns the tree in the target arena, preserving its type and source span.
pub fn into_arena(arena: &mut ValueArena, value: Value) -> Result<ArenaValue, ValueError> {
    let kind = value.node.into_arena(arena)?;
    arena.alloc(kind, value.note.into(), value.span)
}

impl ValueKind {
    /// Expands child handles into values and copies extern JSON unchanged.
    pub(super) fn from_arena(arena: &ValueArena, kind: &ArenaValueKind) -> Self {
        match kind {
            ArenaValueKind::Bool(value) => Self::Bool(*value),
            ArenaValueKind::Num(num) => Self::Num(num.clone()),
            ArenaValueKind::Text(text) => Self::Text(text.clone()),
            ArenaValueKind::Struct(fields) => Self::Struct(
                fields
                    .iter()
                    .map(|(atom, value)| (atom.clone(), from_arena(arena, value)))
                    .collect(),
            ),
            ArenaValueKind::Case(mixfix) => {
                Self::Case(mixfix.map(|value| Box::new(from_arena(arena, value))))
            }
            ArenaValueKind::Tuple(values) => Self::Tuple(
                values
                    .iter()
                    .map(|value| from_arena(arena, value))
                    .collect(),
            ),
            ArenaValueKind::Opt(value) => Self::Opt(
                value
                    .as_ref()
                    .map(|value| Box::new(from_arena(arena, value))),
            ),
            ArenaValueKind::List(values) => Self::List(
                values
                    .iter()
                    .map(|value| from_arena(arena, value))
                    .collect(),
            ),
            ArenaValueKind::Func(id) => Self::Func(id.clone()),
            ArenaValueKind::Extern(json) => Self::Extern(json.as_ref().clone()),
        }
    }

    /// Converts child values to arena handles and keeps extern JSON unchanged.
    pub(super) fn into_arena(self, arena: &mut ValueArena) -> Result<ArenaValueKind, ValueError> {
        Ok(match self {
            Self::Bool(value) => ArenaValueKind::Bool(value),
            Self::Num(num) => ArenaValueKind::Num(num),
            Self::Text(text) => ArenaValueKind::Text(text),
            Self::Struct(fields) => ArenaValueKind::Struct(
                fields
                    .into_iter()
                    .map(|(atom, value)| Ok((atom, into_arena(arena, value)?)))
                    .collect::<Result<_, ValueError>>()?,
            ),
            Self::Case(mixfix) => ArenaValueKind::Case(Self::into_arena_case(arena, mixfix)?),
            Self::Tuple(values) => ArenaValueKind::Tuple(
                values
                    .into_iter()
                    .map(|value| into_arena(arena, value))
                    .collect::<Result<_, _>>()?,
            ),
            Self::Opt(value) => {
                ArenaValueKind::Opt(value.map(|value| into_arena(arena, *value)).transpose()?)
            }
            Self::List(values) => ArenaValueKind::List(
                values
                    .into_iter()
                    .map(|value| into_arena(arena, value))
                    .collect::<Result<_, _>>()?,
            ),
            Self::Func(id) => ArenaValueKind::Func(id),
            Self::Extern(json) => ArenaValueKind::Extern(Rc::new(json)),
        })
    }

    /// Converts case arguments to arena values, preserving atoms and brackets.
    fn into_arena_case(
        arena: &mut ValueArena,
        mixfix: Mixfix<Box<Value>>,
    ) -> Result<Mixfix<ArenaValue>, ValueError> {
        Ok(match mixfix {
            Mixfix::Arg(value) => Mixfix::Arg(into_arena(arena, *value)?),
            Mixfix::Atom(atom) => Mixfix::Atom(atom),
            Mixfix::Brack(atom_l, mixfix, atom_r) => {
                Mixfix::Brack(atom_l, Box::new(Self::into_arena_case(arena, *mixfix)?), atom_r)
            }
            Mixfix::Infix(mixfix_l, atom, mixfix_r) => Mixfix::Infix(
                Box::new(Self::into_arena_case(arena, *mixfix_l)?),
                atom,
                Box::new(Self::into_arena_case(arena, *mixfix_r)?),
            ),
            Mixfix::Seq(mixfixes) => Mixfix::Seq(
                mixfixes
                    .into_iter()
                    .map(|mixfix| Self::into_arena_case(arena, mixfix))
                    .collect::<Result<_, _>>()?,
            ),
        })
    }
}
