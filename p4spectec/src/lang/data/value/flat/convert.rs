//! Expanding flat values into owned trees
//!
//! `into_tree` copies bodies, types, and spans out of the arena.
//! Case expansion fills each notation position with its corresponding value.
//! Extern payloads encode arena-independently through their definition's codec.

use std::slice;

use crate::lang::data::{arena::Arena, encoding::Encoding, notation};

use super::super::{ValueError, tree};
use super::external::EncodeContext;
use super::{Value, ValueCase, ValueField, ValueKind};

// = Values

impl Value {
    /// Copies an arena value into a tree, including its type and source span.
    ///
    /// Fails on an extern value whose definition has no codec.
    pub fn into_tree(self, arena: &Arena) -> Result<tree::Value, ValueError> {
        Ok(tree::Value {
            node: arena.value.kind(self.node).into_tree(arena)?,
            note: arena.value.typ(self.note).as_ref().clone(),
            span: *arena.value.span(self.span),
        })
    }
}

// = Bodies

impl ValueKind {
    /// Expands child handles and case mixops into trees.
    pub(super) fn into_tree(&self, arena: &Arena) -> Result<tree::ValueKind, ValueError> {
        Ok(match self {
            Self::Bool(value) => tree::ValueKind::Bool(*value),
            Self::Num(num) => tree::ValueKind::Num(num.clone()),
            Self::Text(text) => tree::ValueKind::Text(text.clone()),
            Self::Struct(value_fields) => tree::ValueKind::Struct(
                value_fields
                    .iter()
                    .map(|ValueField { atom, value }| {
                        Ok(tree::ValueField { atom: atom.clone(), value: value.into_tree(arena)? })
                    })
                    .collect::<Result<_, ValueError>>()?,
            ),
            Self::Case(value_case) => tree::ValueKind::Case(value_case.into_tree(arena)?),
            Self::Tuple(values) => tree::ValueKind::Tuple(
                values
                    .iter()
                    .map(|value| value.into_tree(arena))
                    .collect::<Result<_, _>>()?,
            ),
            Self::Opt(value) => tree::ValueKind::Opt(
                value
                    .as_ref()
                    .map(|value| value.into_tree(arena).map(Box::new))
                    .transpose()?,
            ),
            Self::List(values) => tree::ValueKind::List(
                values
                    .iter()
                    .map(|value| value.into_tree(arena))
                    .collect::<Result<_, _>>()?,
            ),
            Self::Func(id) => tree::ValueKind::Func(id.clone()),
            // Only the definition's codec can copy the payload out of the arena
            Self::Extern(value_extern) => tree::ValueKind::Extern(
                value_extern.encode(&EncodeContext::new(arena, Encoding::ArenaIndependent))?,
            ),
        })
    }
}

// = Cases

impl ValueCase {
    /// Expands a case and its arguments in notation order.
    fn into_tree(&self, arena: &Arena) -> Result<tree::ValueCase, ValueError> {
        Self::into_tree_inner(arena, *self.mixop(), &mut self.args().iter())
    }

    /// Resolves each argument as the mixop traversal reaches its position.
    fn into_tree_inner(
        arena: &Arena,
        mixop: notation::flat::Mixop,
        values: &mut slice::Iter<'_, Value>,
    ) -> Result<tree::ValueCase, ValueError> {
        Ok(match arena.mixop.kind(mixop) {
            notation::flat::MixopKind::Arg => tree::ValueCase::Arg(Box::new(
                values
                    .next()
                    .expect("a case fills every position")
                    .into_tree(arena)?,
            )),
            notation::flat::MixopKind::Atom(atom) => tree::ValueCase::Atom(atom.clone()),
            notation::flat::MixopKind::Brack(atom_l, mixop, atom_r) => tree::ValueCase::Brack(
                atom_l.clone(),
                Box::new(Self::into_tree_inner(arena, *mixop, values)?),
                atom_r.clone(),
            ),
            notation::flat::MixopKind::Infix(mixop_l, atom, mixop_r) => {
                let value_case_l = Self::into_tree_inner(arena, *mixop_l, values)?;
                let value_case_r = Self::into_tree_inner(arena, *mixop_r, values)?;
                tree::ValueCase::Infix(Box::new(value_case_l), atom.clone(), Box::new(value_case_r))
            }
            notation::flat::MixopKind::Seq(mixops) => tree::ValueCase::Seq(
                mixops
                    .iter()
                    .map(|mixop| Self::into_tree_inner(arena, *mixop, values))
                    .collect::<Result<_, _>>()?,
            ),
        })
    }
}
