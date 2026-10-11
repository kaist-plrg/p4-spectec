//! Value bodies with full source locations and canonical syntax identities
//!
//! A `Value` is three interned handles: body, type, and span.
//! Bodies are stored exactly, spans included, so two values that print alike
//! may still be distinct entries;
//! the canonical identity ignores spans and is what syntax equality uses.

use std::{
    cmp::Ordering,
    hash::{Hash, Hasher},
    num::TryFromIntError,
    rc::Rc,
};

use serde_derive_state::{DeserializeState, SerializeState};
use thiserror::Error;

use crate::util::json::json;

use crate::lang::{
    common::{
        Id,
        notation::{atom::Atom, mixfix::Mixfix},
        prim::num::{self, Number},
        source::{NotePhrase, Phrase, Span},
    },
    data::{
        intern::{CanonEq, CanonHash, CanonInterner, Interned},
        typ::TypKind,
    },
    traits::{cmp::SyntaxCmp, eq::SyntaxEq},
};

use super::arena::ValueArena;

// = Value types

/// A value handle: interned body, type, and span, valid in one arena.
pub type Value = NotePhrase<Interned<ValueKind>, Interned<TypKind>, Interned<Span>>;
/// A struct field: atom and value.
pub type ValueField = (Phrase<Atom>, Value);
/// A variant case: a notation filled with values.
pub type ValueCase = Mixfix<Value>;

// - Borrowed views

/// A value together with its arena, for comparisons that must read bodies.
#[derive(Clone, Copy, Debug)]
pub struct ValueRef<'a> {
    pub(super) arena: &'a ValueArena,
    pub(super) value: Value,
}

// - Bodies

#[derive(Debug, SerializeState, DeserializeState)]
#[serde(serialize_state = "super::external::EncodeContext<'arena>", ser_parameters = "'arena")]
#[serde(deserialize_state = "super::external::DecodeContext<'de>")]
/// A value body; children are handles into the same arena.
pub enum ValueKind {
    /// A boolean.
    Bool(bool),
    /// A natural or integer.
    Num(Number),
    /// A text.
    Text(String),
    /// Named fields in declaration order.
    Struct(#[serde(state)] Vec<ValueField>),
    /// A variant case with its arguments.
    Case(#[serde(state)] ValueCase),
    /// A fixed-length tuple.
    Tuple(#[serde(state)] Vec<Value>),
    /// An optional value.
    Opt(#[serde(state)] Option<Value>),
    /// A list.
    List(#[serde(state)] Vec<Value>),
    /// A function, by name.
    Func(#[serde(state)] Id),
    /// A host-owned value, opaque to the specification.
    Extern(Rc<json>),
}

// - Tags

/// The kind of a value without its payload, for errors and ordering.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ValueTag {
    Bool,
    Num,
    Text,
    Struct,
    Case,
    Tuple,
    Opt,
    List,
    Func,
    Extern,
}

impl ValueKind {
    /// The kind of this body.
    pub(super) fn tag(&self) -> ValueTag {
        match self {
            Self::Bool(_) => ValueTag::Bool,
            Self::Num(_) => ValueTag::Num,
            Self::Text(_) => ValueTag::Text,
            Self::Struct(_) => ValueTag::Struct,
            Self::Case(_) => ValueTag::Case,
            Self::Tuple(_) => ValueTag::Tuple,
            Self::Opt(_) => ValueTag::Opt,
            Self::List(_) => ValueTag::List,
            Self::Func(_) => ValueTag::Func,
            Self::Extern(_) => ValueTag::Extern,
        }
    }
}

// = Exact body equality and hashing

// Mixfix's general Eq/Hash ignore atom spans; exact value storage retains them,
// so cases get their own span-sensitive equality and hash

impl PartialEq for ValueKind {
    fn eq(&self, kind_other: &Self) -> bool {
        /// Case equality including atom spans.
        fn eq_case(value_case_l: &ValueCase, value_case_r: &ValueCase) -> bool {
            match (value_case_l, value_case_r) {
                (Mixfix::Arg(value_l), Mixfix::Arg(value_r)) => value_l == value_r,
                (Mixfix::Atom(atom_l), Mixfix::Atom(atom_r)) => atom_l == atom_r,
                (
                    Mixfix::Brack(atom_l_l, value_case_l, atom_l_r),
                    Mixfix::Brack(atom_r_l, value_case_r, atom_r_r),
                ) => {
                    atom_l_l == atom_r_l
                        && eq_case(value_case_l, value_case_r)
                        && atom_l_r == atom_r_r
                }
                (
                    Mixfix::Infix(value_case_l_l, atom_l, value_case_l_r),
                    Mixfix::Infix(value_case_r_l, atom_r, value_case_r_r),
                ) => {
                    eq_case(value_case_l_l, value_case_r_l)
                        && atom_l == atom_r
                        && eq_case(value_case_l_r, value_case_r_r)
                }
                (Mixfix::Seq(value_cases_l), Mixfix::Seq(value_cases_r)) => {
                    value_cases_l.len() == value_cases_r.len()
                        && value_cases_l
                            .iter()
                            .zip(value_cases_r)
                            .all(|(value_case_l, value_case_r)| eq_case(value_case_l, value_case_r))
                }
                _ => false,
            }
        }

        match (self, kind_other) {
            (Self::Bool(value_l), Self::Bool(value_r)) => value_l == value_r,
            (Self::Num(value_l), Self::Num(value_r)) => value_l == value_r,
            (Self::Text(value_l), Self::Text(value_r)) => value_l == value_r,
            (Self::Struct(value_fields_l), Self::Struct(value_fields_r)) => {
                value_fields_l == value_fields_r
            }
            (Self::Case(value_case_l), Self::Case(value_case_r)) => {
                eq_case(value_case_l, value_case_r)
            }
            (Self::Tuple(values_l), Self::Tuple(values_r))
            | (Self::List(values_l), Self::List(values_r)) => values_l == values_r,
            (Self::Opt(value_l), Self::Opt(value_r)) => value_l == value_r,
            (Self::Func(id_l), Self::Func(id_r)) => id_l == id_r,
            (Self::Extern(json_l), Self::Extern(json_r)) => json_l == json_r,
            _ => false,
        }
    }
}

impl Eq for ValueKind {}

impl Hash for ValueKind {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        /// Case hash including atom spans.
        fn hash_case<H: Hasher>(value_case: &ValueCase, hasher: &mut H) {
            std::mem::discriminant(value_case).hash(hasher);
            match value_case {
                Mixfix::Arg(value) => value.hash(hasher),
                Mixfix::Atom(atom) => atom.hash(hasher),
                Mixfix::Brack(atom_l, value_case, atom_r) => {
                    atom_l.hash(hasher);
                    hash_case(value_case, hasher);
                    atom_r.hash(hasher);
                }
                Mixfix::Infix(value_case_l, atom, value_case_r) => {
                    hash_case(value_case_l, hasher);
                    atom.hash(hasher);
                    hash_case(value_case_r, hasher);
                }
                Mixfix::Seq(value_cases) => {
                    value_cases.len().hash(hasher);
                    for value_case in value_cases {
                        hash_case(value_case, hasher);
                    }
                }
            }
        }

        std::mem::discriminant(self).hash(hasher);
        match self {
            Self::Bool(value) => value.hash(hasher),
            Self::Num(value) => value.hash(hasher),
            Self::Text(value) => value.hash(hasher),
            Self::Struct(value_fields) => value_fields.hash(hasher),
            Self::Case(value_case) => hash_case(value_case, hasher),
            Self::Tuple(values) | Self::List(values) => values.hash(hasher),
            Self::Opt(value) => value.hash(hasher),
            Self::Func(id) => id.hash(hasher),
            Self::Extern(json) => json.hash(hasher),
        }
    }
}

// = Canonical equality and hashing

impl CanonEq for ValueKind {
    fn canon_eq(&self, interner: &CanonInterner<Self>, _: &(), kind_r: &Self) -> bool {
        // Children compare by canonical id, computed when they were interned
        let eq_value = |value_l: &Value, value_r: &Value| {
            interner.canon_id(value_l.node) == interner.canon_id(value_r.node)
        };
        match (self, kind_r) {
            (ValueKind::Bool(value_l), ValueKind::Bool(value_r)) => value_l == value_r,
            (ValueKind::Num(value_l), ValueKind::Num(value_r)) => value_l == value_r,
            (ValueKind::Text(value_l), ValueKind::Text(value_r)) => value_l == value_r,
            (ValueKind::Struct(value_fields_l), ValueKind::Struct(value_fields_r)) => {
                value_fields_l.len() == value_fields_r.len()
                    && value_fields_l.iter().zip(value_fields_r).all(
                        |((atom_l, value_l), (atom_r, value_r))| {
                            atom_l.node == atom_r.node && eq_value(value_l, value_r)
                        },
                    )
            }
            (ValueKind::Case(value_case_l), ValueKind::Case(value_case_r)) => {
                value_case_l.eq_by(value_case_r, eq_value)
            }
            (ValueKind::Tuple(values_l), ValueKind::Tuple(values_r))
            | (ValueKind::List(values_l), ValueKind::List(values_r)) => {
                values_l.len() == values_r.len()
                    && values_l
                        .iter()
                        .zip(values_r)
                        .all(|(value_l, value_r)| eq_value(value_l, value_r))
            }
            (ValueKind::Opt(value_l), ValueKind::Opt(value_r)) => match (value_l, value_r) {
                (Some(value_l), Some(value_r)) => eq_value(value_l, value_r),
                (None, None) => true,
                _ => false,
            },
            (ValueKind::Func(id_l), ValueKind::Func(id_r)) => id_l.node == id_r.node,
            (ValueKind::Extern(json_l), ValueKind::Extern(json_r)) => json_l == json_r,
            _ => false,
        }
    }
}

impl CanonHash for ValueKind {
    fn canon_hash<H: Hasher>(&self, interner: &CanonInterner<Self>, _: &(), hasher: &mut H) {
        /// Case hash over atom names and children's canonical ids.
        fn canon_hash_case<H: Hasher>(
            value_case: &ValueCase,
            interner: &CanonInterner<ValueKind>,
            hasher: &mut H,
        ) {
            std::mem::discriminant(value_case).hash(hasher);
            match value_case {
                Mixfix::Arg(value) => interner.canon_id(value.node).hash(hasher),
                Mixfix::Atom(atom) => atom.node.hash(hasher),
                Mixfix::Brack(atom_l, value_case, atom_r) => {
                    atom_l.node.hash(hasher);
                    canon_hash_case(value_case, interner, hasher);
                    atom_r.node.hash(hasher);
                }
                Mixfix::Infix(value_case_l, atom, value_case_r) => {
                    canon_hash_case(value_case_l, interner, hasher);
                    atom.node.hash(hasher);
                    canon_hash_case(value_case_r, interner, hasher);
                }
                Mixfix::Seq(value_cases) => {
                    value_cases.len().hash(hasher);
                    for value_case in value_cases {
                        canon_hash_case(value_case, interner, hasher);
                    }
                }
            }
        }

        std::mem::discriminant(self).hash(hasher);
        match self {
            ValueKind::Bool(value) => value.hash(hasher),
            ValueKind::Num(value) => value.hash(hasher),
            ValueKind::Text(value) => value.hash(hasher),
            ValueKind::Struct(value_fields) => {
                value_fields.len().hash(hasher);
                for (atom, value) in value_fields {
                    atom.node.hash(hasher);
                    interner.canon_id(value.node).hash(hasher);
                }
            }
            ValueKind::Case(value_case) => canon_hash_case(value_case, interner, hasher),
            ValueKind::Tuple(values) | ValueKind::List(values) => {
                values.len().hash(hasher);
                for value in values {
                    interner.canon_id(value.node).hash(hasher);
                }
            }
            ValueKind::Opt(value) => value
                .map(|value| interner.canon_id(value.node))
                .hash(hasher),
            ValueKind::Func(id) => id.node.hash(hasher),
            ValueKind::Extern(json) => json.hash(hasher),
        }
    }
}

// = Syntax comparison

impl SyntaxEq for ValueRef<'_> {
    fn syntax_eq(&self, value_other: &Self) -> bool {
        // Same arena: canonical ids decide; otherwise compare structurally
        if std::ptr::eq(self.arena, value_other.arena) {
            self.arena.canon_id(&self.value) == value_other.arena.canon_id(&value_other.value)
        } else {
            self.syntax_cmp(value_other).is_eq()
        }
    }
}

impl SyntaxCmp for ValueRef<'_> {
    fn syntax_cmp(&self, value_other: &Self) -> Ordering {
        // Children are compared through their own arenas
        let compare_value = |value_l: &Value, value_r: &Value| {
            self.arena
                .view(*value_l)
                .syntax_cmp(&value_other.arena.view(*value_r))
        };
        let compare_values = |values_l: &[Value], values_r: &[Value]| {
            values_l
                .iter()
                .zip(values_r)
                .map(|(value_l, value_r)| compare_value(value_l, value_r))
                .find(|order| !order.is_eq())
                .unwrap_or_else(|| values_l.len().cmp(&values_r.len()))
        };
        let kind_l = self.arena.kind(&self.value);
        let kind_r = value_other.arena.kind(&value_other.value);
        match (kind_l, kind_r) {
            (ValueKind::Bool(value_l), ValueKind::Bool(value_r)) => value_l.cmp(value_r),
            (ValueKind::Num(value_l), ValueKind::Num(value_r)) => num::compare(value_l, value_r),
            (ValueKind::Text(value_l), ValueKind::Text(value_r)) => value_l.cmp(value_r),
            (ValueKind::Struct(value_fields_l), ValueKind::Struct(value_fields_r)) => {
                value_fields_l
                    .iter()
                    .zip(value_fields_r)
                    .map(|((atom_l, value_l), (atom_r, value_r))| {
                        atom_l
                            .node
                            .cmp(&atom_r.node)
                            .then_with(|| compare_value(value_l, value_r))
                    })
                    .find(|order| !order.is_eq())
                    .unwrap_or_else(|| value_fields_l.len().cmp(&value_fields_r.len()))
            }
            (ValueKind::Case(value_case_l), ValueKind::Case(value_case_r)) => {
                value_case_l.cmp_by(value_case_r, compare_value)
            }
            (ValueKind::Tuple(values_l), ValueKind::Tuple(values_r))
            | (ValueKind::List(values_l), ValueKind::List(values_r)) => {
                compare_values(values_l, values_r)
            }
            (ValueKind::Opt(value_l), ValueKind::Opt(value_r)) => match (value_l, value_r) {
                (Some(value_l), Some(value_r)) => compare_value(value_l, value_r),
                (None, Some(_)) => Ordering::Less,
                (Some(_), None) => Ordering::Greater,
                (None, None) => Ordering::Equal,
            },
            (ValueKind::Func(id_l), ValueKind::Func(id_r)) => id_l.node.cmp(&id_r.node),
            (ValueKind::Extern(json_l), ValueKind::Extern(json_r)) => {
                crate::util::json::compare(json_l, json_r)
            }
            // Different kinds order by tag
            _ => kind_l.tag().cmp(&kind_r.tag()),
        }
    }
}

// = Errors

/// A failure building or projecting a value.
#[derive(Clone, Debug, Error, Eq, PartialEq)]
pub enum ValueError {
    /// The arena ran out of 32-bit handles.
    #[error("value arena index overflow")]
    IndexOverflow,
    /// A projection met a value of another kind.
    #[error("expected {expected:?} value, got {actual:?}")]
    KindMismatch { expected: ValueTag, actual: ValueTag },
    /// An element index past the end.
    #[error("value index {index} is out of bounds for length {len}")]
    IndexOutOfBounds { index: usize, len: usize },
    /// A fixed-arity projection met another count.
    #[error("expected exactly {expected} values, got {actual}")]
    CountMismatch { expected: usize, actual: usize },
}

// - Index overflow

impl From<TryFromIntError> for ValueError {
    fn from(_: TryFromIntError) -> Self {
        Self::IndexOverflow
    }
}
