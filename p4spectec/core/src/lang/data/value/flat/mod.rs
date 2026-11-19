//! Flat values with interned bodies, types, and spans
//!
//! Child values refer to the same arena; cases hold a mixop handle.
//! Exact equality includes stored annotations, while canonical identity
//! compares value contents independently of their source locations.

use std::rc::Rc;

use crate::lang::{
    common::{
        Id,
        prim::num::Number,
        source::{NotePhrase, Span},
    },
    data::{
        intern::Interned,
        notation::{AtomPhrase, flat::Mixfix},
        typ::TypKind,
    },
};

use super::{ValueTag, host::HostPayload};

mod arena;
mod cmp;
pub mod get;
mod hash;
pub mod make;
mod print;
mod view;
pub mod zipper;

pub use arena::ValueArena;
pub use view::ValueRef;

// = Value forms

/// A value's body, type, and span handles in one arena.
pub type Value = NotePhrase<Interned<ValueKind>, Interned<Rc<TypKind>>, Interned<Span>>;

/// A named value field.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ValueField {
    pub atom: AtomPhrase,
    pub value: Value,
}

/// A case with one value per argument position.
pub type ValueCase = Mixfix<Value>;

/// A stored value body whose children belong to the same arena.
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum ValueKind {
    Bool(bool),
    Num(Number),
    Text(String),
    Struct(Vec<ValueField>),
    Case(ValueCase),
    Tuple(Vec<Value>),
    Opt(Option<Value>),
    List(Vec<Value>),
    Func(Id),
    Extern(Rc<HostPayload>),
}

// = Structural properties

impl ValueKind {
    /// The kind of this body.
    pub(crate) fn tag(&self) -> ValueTag {
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
