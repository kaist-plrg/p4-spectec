//! Flat values with interned bodies, types, and spans
//!
//! Child values refer to the same arena; cases hold a mixop handle.
//! Exact equality includes stored annotations, while canonical identity
//! compares value contents independently of their source locations.

use std::rc::Rc;

use serde_derive_state::{DeserializeState, SerializeState};

use crate::util::json::json;

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

use super::ValueTag;

use self::external::{DecodeContext, EncodeContext};

mod arena;
mod cmp;
mod convert;
pub mod external;
pub mod get;
mod hash;
pub mod make;
mod print;
mod view;

pub use arena::ValueArena;
pub use view::ValueRef;

// = Value forms

/// A value's body, type, and span handles in one arena.
pub type Value = NotePhrase<Interned<ValueKind>, Interned<TypKind>, Interned<Span>>;

/// A named value field.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ValueField {
    pub atom: AtomPhrase,
    pub value: Value,
}

/// A case with one value per argument position.
pub type ValueCase = Mixfix<Value>;

/// A stored value body whose children belong to the same arena.
#[derive(Debug, PartialEq, Eq, Hash, SerializeState, DeserializeState)]
#[serde(rename = "ValueKind")]
#[serde(serialize_state = "EncodeContext<'arena>", ser_parameters = "'arena")]
#[serde(deserialize_state = "DecodeContext<'de>")]
pub enum ValueKind {
    Bool(bool),
    Num(Number),
    Text(String),
    Struct(#[serde(state)] Vec<ValueField>),
    Case(#[serde(state)] ValueCase),
    Tuple(#[serde(state)] Vec<Value>),
    Opt(#[serde(state)] Option<Value>),
    List(#[serde(state)] Vec<Value>),
    Func(#[serde(state)] Id),
    Extern(Rc<json>),
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
