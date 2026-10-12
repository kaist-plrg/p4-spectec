//! Tree representation of values
//!
//! `Value` owns its body, type, and source span.
//! Case bodies contain filled notation whose argument leaves own values.
//! Ordinary serde reads and writes these contents without an arena.

use std::rc::Rc;

use serde::{Deserialize, Serialize};

use crate::util::json::json;

use crate::lang::{
    common::{Id, prim::num::Number, source::NotePhrase},
    data::{notation::AtomPhrase, typ::TypKind},
};

mod convert;
mod external;
pub mod get;

// = Value forms

/// An owned value with its type and source span.
pub type Value = NotePhrase<ValueKind, TypKind>;

/// A named value field.
#[derive(Debug)]
pub struct ValueField {
    pub atom: AtomPhrase,
    pub value: Value,
}

/// A filled notation containing its argument values.
#[derive(Debug, Serialize, Deserialize)]
#[serde(rename = "Mixfix")]
pub enum ValueCase {
    Arg(Box<Value>),
    Atom(AtomPhrase),
    Brack(AtomPhrase, Box<ValueCase>, AtomPhrase),
    Infix(Box<ValueCase>, AtomPhrase, Box<ValueCase>),
    Seq(Vec<ValueCase>),
}

/// A value body containing its children directly.
#[derive(Debug, Serialize, Deserialize)]
#[serde(rename = "ValueKind")]
pub enum ValueKind {
    Bool(bool),
    Num(Number),
    Text(String),
    Struct(Vec<ValueField>),
    Case(ValueCase),
    Tuple(Vec<Value>),
    Opt(Option<Box<Value>>),
    List(Vec<Value>),
    Func(Id),
    Extern(Rc<json>),
}
