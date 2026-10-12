//! Types shared by the internal language and its successors
//!
//! `TypKind` is the type language after elaboration:
//! primitives, named types with arguments, tuples, iterations, and functions.
//! `make` builds types with default spans; `SyntaxCmp` orders them by shape.

use std::{
    cmp::Ordering,
    fmt::{self, Write},
};

use serde::{Deserialize, Serialize};

use crate::lang::{
    common::{
        Id, Iter, TId,
        prim::num,
        source::{Phrase, Span},
    },
    traits::{
        cmp::SyntaxCmp,
        print::{Print, Printer},
    },
};

use crate::phrase;

// == Types

/// A type with its span.
pub type Typ = Phrase<TypKind>;

/// The forms of a type.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum TypKind {
    /// `bool`
    Bool,
    /// `numtyp`
    Num(num::Typ),
    /// `text`
    Text,
    /// `id (`<` list(targ, `,`) `>`)?`
    Var(Id, Vec<Typ>),
    /// `(` list(typ, `,`) `)`
    Tuple(Vec<Typ>),
    /// `typ iter`
    Iter(Box<Typ>, Iter),
    /// `<` list(tparam, `,`) `>` `(` list(typ, `,`) `)` `:` typ.
    Func(FuncTyp),
}

/// The type of a function: type parameters, parameter types, result type.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct FuncTyp {
    pub tparams: Vec<TId>,
    pub typs_params: Vec<Typ>,
    pub typ_ret: Box<Typ>,
}

// == Comparison

/// Variant order for comparing types of different shapes.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum TypTag {
    Bool,
    Num,
    Text,
    Var,
    Tuple,
    Iter,
    Func,
}

impl TypKind {
    /// The variant of this type.
    fn tag(&self) -> TypTag {
        match self {
            Self::Bool => TypTag::Bool,
            Self::Num(_) => TypTag::Num,
            Self::Text => TypTag::Text,
            Self::Var(_, _) => TypTag::Var,
            Self::Tuple(_) => TypTag::Tuple,
            Self::Iter(_, _) => TypTag::Iter,
            Self::Func(_) => TypTag::Func,
        }
    }
}

impl SyntaxCmp for TypKind {
    fn syntax_cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Self::Bool, Self::Bool) | (Self::Text, Self::Text) => Ordering::Equal,
            (Self::Num(num_typ_l), Self::Num(num_typ_r)) => {
                num::compare_typ(*num_typ_l, *num_typ_r)
            }
            (Self::Var(id_l, targs_l), Self::Var(id_r, targs_r)) => id_l
                .syntax_cmp(id_r)
                .then_with(|| targs_l.as_slice().syntax_cmp(targs_r)),
            (Self::Tuple(typs_l), Self::Tuple(typs_r)) => typs_l.as_slice().syntax_cmp(typs_r),
            (Self::Iter(typ_l, iter_l), Self::Iter(typ_r, iter_r)) => typ_l
                .syntax_cmp(typ_r)
                .then_with(|| iter_l.syntax_cmp(iter_r)),
            (Self::Func(func_typ_l), Self::Func(func_typ_r)) => func_typ_l.syntax_cmp(func_typ_r),
            // Different shapes order by variant
            _ => self.tag().cmp(&other.tag()),
        }
    }
}

impl SyntaxCmp for FuncTyp {
    fn syntax_cmp(&self, other: &Self) -> Ordering {
        self.tparams
            .as_slice()
            .syntax_cmp(&other.tparams)
            .then_with(|| self.typs_params.as_slice().syntax_cmp(&other.typs_params))
            .then_with(|| self.typ_ret.syntax_cmp(&other.typ_ret))
    }
}

// == Smart constructors

/// Constructors for types with default spans.
pub mod make {
    use super::*;

    /// Wraps a type in each iterator from innermost to outermost.
    pub fn iterate(mut typ: Typ, iters: &[Iter]) -> Typ {
        for iter in iters {
            let span = typ.span.clone();
            let typ_inner = Box::new(typ);
            let typ_kind = TypKind::Iter(typ_inner, *iter);
            typ = phrase!(node: typ_kind, span: span);
        }
        typ
    }

    /// `bool`.
    pub fn bool() -> Typ {
        let typ_kind = TypKind::Bool;
        phrase!(node: typ_kind, span: Span::default())
    }

    /// `nat`.
    pub fn nat() -> Typ {
        let num_typ = num::Typ::Nat;
        num(num_typ)
    }

    /// `int`.
    pub fn int() -> Typ {
        let num_typ = num::Typ::Int;
        num(num_typ)
    }

    /// A numeric type.
    pub fn num(num_typ: num::Typ) -> Typ {
        let typ_kind = TypKind::Num(num_typ);
        phrase!(node: typ_kind, span: Span::default())
    }

    /// `text`.
    pub fn text() -> Typ {
        let typ_kind = TypKind::Text;
        phrase!(node: typ_kind, span: Span::default())
    }

    /// A named type with arguments.
    pub fn var(id: Id, targs: Vec<Typ>) -> Typ {
        let typ_kind = TypKind::Var(id, targs);
        phrase!(node: typ_kind, span: Span::default())
    }

    /// A tuple type.
    pub fn tuple(typs: Vec<Typ>) -> Typ {
        let typ_kind = TypKind::Tuple(typs);
        phrase!(node: typ_kind, span: Span::default())
    }

    /// One iteration over a type.
    pub fn iter(typ: Typ, iter: Iter) -> Typ {
        let typ_inner = Box::new(typ);
        let typ_kind = TypKind::Iter(typ_inner, iter);
        phrase!(node: typ_kind, span: Span::default())
    }

    /// `typ?`.
    pub fn opt(typ: Typ) -> Typ {
        let iter = Iter::Opt;
        self::iter(typ, iter)
    }

    /// `typ*`.
    pub fn list(typ: Typ) -> Typ {
        let iter = Iter::List;
        self::iter(typ, iter)
    }

    /// A function type.
    pub fn func(tparams: Vec<TId>, typs_params: Vec<Typ>, typ_ret: Typ) -> Typ {
        let typ_ret = Box::new(typ_ret);
        let func_typ = FuncTyp { tparams, typs_params, typ_ret };
        let typ_kind = TypKind::Func(func_typ);
        phrase!(node: typ_kind, span: Span::default())
    }
}

// == Serialization

// - Encode

// Ordinary serde needs extra stack space for deeply nested types
impl<State> serde_state::SerializeState<State> for TypKind {
    fn serialize_state<Serializer>(
        &self,
        serializer: Serializer,
        _state: &State,
    ) -> Result<Serializer::Ok, Serializer::Error>
    where
        Serializer: serde::Serializer,
    {
        stacker::grow(32 * 1024 * 1024, || self.serialize(serializer))
    }
}

impl<State> serde_state::SerializeState<State> for FuncTyp {
    fn serialize_state<Serializer>(
        &self,
        serializer: Serializer,
        _state: &State,
    ) -> Result<Serializer::Ok, Serializer::Error>
    where
        Serializer: serde::Serializer,
    {
        stacker::grow(32 * 1024 * 1024, || self.serialize(serializer))
    }
}

// - Decode

impl<'de, State> serde_state::DeserializeState<'de, State> for TypKind {
    fn deserialize_state<Deserializer>(
        _state: &mut State,
        deserializer: Deserializer,
    ) -> Result<Self, Deserializer::Error>
    where
        Deserializer: serde::Deserializer<'de>,
    {
        Self::deserialize(deserializer)
    }
}

impl<'de, State> serde_state::DeserializeState<'de, State> for FuncTyp {
    fn deserialize_state<Deserializer>(
        _state: &mut State,
        deserializer: Deserializer,
    ) -> Result<Self, Deserializer::Error>
    where
        Deserializer: serde::Deserializer<'de>,
    {
        Self::deserialize(deserializer)
    }
}

// == Printing

impl Print for Typ {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        match &self.node {
            TypKind::Bool => printer.write_str("bool"),
            TypKind::Num(num::Typ::Nat) => printer.write_str("nat"),
            TypKind::Num(num::Typ::Int) => printer.write_str("int"),
            TypKind::Text => printer.write_str("text"),
            TypKind::Var(id, targs) => {
                id.print(printer)?;
                if !targs.is_empty() {
                    printer.write_char('<')?;
                    printer.separated(targs, ", ")?;
                    printer.write_char('>')?;
                }
                Ok(())
            }
            TypKind::Tuple(typs) => {
                printer.write_char('(')?;
                printer.separated(typs, ", ")?;
                printer.write_char(')')
            }
            TypKind::Iter(typ, iter) => {
                typ.print(printer)?;
                iter.print(printer)
            }
            TypKind::Func(func_typ) => {
                if !func_typ.tparams.is_empty() {
                    printer.write_char('<')?;
                    printer.separated(&func_typ.tparams, ", ")?;
                    printer.write_char('>')?;
                }
                printer.write_char('(')?;
                printer.separated(&func_typ.typs_params, ", ")?;
                printer.write_str(") : ")?;
                func_typ.typ_ret.print(printer)
            }
        }
    }
}
