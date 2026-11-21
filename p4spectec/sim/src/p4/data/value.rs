//! P4 values as the simulator sees them
//!
//! `P4Value` is an owned Rust value for each P4 value form,
//! named after the P4 type it belongs to (`bit<W>` is `Bit`, `header` is `Header`).
//! Code builds and inspects it as an AST:
//! `P4Value::Bit { width: 9, int: 511.into() }`, or a `match` on its variants.
//! A value the simulator does not take apart,
//! such as an object reference or a set, stays `Opaque`.
//! `spec::bridge` converts between `P4Value` and the specification's values.
//!
//! An argument whose P4 type the extern prototype fixes, such as `in bit<32> index`,
//! converts to a Rust `usize`, `BigInt`, `bool`, or `String` with `TryFrom`:
//! `let idx: usize = env.get_local("index")?.try_into()?;`
//! fails with an extern error unless the argument is a `bit<W>`.

use std::fmt;

use num_bigint::BigInt;

use p4spectec::runner::ExternError;

use crate::{error, spec::SpecValue};

// == Values

/// A P4 runtime value.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum P4Value {
    /// `bool`.
    Bool(bool),
    /// An arbitrary-precision integer, `int` without a width.
    InfInt(BigInt),
    /// `bit<W>`, an unsigned integer of `width` bits.
    Bit { width: usize, int: BigInt },
    /// `int<W>`, a signed integer of `width` bits.
    Int { width: usize, int: BigInt },
    /// `varbit<W>` holding `width` of at most `width_max` bits.
    Varbit { width_max: usize, width: usize, int: BigInt },
    /// `string`.
    String(String),
    /// An `error` member.
    Error(String),
    /// A `match_kind` member.
    MatchKind(String),
    /// A member of the enum `id_typ`.
    Enum { id_typ: String, id_member: String },
    /// A member of the serializable enum `id_typ`, with its value.
    SerEnum { id_typ: String, id_member: String, p4value: Box<P4Value> },
    /// A struct of type `id_typ`, fields in declaration order.
    Struct { id_typ: String, fields: P4Fields },
    /// A header of type `id_typ` with its validity bit.
    Header { id_typ: String, valid: bool, fields: P4Fields },
    /// A header union of type `id_typ`.
    HeaderUnion { id_typ: String, fields: P4Fields },
    /// A tuple.
    Tuple(Vec<P4Value>),
    /// A `list<T>`.
    List(Vec<P4Value>),
    /// An array `T[n]`.
    Array(Vec<P4Value>),
    /// A header stack and its `nextIndex`.
    HeaderStack { p4values: Vec<P4Value>, idx_next: usize },
    /// A specification value the simulator passes through untouched.
    Opaque(SpecValue),
}

/// The fields of a struct, header, or header union in declaration order,
/// each a field name and its value.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct P4Fields(pub Vec<(String, P4Value)>);

// Prints as the list of fields alone, without the wrapper
impl fmt::Debug for P4Fields {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl P4Value {
    /// The failure for this value where a value of the form `expected` belongs,
    /// such as `expected a bit<W> value, got bool`.
    pub fn mismatch(&self, expected: &str) -> ExternError {
        error::value_invalid(format!("expected {expected} value, got {}", self.form())).into()
    }

    /// The form of this value for failure messages, such as `bit<8>` or `tuple`.
    fn form(&self) -> String {
        match self {
            Self::Bool(_) => "bool".to_owned(),
            Self::InfInt(_) => "int".to_owned(),
            Self::Bit { width, .. } => format!("bit<{width}>"),
            Self::Int { width, .. } => format!("int<{width}>"),
            Self::Varbit { width_max, .. } => format!("varbit<{width_max}>"),
            Self::String(_) => "string".to_owned(),
            Self::Error(_) => "error".to_owned(),
            Self::MatchKind(_) => "match_kind".to_owned(),
            Self::Enum { id_typ, .. }
            | Self::SerEnum { id_typ, .. }
            | Self::Struct { id_typ, .. }
            | Self::Header { id_typ, .. }
            | Self::HeaderUnion { id_typ, .. } => id_typ.clone(),
            Self::Tuple(_) => "tuple".to_owned(),
            Self::List(_) => "list".to_owned(),
            Self::Array(_) => "array".to_owned(),
            Self::HeaderStack { .. } => "header stack".to_owned(),
            Self::Opaque(_) => "an opaque value".to_owned(),
        }
    }
}

// == Conversions to Rust

/// The value of a `bit<W>`, as an index or count.
impl TryFrom<P4Value> for usize {
    type Error = ExternError;

    fn try_from(p4value: P4Value) -> Result<Self, Self::Error> {
        match p4value {
            P4Value::Bit { int, .. } => Ok(usize::try_from(&int)?),
            _ => Err(p4value.mismatch("a bit<W>")),
        }
    }
}

/// The value of a `bit<W>`.
impl TryFrom<P4Value> for BigInt {
    type Error = ExternError;

    fn try_from(p4value: P4Value) -> Result<Self, Self::Error> {
        match p4value {
            P4Value::Bit { int, .. } => Ok(int),
            _ => Err(p4value.mismatch("a bit<W>")),
        }
    }
}

/// The boolean of a `bool`.
impl TryFrom<P4Value> for bool {
    type Error = ExternError;

    fn try_from(p4value: P4Value) -> Result<Self, Self::Error> {
        match p4value {
            P4Value::Bool(value) => Ok(value),
            _ => Err(p4value.mismatch("a bool")),
        }
    }
}

/// The text of a `string`.
impl TryFrom<P4Value> for String {
    type Error = ExternError;

    fn try_from(p4value: P4Value) -> Result<Self, Self::Error> {
        match p4value {
            P4Value::String(text) => Ok(text),
            _ => Err(p4value.mismatch("a string")),
        }
    }
}
