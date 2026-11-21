//! Converts Rust values to and from the specification's values
//!
//! `FromSpec` reads a specification value into a Rust value,
//! and `ToSpec` builds one back, with the case shape and type annotation
//! the specification gives that form:
//! `text`, `int`, `nat`, and `bool`; a `value*` list; a `fieldValue*` list (`P4Fields`);
//! a `P4Value`, such as `nat W int` or `HEADER typeId { bool ; fieldValue* }`;
//! the `returnResult` or `rejectTransitionResult` of a `CallResult`;
//! how a parser or control block ended, `BlockResult`;
//! and an argument of an initialization relation, `RelArg`.
//! The round trip preserves a value, if not its source span.

use num_bigint::BigInt;

use p4spectec::lang::{
    common::{prim::num, source::Span},
    data::{
        arena::Arena,
        typ,
        value::{
            ValueError,
            flat::{self as value, Value},
        },
    },
};

use p4spectec::runner::ExternError;

use crate::p4::{BlockResult, CallResult, P4Fields, P4Value, RelArg};

use super::SpecValue;

// == Conversions

/// A Rust value read from a specification value.
pub trait FromSpec: Sized {
    /// Reads `value`.
    fn from_spec(arena: &mut Arena, value: &Value) -> Result<Self, ExternError>;
}

/// A Rust value the specification has a value for.
pub trait ToSpec {
    /// Builds the specification value for this value.
    fn to_spec(&self, arena: &mut Arena) -> Result<Value, ExternError>;
}

// == Helpers

/// The arguments of a case, which must number `N`.
fn case_args<const N: usize>(values: &[Value]) -> Result<&[Value; N], ExternError> {
    <&[Value; N]>::try_from(values)
        .map_err(|_| ValueError::CountMismatch { expected: N, actual: values.len() }.into())
}

// == Primitives

/// A `text`.
impl FromSpec for String {
    fn from_spec(arena: &mut Arena, value: &Value) -> Result<Self, ExternError> {
        Ok(value::get::text(arena, value)?.to_owned())
    }
}

/// A `text`.
impl ToSpec for str {
    fn to_spec(&self, arena: &mut Arena) -> Result<Value, ExternError> {
        Ok(value::make::text(arena, self.to_owned(), Span::default())?)
    }
}

/// An `int`.
impl FromSpec for BigInt {
    fn from_spec(arena: &mut Arena, value: &Value) -> Result<Self, ExternError> {
        Ok(num::to_int(value::get::num(arena, value)?).clone())
    }
}

/// An `int`.
impl ToSpec for BigInt {
    fn to_spec(&self, arena: &mut Arena) -> Result<Value, ExternError> {
        Ok(value::make::int(arena, self.clone(), Span::default())?)
    }
}

/// A `nat`, such as a width.
impl FromSpec for usize {
    fn from_spec(arena: &mut Arena, value: &Value) -> Result<Self, ExternError> {
        Ok(usize::try_from(num::to_int(value::get::num(arena, value)?))?)
    }
}

/// A `nat`, such as a width.
impl ToSpec for usize {
    fn to_spec(&self, arena: &mut Arena) -> Result<Value, ExternError> {
        Ok(value::make::nat(arena, num::Natural::from(*self as u64), Span::default())?)
    }
}

/// A `bool`.
impl FromSpec for bool {
    fn from_spec(arena: &mut Arena, value: &Value) -> Result<Self, ExternError> {
        Ok(value::get::bool(arena, value)?)
    }
}

/// A `bool`.
impl ToSpec for bool {
    fn to_spec(&self, arena: &mut Arena) -> Result<Value, ExternError> {
        Ok(value::make::bool(arena, *self, Span::default())?)
    }
}

// == Lists

/// A list, each element read as a `T`.
impl<T: FromSpec> FromSpec for Vec<T> {
    fn from_spec(arena: &mut Arena, value: &Value) -> Result<Self, ExternError> {
        let values = value::get::list(arena, value)?.to_vec();
        values
            .iter()
            .map(|value| T::from_spec(arena, value))
            .collect()
    }
}

/// A `value*` list.
impl ToSpec for [bool] {
    /// A `bit*` list, as the bit conversions take it.
    fn to_spec(&self, arena: &mut Arena) -> Result<Value, ExternError> {
        let values = self
            .iter()
            .map(|bit| bit.to_spec(arena))
            .collect::<Result<Vec<_>, _>>()?;
        let typ_bit = typ::make::var(
            p4spectec::phrase!(node: "bit".into(), span: Span::default()),
            Vec::new(),
        );
        let typ_list = typ::make::list(typ_bit).node;
        Ok(value::make::list(arena, typ_list.into(), values, Span::default())?)
    }
}

impl<T: FromSpec> FromSpec for Option<T> {
    /// An optional value, `eps` or one value.
    fn from_spec(arena: &mut Arena, value: &Value) -> Result<Self, ExternError> {
        value::get::opt(arena, value)?
            .map(|value| T::from_spec(arena, &value))
            .transpose()
    }
}

impl<A: FromSpec, B: FromSpec, C: FromSpec> FromSpec for (A, B, C) {
    /// A three-element tuple.
    fn from_spec(arena: &mut Arena, value: &Value) -> Result<Self, ExternError> {
        let values = value::get::tuple(arena, value)?.to_vec();
        let (value_a, value_b, value_c) = value::get::three(&values)?;
        Ok((
            A::from_spec(arena, value_a)?,
            B::from_spec(arena, value_b)?,
            C::from_spec(arena, value_c)?,
        ))
    }
}

impl FromSpec for Value {
    /// A specification value passed through as is, such as a context or a type.
    fn from_spec(_arena: &mut Arena, value: &Value) -> Result<Self, ExternError> {
        Ok(*value)
    }
}

impl ToSpec for Vec<P4Value> {
    fn to_spec(&self, arena: &mut Arena) -> Result<Value, ExternError> {
        let values = self
            .iter()
            .map(|p4value| p4value.to_spec(arena))
            .collect::<Result<Vec<_>, _>>()?;
        let typ_value = typ::make::var(
            p4spectec::phrase!(node: "value".into(), span: Span::default()),
            Vec::new(),
        );
        let typ_list = typ::make::list(typ_value).node;
        Ok(value::make::list(arena, typ_list.into(), values, Span::default())?)
    }
}

/// A `fieldValue*` list, each field a `value nameIR ;`.
impl FromSpec for P4Fields {
    fn from_spec(arena: &mut Arena, value: &Value) -> Result<Self, ExternError> {
        let values_field = value::get::list(arena, value)?.to_vec();
        let fields = values_field
            .iter()
            .map(|value_field| {
                value::get::matches_string! { arena, value_field,
                    "value nameIR ';'" => |values| {
                        let [value_inner, value_id] = case_args(values)?;
                        Ok((String::from_spec(arena, value_id)?, P4Value::from_spec(arena, value_inner)?))
                    },
                    _ => Err(crate::error::value_invalid("expected P4 field value".to_owned()).into()),
                }
            })
            .collect::<Result<_, ExternError>>()?;
        Ok(Self(fields))
    }
}

/// A `fieldValue*` list, each field a `value nameIR ;`.
impl ToSpec for P4Fields {
    fn to_spec(&self, arena: &mut Arena) -> Result<Value, ExternError> {
        let values_field = self
            .0
            .iter()
            .map(|(id, p4value)| {
                let value_inner = p4value.to_spec(arena)?;
                let value_id = id.to_spec(arena)?;
                Ok(value::make::case_string! {
                    arena: arena,
                    shape: "value nameIR ';'",
                    args: vec![value_inner, value_id],
                    typ: "fieldValue",
                    span: Span::default(),
                }?)
            })
            .collect::<Result<Vec<_>, ExternError>>()?;
        let typ_field = typ::make::var(
            p4spectec::phrase!(node: "fieldValue".into(), span: Span::default()),
            Vec::new(),
        );
        let typ_list = typ::make::list(typ_field).node;
        Ok(value::make::list(arena, typ_list.into(), values_field, Span::default())?)
    }
}

// == Values

/// A `value`; a form without a variant stays opaque.
impl FromSpec for P4Value {
    fn from_spec(arena: &mut Arena, value: &Value) -> Result<Self, ExternError> {
        value::get::matches_string! { arena, value,
            "_B bool" => |values| {
                let [value_bool] = case_args(values)?;
                Ok(Self::Bool(bool::from_spec(arena, value_bool)?))
            },
            "D int" => |values| {
                let [value_int] = case_args(values)?;
                Ok(Self::InfInt(BigInt::from_spec(arena, value_int)?))
            },
            "nat W int" => |values| {
                let [value_width, value_int] = case_args(values)?;
                Ok(Self::Bit {
                    width: usize::from_spec(arena, value_width)?,
                    int: BigInt::from_spec(arena, value_int)?,
                })
            },
            "nat S int" => |values| {
                let [value_width, value_int] = case_args(values)?;
                Ok(Self::Int {
                    width: usize::from_spec(arena, value_width)?,
                    int: BigInt::from_spec(arena, value_int)?,
                })
            },
            "nat '.' nat V int" => |values| {
                let [value_width_max, value_width, value_int] = case_args(values)?;
                Ok(Self::Varbit {
                    width_max: usize::from_spec(arena, value_width_max)?,
                    width: usize::from_spec(arena, value_width)?,
                    int: BigInt::from_spec(arena, value_int)?,
                })
            },
            "'\"' text '\"'" => |values| {
                let [value_text] = case_args(values)?;
                Ok(Self::String(String::from_spec(arena, value_text)?))
            },
            "ERROR '.' nameIR" => |values| {
                let [value_id] = case_args(values)?;
                Ok(Self::Error(String::from_spec(arena, value_id)?))
            },
            "MATCH_KIND '.' nameIR" => |values| {
                let [value_id] = case_args(values)?;
                Ok(Self::MatchKind(String::from_spec(arena, value_id)?))
            },
            "typeId '.' nameIR" => |values| {
                let [value_id_typ, value_id_member] = case_args(values)?;
                Ok(Self::Enum {
                    id_typ: String::from_spec(arena, value_id_typ)?,
                    id_member: String::from_spec(arena, value_id_member)?,
                })
            },
            "typeId '.' nameIR '.' value" => |values| {
                let [value_id_typ, value_id_member, value_inner] = case_args(values)?;
                Ok(Self::SerEnum {
                    id_typ: String::from_spec(arena, value_id_typ)?,
                    id_member: String::from_spec(arena, value_id_member)?,
                    p4value: Box::new(Self::from_spec(arena, value_inner)?),
                })
            },
            "STRUCT typeId `{ fieldValue* `}" => |values| {
                let [value_id_typ, value_fields] = case_args(values)?;
                Ok(Self::Struct {
                    id_typ: String::from_spec(arena, value_id_typ)?,
                    fields: P4Fields::from_spec(arena, value_fields)?,
                })
            },
            "HEADER typeId `{ bool ';' fieldValue* `}" => |values| {
                let [value_id_typ, value_valid, value_fields] = case_args(values)?;
                Ok(Self::Header {
                    id_typ: String::from_spec(arena, value_id_typ)?,
                    valid: bool::from_spec(arena, value_valid)?,
                    fields: P4Fields::from_spec(arena, value_fields)?,
                })
            },
            "HEADER_UNION typeId `{ fieldValue* `}" => |values| {
                let [value_id_typ, value_fields] = case_args(values)?;
                Ok(Self::HeaderUnion {
                    id_typ: String::from_spec(arena, value_id_typ)?,
                    fields: P4Fields::from_spec(arena, value_fields)?,
                })
            },
            "TUPLE `( value* `)" => |values| {
                let [value_list] = case_args(values)?;
                Ok(Self::Tuple(Vec::from_spec(arena, value_list)?))
            },
            "LIST `[ value* `]" => |values| {
                let [value_list] = case_args(values)?;
                Ok(Self::List(Vec::from_spec(arena, value_list)?))
            },
            "ARRAY `[ value* `]" => |values| {
                let [value_list] = case_args(values)?;
                Ok(Self::Array(Vec::from_spec(arena, value_list)?))
            },
            "HEADER_STACK `[ value* `( nat `) `]" => |values| {
                let [value_list, value_idx_next] = case_args(values)?;
                Ok(Self::HeaderStack {
                    p4values: Vec::from_spec(arena, value_list)?,
                    idx_next: usize::from_spec(arena, value_idx_next)?,
                })
            },
            _ => Ok(Self::Opaque(SpecValue(*value))),
        }
    }
}

/// A `value`; an opaque value is the one it holds.
impl ToSpec for P4Value {
    fn to_spec(&self, arena: &mut Arena) -> Result<Value, ExternError> {
        let (shape, values) = match self {
            Self::Bool(value_bool) => ("_B bool", vec![value_bool.to_spec(arena)?]),
            Self::InfInt(int) => ("D int", vec![int.to_spec(arena)?]),
            Self::Bit { width, int } => {
                ("nat W int", vec![width.to_spec(arena)?, int.to_spec(arena)?])
            }
            Self::Int { width, int } => {
                ("nat S int", vec![width.to_spec(arena)?, int.to_spec(arena)?])
            }
            Self::Varbit { width_max, width, int } => (
                "nat '.' nat V int",
                vec![width_max.to_spec(arena)?, width.to_spec(arena)?, int.to_spec(arena)?],
            ),
            Self::String(text) => ("'\"' text '\"'", vec![text.to_spec(arena)?]),
            Self::Error(id) => ("ERROR '.' nameIR", vec![id.to_spec(arena)?]),
            Self::MatchKind(id) => ("MATCH_KIND '.' nameIR", vec![id.to_spec(arena)?]),
            Self::Enum { id_typ, id_member } => {
                ("typeId '.' nameIR", vec![id_typ.to_spec(arena)?, id_member.to_spec(arena)?])
            }
            Self::SerEnum { id_typ, id_member, p4value } => (
                "typeId '.' nameIR '.' value",
                vec![id_typ.to_spec(arena)?, id_member.to_spec(arena)?, p4value.to_spec(arena)?],
            ),
            Self::Struct { id_typ, fields } => (
                "STRUCT typeId `{ fieldValue* `}",
                vec![id_typ.to_spec(arena)?, fields.to_spec(arena)?],
            ),
            Self::Header { id_typ, valid, fields } => (
                "HEADER typeId `{ bool ';' fieldValue* `}",
                vec![id_typ.to_spec(arena)?, valid.to_spec(arena)?, fields.to_spec(arena)?],
            ),
            Self::HeaderUnion { id_typ, fields } => (
                "HEADER_UNION typeId `{ fieldValue* `}",
                vec![id_typ.to_spec(arena)?, fields.to_spec(arena)?],
            ),
            Self::Tuple(p4values) => ("TUPLE `( value* `)", vec![p4values.to_spec(arena)?]),
            Self::List(p4values) => ("LIST `[ value* `]", vec![p4values.to_spec(arena)?]),
            Self::Array(p4values) => ("ARRAY `[ value* `]", vec![p4values.to_spec(arena)?]),
            Self::HeaderStack { p4values, idx_next } => (
                "HEADER_STACK `[ value* `( nat `) `]",
                vec![p4values.to_spec(arena)?, idx_next.to_spec(arena)?],
            ),
            Self::Opaque(value) => return Ok(value.0),
        };
        Ok(value::make::case_string! {
            arena: arena,
            shape: shape,
            args: values,
            typ: "value",
            span: Span::default(),
        }?)
    }
}

// == Call results

/// The `returnResult` or `rejectTransitionResult` of an extern call.
impl ToSpec for CallResult {
    fn to_spec(&self, arena: &mut Arena) -> Result<Value, ExternError> {
        match self {
            // `RETURN value?`, with or without a value
            Self::Return(p4value_ret) => {
                let value_ret = p4value_ret
                    .as_ref()
                    .map(|p4value| p4value.to_spec(arena))
                    .transpose()?;
                let typ_value = typ::make::var(
                    p4spectec::phrase!(node: "value".into(), span: Span::default()),
                    Vec::new(),
                );
                let typ_opt = typ::make::opt(typ_value).node;
                let value_opt =
                    value::make::opt(arena, typ_opt.into(), value_ret, Span::default())?;
                Ok(value::make::case_string! {
                    arena: arena,
                    shape: "RETURN value?",
                    args: vec![value_opt],
                    typ: "returnResult",
                    span: Span::default(),
                }?)
            }
            // `REJECT errorValue` with the error member
            Self::Reject(id) => {
                let value_id = id.to_spec(arena)?;
                let value_error = value::make::case_string! {
                    arena: arena,
                    shape: "ERROR '.' nameIR",
                    args: vec![value_id],
                    typ: "errorValue",
                    span: Span::default(),
                }?;
                Ok(value::make::case_string! {
                    arena: arena,
                    shape: "REJECT errorValue",
                    args: vec![value_error],
                    typ: "rejectTransitionResult",
                    span: Span::default(),
                }?)
            }
        }
    }
}

// == Block results

/// The `transitionResult` or `callResult` of a parser or control block.
impl FromSpec for BlockResult {
    fn from_spec(arena: &mut Arena, value: &Value) -> Result<Self, ExternError> {
        value::get::matches_string! { arena, value,
            "REJECT errorValue" => |values| {
                let [value_error] = case_args(values)?;
                match P4Value::from_spec(arena, value_error)? {
                    P4Value::Error(id) => Ok(Self::Reject(id)),
                    p4value_error => Err(p4value_error.mismatch("an error")),
                }
            },
            "EXIT" => |_values| Ok(Self::Exit),
            _ => Ok(Self::Done),
        }
    }
}

// == Relation arguments

/// An `int`, a `text`, or a P4 value, after the context and the state.
impl ToSpec for RelArg {
    fn to_spec(&self, arena: &mut Arena) -> Result<Value, ExternError> {
        match self {
            Self::Int(int) => BigInt::from(*int).to_spec(arena),
            Self::Text(text) => text.to_spec(arena),
            Self::Value(p4value) => p4value.to_spec(arena),
        }
    }
}
