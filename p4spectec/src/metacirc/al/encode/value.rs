//! Encoding of runtime value payloads into meta values
//!
//! `value` and `values` keep child handles in the supplied arena
//! Original named type annotations are not represented by `val`
//! Encoded values carry meta annotations

use super::super::{mixops as m, typs};
use super::{atom, case, id, list, mixop, opt};
use crate::{
    lang::{
        common::{Id, prim::num::Number, source::Span},
        data::{
            arena::Arena,
            notation::tree::Mixop,
            value::flat::{Value, ValueField, ValueKind, make},
        },
    },
    metacirc::Error,
};

/// Encodes a runtime value's payload in the same arena.
pub fn value(arena: &mut Arena, value_target: Value) -> Result<Value, Error> {
    let span = *arena.value.span(value_target.span);
    match arena.value.kind(value_target.node) {
        ValueKind::Bool(value_bool) => {
            let value_bool = *value_bool;
            encode_bool_value(arena, value_bool, span)
        }
        ValueKind::Num(num) => {
            let num = num.clone();
            encode_num_value(arena, num, span)
        }
        ValueKind::Text(text) => {
            let text = text.clone();
            encode_text_value(arena, text, span)
        }
        ValueKind::Struct(value_fields) => {
            let value_fields = value_fields.clone();
            encode_struct_value(arena, &value_fields, span)
        }
        ValueKind::Case(value_case) => {
            let mixop_target = value_case.mixop().into_tree(&arena.mixop);
            let values_target = value_case.args().to_vec();
            encode_case_value(arena, &mixop_target, &values_target, span)
        }
        ValueKind::Tuple(values_target) => {
            let values_target = values_target.clone();
            encode_tuple_value(arena, &values_target, span)
        }
        ValueKind::Opt(value_inner) => {
            let value_inner = *value_inner;
            encode_opt_value(arena, value_inner, span)
        }
        ValueKind::List(values_target) => {
            let values_target = values_target.clone();
            encode_list_value(arena, &values_target, span)
        }
        ValueKind::Func(id_target) => {
            let id_target = id_target.clone();
            encode_func_value(arena, &id_target, span)
        }
        ValueKind::Extern(json) => {
            let json = json.clone();
            encode_extern_value(arena, json, span)
        }
    }
}

/// Encodes a boolean payload.
fn encode_bool_value(arena: &mut Arena, value_bool: bool, span: Span) -> Result<Value, Error> {
    let values_meta = vec![make::bool(arena, value_bool, span)?];
    case(arena, m::BOOL, "val", values_meta, span).map_err(|error| error.at(span))
}

/// Encodes a numeric payload with its natural or integer constructor.
fn encode_num_value(arena: &mut Arena, num: Number, span: Span) -> Result<Value, Error> {
    let shape = match &num {
        Number::Nat(_) => m::NAT,
        Number::Int(_) => m::INT,
    };
    let values_meta = vec![make::num(arena, num, span)?];
    case(arena, shape, "val", values_meta, span).map_err(|error| error.at(span))
}

/// Encodes a text payload.
fn encode_text_value(arena: &mut Arena, text: String, span: Span) -> Result<Value, Error> {
    let values_meta = vec![make::text(arena, text, span)?];
    case(arena, m::TEXT, "val", values_meta, span).map_err(|error| error.at(span))
}

/// Encodes the fields of a runtime struct.
fn encode_struct_value(
    arena: &mut Arena,
    value_fields: &[ValueField],
    span: Span,
) -> Result<Value, Error> {
    let values_meta = vec![list(arena, "valfield", value_fields, valuefield)?];
    case(arena, m::STR, "val", values_meta, span).map_err(|error| error.at(span))
}

/// Encodes a case notation together with its runtime arguments.
fn encode_case_value(
    arena: &mut Arena,
    mixop_target: &Mixop,
    values_target: &[Value],
    span: Span,
) -> Result<Value, Error> {
    let values_case = vec![mixop(arena, mixop_target)?, values(arena, values_target)?];
    let values_meta = vec![case(arena, m::VALCASE, "valcase", values_case, span)?];
    case(arena, m::INJ, "val", values_meta, span).map_err(|error| error.at(span))
}

/// Encodes the elements of a runtime tuple.
fn encode_tuple_value(
    arena: &mut Arena,
    values_target: &[Value],
    span: Span,
) -> Result<Value, Error> {
    let values_meta = vec![values(arena, values_target)?];
    case(arena, m::TUP, "val", values_meta, span).map_err(|error| error.at(span))
}

/// Encodes an optional runtime value.
fn encode_opt_value(
    arena: &mut Arena,
    value_inner: Option<Value>,
    span: Span,
) -> Result<Value, Error> {
    let values_meta = vec![opt(arena, "val", value_inner.as_ref(), |arena, value_inner| {
        value(arena, *value_inner)
    })?];
    case(arena, m::OPT, "val", values_meta, span).map_err(|error| error.at(span))
}

/// Encodes the elements of a runtime list.
fn encode_list_value(
    arena: &mut Arena,
    values_target: &[Value],
    span: Span,
) -> Result<Value, Error> {
    let values_meta = vec![values(arena, values_target)?];
    case(arena, m::LIST, "val", values_meta, span).map_err(|error| error.at(span))
}

/// Encodes a function identifier without its native signature.
fn encode_func_value(arena: &mut Arena, id_target: &Id, span: Span) -> Result<Value, Error> {
    let values_meta = vec![id(arena, id_target)?];
    case(arena, m::FUNC, "val", values_meta, span).map_err(|error| error.at(span))
}

/// Encodes an external JSON payload.
fn encode_extern_value(
    arena: &mut Arena,
    json: std::rc::Rc<crate::util::json::json>,
    span: Span,
) -> Result<Value, Error> {
    let values_meta = vec![make::external(arena, typs::named("json"), json, span)?];
    case(arena, m::EXT, "val", values_meta, span).map_err(|error| error.at(span))
}

/// Encodes target values as a meta `val*`.
pub fn values(arena: &mut Arena, values_target: &[Value]) -> Result<Value, Error> {
    list(arena, "val", values_target, |arena, value_target| value(arena, *value_target))
}

/// Encodes one runtime struct field.
fn valuefield(arena: &mut Arena, value_field: &ValueField) -> Result<Value, Error> {
    let values_meta = vec![atom(arena, &value_field.atom)?, value(arena, value_field.value)?];
    case(arena, m::VALFIELD, "valfield", values_meta, value_field.atom.span)
}
