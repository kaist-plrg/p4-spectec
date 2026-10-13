//! Encoding of runtime value payloads into meta values
//!
//! Child handles stay in the supplied arena. Original named type annotations
//! are not represented by `val`; encoded values carry meta annotations.

use super::super::{mixops as m, typs};
use super::{atom, case, id, list, mixop, opt};
use crate::{
    lang::{
        common::prim::num::Number,
        data::{
            arena::Arena,
            value::flat::{Value, ValueField, ValueKind, make},
        },
    },
    metacirc::Error,
};

/// Encodes a runtime value's payload in the same arena.
pub fn value(arena: &mut Arena, value_target: Value) -> Result<Value, Error> {
    let span = *arena.value.span(value_target.span);
    let (shape, values) = match arena.value.kind(value_target.node) {
        ValueKind::Bool(b) => {
            let b = *b;
            (m::BOOL, vec![make::bool(arena, b, span)?])
        }
        ValueKind::Num(num) => {
            let shape = match num {
                Number::Nat(_) => m::NAT,
                Number::Int(_) => m::INT,
            };
            let num = num.clone();
            (shape, vec![make::num(arena, num, span)?])
        }
        ValueKind::Text(text) => {
            let text = text.clone();
            (m::TEXT, vec![make::text(arena, text, span)?])
        }
        ValueKind::Struct(value_fields) => {
            let value_fields = value_fields.clone();
            (m::STR, vec![list(arena, "valfield", &value_fields, valuefield)?])
        }
        ValueKind::Case(value_case) => {
            let mixop_tree = value_case.mixop().into_tree(&arena.mixop);
            let values_target = value_case.args().to_vec();
            let values = vec![mixop(arena, &mixop_tree)?, self::values(arena, &values_target)?];
            (m::INJ, vec![case(arena, m::VALCASE, "valcase", values, span)?])
        }
        ValueKind::Tuple(values_target) => {
            let values_target = values_target.clone();
            (m::TUP, vec![self::values(arena, &values_target)?])
        }
        ValueKind::Opt(value_target) => {
            let value_target = *value_target;
            (
                m::OPT,
                vec![opt(arena, "val", value_target.as_ref(), |arena, value_target| {
                    value(arena, *value_target)
                })?],
            )
        }
        ValueKind::List(values_target) => {
            let values_target = values_target.clone();
            (m::LIST, vec![self::values(arena, &values_target)?])
        }
        ValueKind::Func(id_target) => {
            let id_target = id_target.clone();
            (m::FUNC, vec![id(arena, &id_target)?])
        }
        ValueKind::Extern(json) => {
            let json = json.clone();
            (m::EXT, vec![make::external(arena, typs::named("json"), json, span)?])
        }
    };
    case(arena, shape, "val", values, span).map_err(|error| error.at(span))
}

/// Encodes target values as a meta `val*`.
pub fn values(arena: &mut Arena, values_target: &[Value]) -> Result<Value, Error> {
    list(arena, "val", values_target, |arena, value_target| value(arena, *value_target))
}

/// Encodes one runtime struct field.
fn valuefield(arena: &mut Arena, value_field: &ValueField) -> Result<Value, Error> {
    let values = vec![atom(arena, &value_field.atom)?, value(arena, value_field.value)?];
    case(arena, m::VALFIELD, "valfield", values, value_field.atom.span)
}
