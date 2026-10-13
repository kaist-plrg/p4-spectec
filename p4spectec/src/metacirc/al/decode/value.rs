//! Decoding runtime payloads from the AL meta representation
//!
//! `value` and `values` allocate payloads after checking their meta constructors
//! Compound payloads use canonical annotations,
//! and function values retain their identifier only

use super::super::{mixops as m, typs};
use super::{atom, case, id, mixop};
use crate::{
    lang::{
        common::{Iter, prim::num::Number, source::Span},
        data::{
            arena::Arena,
            notation::tree::Mixfix,
            typ::make as make_typ,
            value::flat::{Value, ValueField, get, make},
        },
    },
    metacirc::Error,
};

/// Decodes a runtime payload, allocating only in the supplied arena.
pub fn value(arena: &mut Arena, value_meta: Value) -> Result<Value, Error> {
    let (mixop_meta, values_meta, span) = case(arena, value_meta)?;
    let value_target = match values_meta.as_slice() {
        [value_bool] if m::is(&mixop_meta, m::BOOL) => decode_bool_value(arena, *value_bool, span)?,
        [value_num] if m::is(&mixop_meta, m::NAT) => decode_nat_value(arena, *value_num, span)?,
        [value_num] if m::is(&mixop_meta, m::INT) => decode_int_value(arena, *value_num, span)?,
        [value_text] if m::is(&mixop_meta, m::TEXT) => decode_text_value(arena, *value_text, span)?,
        [value_fields] if m::is(&mixop_meta, m::STR) => {
            decode_struct_value(arena, *value_fields, span)?
        }
        [value_case] if m::is(&mixop_meta, m::INJ) => decode_case_value(arena, *value_case, span)?,
        [value_values] if m::is(&mixop_meta, m::TUP) => {
            decode_tuple_value(arena, *value_values, span)?
        }
        [value_opt] if m::is(&mixop_meta, m::OPT) => decode_opt_value(arena, *value_opt, span)?,
        [value_values] if m::is(&mixop_meta, m::LIST) => {
            decode_list_value(arena, *value_values, span)?
        }
        [value_id] if m::is(&mixop_meta, m::FUNC) => decode_func_value(arena, *value_id, span)?,
        [value_json] if m::is(&mixop_meta, m::EXT) => {
            decode_external_value(arena, *value_json, span)?
        }
        _ => {
            return Err(Error::unexpected(
                arena,
                value_meta,
                "an AL value constructor with its declared arguments",
            ));
        }
    };
    Ok(value_target)
}

fn decode_bool_value(arena: &mut Arena, value_bool: Value, span: Span) -> Result<Value, Error> {
    Ok(make::bool(arena, get::bool(arena, &value_bool)?, span)?)
}

/// Checks a natural payload before allocating its runtime value.
fn decode_nat_value(arena: &mut Arena, value_num: Value, span: Span) -> Result<Value, Error> {
    let Number::Nat(nat) = get::num(arena, &value_num)? else {
        return Err(Error::unexpected(arena, value_num, "a natural-number payload"));
    };
    let nat = nat.clone();
    Ok(make::nat(arena, nat, span)?)
}

/// Converts a numeric payload to its canonical integer representation.
fn decode_int_value(arena: &mut Arena, value_num: Value, span: Span) -> Result<Value, Error> {
    let num = get::num(arena, &value_num)?.clone();
    Ok(make::int(arena, crate::lang::common::prim::num::to_int(&num).clone(), span)?)
}

fn decode_text_value(arena: &mut Arena, value_text: Value, span: Span) -> Result<Value, Error> {
    Ok(make::text(arena, get::text(arena, &value_text)?.to_owned(), span)?)
}

/// Copies field handles before recursively allocating their payloads.
fn decode_struct_value(arena: &mut Arena, value_fields: Value, span: Span) -> Result<Value, Error> {
    let values_fields = get::list(arena, &value_fields)?.to_vec();
    let value_fields_target = values_fields
        .into_iter()
        .map(|value_meta| valuefield(arena, value_meta))
        .collect::<Result<_, _>>()?;
    Ok(make::structure(arena, typs::named("val"), value_fields_target, span)?)
}

/// Checks case notation before recursively allocating its arguments.
fn decode_case_value(arena: &mut Arena, value_case: Value, span: Span) -> Result<Value, Error> {
    // Check the nested case shape before decoding its children
    let (mixop_meta, values_meta, _) = case(arena, value_case)?;
    let [value_mixop, value_values] = values_meta.as_slice() else {
        return Err(Error::unexpected(arena, value_case, "mixop val*"));
    };
    // Reject constructors with the right arity but the wrong notation
    if !m::is(&mixop_meta, m::VALCASE) {
        return Err(Error::unexpected(arena, value_case, "mixop val*"));
    }
    // Rebuild the notation with its decoded runtime arguments
    let mixop_target = mixop(arena, *value_mixop)?;
    let values_target = values(arena, *value_values)?;
    let mixfix_target = Mixfix::new(mixop_target, values_target)?;
    Ok(make::case(arena, typs::named("val"), mixfix_target, span)?)
}

fn decode_tuple_value(arena: &mut Arena, value_values: Value, span: Span) -> Result<Value, Error> {
    let values_target = values(arena, value_values)?;
    Ok(make::tuple(arena, typs::named("val"), values_target, span)?)
}

fn decode_opt_value(arena: &mut Arena, value_opt: Value, span: Span) -> Result<Value, Error> {
    let value_target = get::opt(arena, &value_opt)?
        .map(|value_meta| value(arena, value_meta))
        .transpose()?;
    Ok(make::opt(arena, typs::iter("val", Iter::Opt), value_target, span)?)
}

fn decode_list_value(arena: &mut Arena, value_values: Value, span: Span) -> Result<Value, Error> {
    let values_target = values(arena, value_values)?;
    Ok(make::list(arena, typs::iter("val", Iter::List), values_target, span)?)
}

fn decode_func_value(arena: &mut Arena, value_id: Value, span: Span) -> Result<Value, Error> {
    Ok(make::func(arena, id(arena, value_id)?, vec![], vec![], make_typ::bool(), span)?)
}

fn decode_external_value(arena: &mut Arena, value_json: Value, span: Span) -> Result<Value, Error> {
    Ok(make::external(
        arena,
        typs::named("json"),
        get::external(arena, &value_json)?.clone(),
        span,
    )?)
}

/// Decodes a meta `val*` into an owned list of runtime handles.
pub fn values(arena: &mut Arena, value_meta: Value) -> Result<Vec<Value>, Error> {
    let values_meta = get::list(arena, &value_meta)?.to_vec();
    values_meta
        .into_iter()
        .map(|value_meta| value(arena, value_meta))
        .collect()
}

/// Decodes one runtime struct field.
fn valuefield(arena: &mut Arena, value_meta: Value) -> Result<ValueField, Error> {
    let (mixop_meta, values_meta, _) = case(arena, value_meta)?;
    match values_meta.as_slice() {
        [value_atom, value_inner] if m::is(&mixop_meta, m::VALFIELD) => {
            decode_valuefield(arena, *value_atom, *value_inner)
        }
        _ => Err(Error::unexpected(arena, value_meta, "atom val")),
    }
}

fn decode_valuefield(
    arena: &mut Arena,
    value_atom: Value,
    value_inner: Value,
) -> Result<ValueField, Error> {
    Ok(ValueField { atom: atom(arena, value_atom)?, value: value(arena, value_inner)? })
}
