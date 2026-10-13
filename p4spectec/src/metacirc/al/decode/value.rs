//! Decoding runtime payloads from the AL meta representation
//!
//! The meta syntax omits original type annotations. Compound payloads use
//! canonical annotations; function values retain their identifier only.

use super::super::{mixops as m, typs};
use super::{atom, case, id, mixop};
use crate::{
    lang::{
        common::{Iter, prim::num::Number},
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
        [value_bool] if m::is(&mixop_meta, m::BOOL) => {
            make::bool(arena, get::bool(arena, value_bool)?, span)?
        }
        [value_num] if m::is(&mixop_meta, m::NAT) => {
            let Number::Nat(nat) = get::num(arena, value_num)? else {
                return Err(Error::unexpected(arena, *value_num, "a natural-number payload"));
            };
            let nat = nat.clone();
            make::nat(arena, nat, span)?
        }
        [value_num] if m::is(&mixop_meta, m::INT) => {
            let num = get::num(arena, value_num)?.clone();
            make::int(arena, crate::lang::common::prim::num::to_int(&num).clone(), span)?
        }
        [value_text] if m::is(&mixop_meta, m::TEXT) => {
            make::text(arena, get::text(arena, value_text)?.to_owned(), span)?
        }
        [value_fields] if m::is(&mixop_meta, m::STR) => {
            let values_fields = get::list(arena, value_fields)?.to_vec();
            let fields = values_fields
                .into_iter()
                .map(|value_meta| valuefield(arena, value_meta))
                .collect::<Result<_, _>>()?;
            make::structure(arena, typs::named("val"), fields, span)?
        }
        [value_case] if m::is(&mixop_meta, m::INJ) => {
            let (mixop_meta, values_meta, _) = case(arena, *value_case)?;
            let [value_mixop, values_meta] = values_meta.as_slice() else {
                return Err(Error::unexpected(arena, *value_case, "mixop val*"));
            };
            if !m::is(&mixop_meta, m::VALCASE) {
                return Err(Error::unexpected(arena, *value_case, "mixop val*"));
            }
            let mixop_target = mixop(arena, *value_mixop)?;
            let values_target = values(arena, *values_meta)?;
            let mixfix = Mixfix::new(mixop_target, values_target)?;
            make::case(arena, typs::named("val"), mixfix, span)?
        }
        [values_meta] if m::is(&mixop_meta, m::TUP) => {
            let values_target = values(arena, *values_meta)?;
            make::tuple(arena, typs::named("val"), values_target, span)?
        }
        [value_opt] if m::is(&mixop_meta, m::OPT) => {
            let value_target = get::opt(arena, value_opt)?
                .map(|value_meta| value(arena, value_meta))
                .transpose()?;
            make::opt(arena, typs::iter("val", Iter::Opt), value_target, span)?
        }
        [values_meta] if m::is(&mixop_meta, m::LIST) => {
            let values_target = values(arena, *values_meta)?;
            make::list(arena, typs::iter("val", Iter::List), values_target, span)?
        }
        [value_id] if m::is(&mixop_meta, m::FUNC) => {
            make::func(arena, id(arena, *value_id)?, vec![], vec![], make_typ::bool(), span)?
        }
        [value_json] if m::is(&mixop_meta, m::EXT) => make::external(
            arena,
            typs::named("json"),
            get::external(arena, value_json)?.clone(),
            span,
        )?,
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

/// Decodes a meta `val*` into an owned list of runtime handles.
pub fn values(arena: &mut Arena, value_meta: Value) -> Result<Vec<Value>, Error> {
    let values_meta = get::list(arena, &value_meta)?.to_vec();
    values_meta
        .into_iter()
        .map(|value_meta| value(arena, value_meta))
        .collect()
}

fn valuefield(arena: &mut Arena, value_meta: Value) -> Result<ValueField, Error> {
    let (mixop_meta, values_meta, _) = case(arena, value_meta)?;
    match values_meta.as_slice() {
        [value_atom, value_inner] if m::is(&mixop_meta, m::VALFIELD) => {
            Ok(ValueField { atom: atom(arena, *value_atom)?, value: value(arena, *value_inner)? })
        }
        _ => Err(Error::unexpected(arena, value_meta, "atom val")),
    }
}
