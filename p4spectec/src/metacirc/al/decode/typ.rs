//! Decoding of complete types and canonical type definitions
//!
//! Alias bodies, fields, and variant arguments survive conversion.
//! General hints and variant origins are absent from the representation.

use super::super::mixops as m;
use super::{atom, case, id, iter, list, mixop};
use crate::{
    lang::{
        common::prim::num,
        data::{arena::Arena, notation::tree::Mixfix, value::flat::Value},
        il::ast,
    },
    metacirc::Error,
    phrase,
};

/// Decodes a source type, checking every constructor's shape.
pub fn typ(arena: &Arena, value_meta: Value) -> Result<ast::Typ, Error> {
    let (mixop, values, span) = case(arena, value_meta)?;
    let typ_kind = match values.as_slice() {
        [] if m::is(&mixop, m::BOOL_TYP) => ast::TypKind::Bool,
        [] if m::is(&mixop, m::NAT_TYP) => ast::TypKind::Num(num::Typ::Nat),
        [] if m::is(&mixop, m::INT_TYP) => ast::TypKind::Num(num::Typ::Int),
        [] if m::is(&mixop, m::TEXT_TYP) => ast::TypKind::Text,
        [value_id, value_typs] if m::is(&mixop, m::VAR_TYP) => {
            ast::TypKind::Var(id(arena, *value_id)?, typs(arena, *value_typs)?)
        }
        [value_typs] if m::is(&mixop, m::TUP_TYP) => ast::TypKind::Tuple(typs(arena, *value_typs)?),
        [value_typ, value_iter] if m::is(&mixop, m::ITER_TYP) => {
            ast::TypKind::Iter(Box::new(typ(arena, *value_typ)?), iter(arena, *value_iter)?)
        }
        [value_ids, value_typs, value_ret] if m::is(&mixop, m::FUNC_TYP) => {
            ast::TypKind::Func(ast::FuncTyp {
                tparams: list(arena, *value_ids, id)?,
                typs_params: typs(arena, *value_typs)?,
                typ_ret: Box::new(typ(arena, *value_ret)?),
            })
        }
        _ => {
            return Err(Error::unexpected(
                arena,
                value_meta,
                "an AL type constructor with its declared arguments",
            ));
        }
    };
    Ok(phrase!(node: typ_kind, span: span))
}

/// Decodes an owned list of source types.
pub fn typs(arena: &Arena, value_meta: Value) -> Result<Vec<ast::Typ>, Error> {
    list(arena, value_meta, typ)
}

/// Decodes an alias, struct, or variant without general hints.
pub fn deftyp(arena: &Arena, value_meta: Value) -> Result<ast::DefTyp, Error> {
    let (mixop, values, span) = case(arena, value_meta)?;
    let deftyp_kind = match values.as_slice() {
        [value_typ] if m::is(&mixop, m::ALIAS) => ast::DefTypKind::Plain(typ(arena, *value_typ)?),
        [value_fields] if m::is(&mixop, m::STRUCT) => {
            ast::DefTypKind::Struct(list(arena, *value_fields, typfield)?)
        }
        [value_cases] if m::is(&mixop, m::VARIANT) => {
            ast::DefTypKind::Variant(list(arena, *value_cases, typcase)?)
        }
        _ => return Err(Error::unexpected(arena, value_meta, "ALIAS, STRUCT, or VARIANT")),
    };
    Ok(phrase!(node: deftyp_kind, span: span))
}

/// Decodes one struct type field.
fn typfield(arena: &Arena, value_meta: Value) -> Result<ast::TypField, Error> {
    let (mixop, values, _) = case(arena, value_meta)?;
    match values.as_slice() {
        [value_atom, value_typ] if m::is(&mixop, m::TYPFIELD) => {
            Ok(ast::TypField { atom: atom(arena, *value_atom)?, typ: typ(arena, *value_typ)? })
        }
        _ => Err(Error::unexpected(arena, value_meta, "atom typ")),
    }
}

/// Decodes one variant case without its origin or hints.
fn typcase(arena: &Arena, value_meta: Value) -> Result<ast::TypCase, Error> {
    let (mixop_meta, values, span) = case(arena, value_meta)?;
    match values.as_slice() {
        [value_mixop, value_typs] if m::is(&mixop_meta, m::TYPCASE) => {
            let not_typ = Mixfix::new(mixop(arena, *value_mixop)?, typs(arena, *value_typs)?)?;
            let id_origin = phrase!(node: "".into(), span: span);
            let typ_origin =
                phrase!(node: ast::TypOriginKind { id: id_origin, targs: vec![] }, span: span);
            Ok(ast::TypCase {
                not_typ: phrase!(node: not_typ, span: span),
                typ_origin,
                hints: vec![],
            })
        }
        _ => Err(Error::unexpected(arena, value_meta, "mixop typ*")),
    }
}
