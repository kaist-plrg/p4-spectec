//! Decoding of complete types and canonical type definitions
//!
//! `typ` and `deftyp` retain alias bodies, fields, and variant arguments
//! General hints and variant origins are absent from the representation

use super::super::mixops as m;
use super::{atom, case, id, iter, list, mixop};
use crate::{
    lang::{
        common::{prim::num, source::Span},
        data::{arena::Arena, notation::tree::Mixfix, value::flat::Value},
        il::ast,
    },
    metacirc::Error,
    phrase,
};

/// Decodes a source type, checking every constructor's shape.
pub fn typ(arena: &Arena, value_meta: Value) -> Result<ast::Typ, Error> {
    let (mixop_meta, values_meta, span) = case(arena, value_meta)?;
    let typ_kind_al = match values_meta.as_slice() {
        [] if m::is(&mixop_meta, m::BOOL_TYP) => ast::TypKind::Bool,
        [] if m::is(&mixop_meta, m::NAT_TYP) => ast::TypKind::Num(num::Typ::Nat),
        [] if m::is(&mixop_meta, m::INT_TYP) => ast::TypKind::Num(num::Typ::Int),
        [] if m::is(&mixop_meta, m::TEXT_TYP) => ast::TypKind::Text,
        [value_id, value_typs] if m::is(&mixop_meta, m::VAR_TYP) => {
            decode_var_typ(arena, *value_id, *value_typs)?
        }
        [value_typs] if m::is(&mixop_meta, m::TUP_TYP) => decode_tuple_typ(arena, *value_typs)?,
        [value_typ, value_iter] if m::is(&mixop_meta, m::ITER_TYP) => {
            decode_iter_typ(arena, *value_typ, *value_iter)?
        }
        [value_ids, value_typs, value_ret] if m::is(&mixop_meta, m::FUNC_TYP) => {
            decode_func_typ(arena, *value_ids, *value_typs, *value_ret)?
        }
        _ => {
            return Err(Error::unexpected(
                arena,
                value_meta,
                "an AL type constructor with its declared arguments",
            ));
        }
    };
    Ok(phrase!(node: typ_kind_al, span: span))
}

fn decode_var_typ(
    arena: &Arena,
    value_id: Value,
    value_typs: Value,
) -> Result<ast::TypKind, Error> {
    Ok(ast::TypKind::Var(id(arena, value_id)?, typs(arena, value_typs)?))
}

fn decode_tuple_typ(arena: &Arena, value_typs: Value) -> Result<ast::TypKind, Error> {
    Ok(ast::TypKind::Tuple(typs(arena, value_typs)?))
}

fn decode_iter_typ(
    arena: &Arena,
    value_typ: Value,
    value_iter: Value,
) -> Result<ast::TypKind, Error> {
    Ok(ast::TypKind::Iter(Box::new(typ(arena, value_typ)?), iter(arena, value_iter)?))
}

/// Reconstructs a function type and its type parameters.
fn decode_func_typ(
    arena: &Arena,
    value_ids: Value,
    value_typs: Value,
    value_ret: Value,
) -> Result<ast::TypKind, Error> {
    Ok(ast::TypKind::Func(ast::FuncTyp {
        tparams: list(arena, value_ids, id)?,
        typs_params: typs(arena, value_typs)?,
        typ_ret: Box::new(typ(arena, value_ret)?),
    }))
}

/// Decodes an owned list of source types.
pub fn typs(arena: &Arena, value_meta: Value) -> Result<Vec<ast::Typ>, Error> {
    list(arena, value_meta, typ)
}

/// Decodes an alias, struct, or variant without general hints.
pub fn deftyp(arena: &Arena, value_meta: Value) -> Result<ast::DefTyp, Error> {
    let (mixop_meta, values_meta, span) = case(arena, value_meta)?;
    let deftyp_kind_al = match values_meta.as_slice() {
        [value_typ] if m::is(&mixop_meta, m::ALIAS) => decode_alias_deftyp(arena, *value_typ)?,
        [value_fields] if m::is(&mixop_meta, m::STRUCT) => {
            decode_struct_deftyp(arena, *value_fields)?
        }
        [value_cases] if m::is(&mixop_meta, m::VARIANT) => {
            decode_variant_deftyp(arena, *value_cases)?
        }
        _ => return Err(Error::unexpected(arena, value_meta, "ALIAS, STRUCT, or VARIANT")),
    };
    Ok(phrase!(node: deftyp_kind_al, span: span))
}

fn decode_alias_deftyp(arena: &Arena, value_typ: Value) -> Result<ast::DefTypKind, Error> {
    Ok(ast::DefTypKind::Plain(typ(arena, value_typ)?))
}

fn decode_struct_deftyp(arena: &Arena, value_fields: Value) -> Result<ast::DefTypKind, Error> {
    Ok(ast::DefTypKind::Struct(list(arena, value_fields, typfield)?))
}

fn decode_variant_deftyp(arena: &Arena, value_cases: Value) -> Result<ast::DefTypKind, Error> {
    Ok(ast::DefTypKind::Variant(list(arena, value_cases, typcase)?))
}

/// Decodes one struct type field.
fn typfield(arena: &Arena, value_meta: Value) -> Result<ast::TypField, Error> {
    let (mixop_meta, values_meta, _) = case(arena, value_meta)?;
    match values_meta.as_slice() {
        [value_atom, value_typ] if m::is(&mixop_meta, m::TYPFIELD) => {
            decode_typfield(arena, *value_atom, *value_typ)
        }
        _ => Err(Error::unexpected(arena, value_meta, "atom typ")),
    }
}

fn decode_typfield(
    arena: &Arena,
    value_atom: Value,
    value_typ: Value,
) -> Result<ast::TypField, Error> {
    Ok(ast::TypField { atom: atom(arena, value_atom)?, typ: typ(arena, value_typ)? })
}

/// Decodes one variant case without its origin or hints.
fn typcase(arena: &Arena, value_meta: Value) -> Result<ast::TypCase, Error> {
    let (mixop_meta, values_meta, span) = case(arena, value_meta)?;
    match values_meta.as_slice() {
        [value_mixop, value_typs] if m::is(&mixop_meta, m::TYPCASE) => {
            decode_typcase(arena, *value_mixop, *value_typs, span)
        }
        _ => Err(Error::unexpected(arena, value_meta, "mixop typ*")),
    }
}

/// Rebuilds a variant case with an empty origin and no hints.
fn decode_typcase(
    arena: &Arena,
    value_mixop: Value,
    value_typs: Value,
    span: Span,
) -> Result<ast::TypCase, Error> {
    // Decode the notation and its type arguments
    let mixfix_typ_al = Mixfix::new(mixop(arena, value_mixop)?, typs(arena, value_typs)?)?;
    // Supply the canonical origin omitted from the meta representation
    let id_origin_al = phrase!(node: "".into(), span: span);
    let typ_origin_al =
        phrase!(node: ast::TypOriginKind { id: id_origin_al, targs: vec![] }, span: span);
    // Attach the decoded span and omit general hints
    Ok(ast::TypCase {
        not_typ: phrase!(node: mixfix_typ_al, span: span),
        typ_origin: typ_origin_al,
        hints: vec![],
    })
}
