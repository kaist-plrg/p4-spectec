//! Encoding of AL types and type definitions
//!
//! `typ` retains binders, parameter types, and return types
//! `deftyp` encodes aliases, structures, and variants
//! Type origins and general hints have no fields in the meta syntax

use super::super::mixops as m;
use super::{atom, case, id, iter, list, mixop};
use crate::{
    lang::{
        common::{prim::num, source::Span},
        data::{arena::Arena, value::flat::Value},
        il::ast,
    },
    metacirc::Error,
};

/// Encodes a source type, including complete function signatures.
pub fn typ(arena: &mut Arena, typ_al: &ast::Typ) -> Result<Value, Error> {
    let span = typ_al.span;
    match &typ_al.node {
        ast::TypKind::Bool => {
            case(arena, m::BOOL_TYP, "typ", vec![], span).map_err(|error| error.at(span))
        }
        ast::TypKind::Num(num::Typ::Nat) => {
            case(arena, m::NAT_TYP, "typ", vec![], span).map_err(|error| error.at(span))
        }
        ast::TypKind::Num(num::Typ::Int) => {
            case(arena, m::INT_TYP, "typ", vec![], span).map_err(|error| error.at(span))
        }
        ast::TypKind::Text => {
            case(arena, m::TEXT_TYP, "typ", vec![], span).map_err(|error| error.at(span))
        }
        ast::TypKind::Var(id_al, targs_al) => encode_var_typ(arena, id_al, targs_al, span),
        ast::TypKind::Tuple(typs_al) => encode_tuple_typ(arena, typs_al, span),
        ast::TypKind::Iter(typ_inner_al, iter_al) => {
            encode_iter_typ(arena, typ_inner_al, iter_al, span)
        }
        ast::TypKind::Func(typ_func_al) => encode_func_typ(arena, typ_func_al, span),
    }
}

/// Encodes a named type and its arguments.
fn encode_var_typ(
    arena: &mut Arena,
    id_al: &ast::Id,
    targs_al: &[ast::Typ],
    span: Span,
) -> Result<Value, Error> {
    let values_meta = vec![id(arena, id_al)?, typs(arena, targs_al)?];
    case(arena, m::VAR_TYP, "typ", values_meta, span).map_err(|error| error.at(span))
}

/// Encodes the component types of a tuple.
fn encode_tuple_typ(arena: &mut Arena, typs_al: &[ast::Typ], span: Span) -> Result<Value, Error> {
    let value_typs = typs(arena, typs_al)?;
    case(arena, m::TUP_TYP, "typ", vec![value_typs], span).map_err(|error| error.at(span))
}

/// Encodes an iterated type and its dimension.
fn encode_iter_typ(
    arena: &mut Arena,
    typ_inner_al: &ast::Typ,
    iter_al: &crate::lang::common::Iter,
    span: Span,
) -> Result<Value, Error> {
    let values_meta = vec![typ(arena, typ_inner_al)?, iter(arena, iter_al)?];
    case(arena, m::ITER_TYP, "typ", values_meta, span).map_err(|error| error.at(span))
}

/// Encodes a complete function type.
fn encode_func_typ(
    arena: &mut Arena,
    typ_func_al: &ast::FuncTyp,
    span: Span,
) -> Result<Value, Error> {
    let values_meta = vec![
        list(arena, "id", &typ_func_al.tparams, id)?,
        typs(arena, &typ_func_al.typs_params)?,
        typ(arena, &typ_func_al.typ_ret)?,
    ];
    case(arena, m::FUNC_TYP, "typ", values_meta, span).map_err(|error| error.at(span))
}

/// Encodes a list of source types.
pub fn typs(arena: &mut Arena, typs_al: &[ast::Typ]) -> Result<Value, Error> {
    list(arena, "typ", typs_al, typ)
}

/// Encodes an alias, struct, or variant definition.
pub fn deftyp(arena: &mut Arena, deftyp_al: &ast::DefTyp) -> Result<Value, Error> {
    let (shape, values_meta) = match &deftyp_al.node {
        ast::DefTypKind::Plain(typ_al) => (m::ALIAS, vec![typ(arena, typ_al)?]),
        ast::DefTypKind::Struct(typ_fields_al) => {
            (m::STRUCT, vec![list(arena, "typfield", typ_fields_al, typfield)?])
        }
        ast::DefTypKind::Variant(typ_cases_al) => {
            (m::VARIANT, vec![list(arena, "typcase", typ_cases_al, typcase)?])
        }
    };
    case(arena, shape, "deftyp", values_meta, deftyp_al.span)
}

/// Encodes one struct type field.
fn typfield(arena: &mut Arena, typ_field_al: &ast::TypField) -> Result<Value, Error> {
    let values_meta = vec![atom(arena, &typ_field_al.atom)?, typ(arena, &typ_field_al.typ)?];
    case(arena, m::TYPFIELD, "typfield", values_meta, typ_field_al.atom.span)
}

/// Encodes one variant case without its origin or hints.
fn typcase(arena: &mut Arena, typ_case_al: &ast::TypCase) -> Result<Value, Error> {
    let values_meta = vec![
        mixop(arena, typ_case_al.not_typ.node.mixop())?,
        typs(arena, typ_case_al.not_typ.node.args())?,
    ];
    case(arena, m::TYPCASE, "typcase", values_meta, typ_case_al.not_typ.span)
}
