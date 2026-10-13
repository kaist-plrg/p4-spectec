//! Encoding of AL types and type definitions
//!
//! Function types retain their binders, parameter types, and return type.
//! Type origins and general hints have no fields in the meta syntax.

use super::super::mixops as m;
use super::{atom, case, id, iter, list, mixop};
use crate::{
    lang::{
        common::prim::num,
        data::{arena::Arena, value::flat::Value},
        il::ast,
    },
    metacirc::Error,
};

/// Encodes a source type, including complete function signatures.
pub fn typ(arena: &mut Arena, typ_al: &ast::Typ) -> Result<Value, Error> {
    let (shape, values) = match &typ_al.node {
        ast::TypKind::Bool => (m::BOOL_TYP, vec![]),
        ast::TypKind::Num(num::Typ::Nat) => (m::NAT_TYP, vec![]),
        ast::TypKind::Num(num::Typ::Int) => (m::INT_TYP, vec![]),
        ast::TypKind::Text => (m::TEXT_TYP, vec![]),
        ast::TypKind::Var(id_al, targs) => {
            (m::VAR_TYP, vec![id(arena, id_al)?, typs(arena, targs)?])
        }
        ast::TypKind::Tuple(typs_al) => (m::TUP_TYP, vec![typs(arena, typs_al)?]),
        ast::TypKind::Iter(typ_inner, iter_al) => {
            (m::ITER_TYP, vec![typ(arena, typ_inner)?, iter(arena, iter_al)?])
        }
        ast::TypKind::Func(func_typ) => (
            m::FUNC_TYP,
            vec![
                list(arena, "id", &func_typ.tparams, id)?,
                typs(arena, &func_typ.typs_params)?,
                typ(arena, &func_typ.typ_ret)?,
            ],
        ),
    };
    case(arena, shape, "typ", values, typ_al.span).map_err(|error| error.at(typ_al.span))
}

/// Encodes a list of source types.
pub fn typs(arena: &mut Arena, typs_al: &[ast::Typ]) -> Result<Value, Error> {
    list(arena, "typ", typs_al, typ)
}

/// Encodes an alias, struct, or variant definition.
pub fn deftyp(arena: &mut Arena, deftyp_al: &ast::DefTyp) -> Result<Value, Error> {
    let (shape, values) = match &deftyp_al.node {
        ast::DefTypKind::Plain(typ_al) => (m::ALIAS, vec![typ(arena, typ_al)?]),
        ast::DefTypKind::Struct(typ_fields) => {
            (m::STRUCT, vec![list(arena, "typfield", typ_fields, typfield)?])
        }
        ast::DefTypKind::Variant(typ_cases) => {
            (m::VARIANT, vec![list(arena, "typcase", typ_cases, typcase)?])
        }
    };
    case(arena, shape, "deftyp", values, deftyp_al.span)
}

fn typfield(arena: &mut Arena, typ_field: &ast::TypField) -> Result<Value, Error> {
    let values = vec![atom(arena, &typ_field.atom)?, typ(arena, &typ_field.typ)?];
    case(arena, m::TYPFIELD, "typfield", values, typ_field.atom.span)
}

fn typcase(arena: &mut Arena, typ_case: &ast::TypCase) -> Result<Value, Error> {
    let values = vec![
        mixop(arena, typ_case.not_typ.node.mixop())?,
        typs(arena, typ_case.not_typ.node.args())?,
    ];
    case(arena, m::TYPCASE, "typcase", values, typ_case.not_typ.span)
}
