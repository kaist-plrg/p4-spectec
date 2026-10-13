//! Canonical reconstruction of source expressions and their children
//!
//! Notes use bool as the omitted annotation. Operator types follow the
//! operator family (bool or nat), rather than pretending to recover typing.
//! A decoded SUB receives a conservative native Recurse plan.

use super::super::mixops as m;
use super::{atom, case, id, iter, list, mixop, opt, typ, typs, var};
use crate::{
    lang::{
        common::prim::{bool as boolean, num},
        data::{
            arena::Arena,
            notation::tree::Mixfix,
            value::flat::{Value, get},
        },
        il::ast,
    },
    metacirc::Error,
    note_phrase, phrase,
};
use num_traits::ToPrimitive;

/// Reconstructs an expression suitable for editing and re-encoding.
pub fn exp(arena: &Arena, value_meta: Value) -> Result<ast::Exp, Error> {
    use ast::ExpKind as E;
    let (mixop, values, span) = case(arena, value_meta)?;
    let exp_kind = match values.as_slice() {
        [value_bool] if m::is(&mixop, m::BOOL) => E::Bool(get::bool(arena, value_bool)?),
        [value_num] if m::is(&mixop, m::NAT) => {
            let num::Number::Nat(nat) = get::num(arena, value_num)? else {
                return Err(Error::unexpected(arena, *value_num, "a natural-number payload"));
            };
            E::Num(num::Number::Nat(nat.clone()))
        }
        [value_num] if m::is(&mixop, m::INT) => {
            E::Num(num::Number::Int(num::to_int(get::num(arena, value_num)?).clone()))
        }
        [value_text] if m::is(&mixop, m::TEXT) => E::Text(get::text(arena, value_text)?.into()),
        [value_id] if m::is(&mixop, m::VAR_EXP) => E::Id(id(arena, *value_id)?),
        [value_op, value_exp] if m::is(&mixop, m::UN) => {
            let (op, optyp) = unop(arena, *value_op)?;
            E::Un(op, optyp, Box::new(exp(arena, *value_exp)?))
        }
        [value_op, value_l, value_r] if m::is(&mixop, m::BIN) => {
            let (op, optyp) = binop(arena, *value_op)?;
            E::Bin(op, optyp, Box::new(exp(arena, *value_l)?), Box::new(exp(arena, *value_r)?))
        }
        [value_op, value_l, value_r] if m::is(&mixop, m::CMP) => {
            let (op, optyp) = cmpop(arena, *value_op)?;
            E::Cmp(op, optyp, Box::new(exp(arena, *value_l)?), Box::new(exp(arena, *value_r)?))
        }
        [value_typ, value_exp] if m::is(&mixop, m::UPCAST) => {
            E::UpCast(Box::new(typ(arena, *value_typ)?), Box::new(exp(arena, *value_exp)?))
        }
        [value_typ, value_exp] if m::is(&mixop, m::DOWNCAST) => {
            E::DownCast(Box::new(typ(arena, *value_typ)?), Box::new(exp(arena, *value_exp)?))
        }
        [value_exp, value_typ] if m::is(&mixop, m::SUB_EXP) => {
            let typ_al = typ(arena, *value_typ)?;
            let subcheck = ast::Subcheck::Recurse(typ_al.clone());
            E::Sub(Box::new(exp(arena, *value_exp)?), Box::new(typ_al), Box::new(subcheck))
        }
        [value_exp, value_pattern] if m::is(&mixop, m::MATCH) => {
            E::Match(Box::new(exp(arena, *value_exp)?), pattern(arena, *value_pattern)?)
        }
        [value_exps] if m::is(&mixop, m::TUP) => E::Tuple(exps(arena, *value_exps)?),
        [value_case] if m::is(&mixop, m::INJ) => E::Case(Box::new(expcase(arena, *value_case)?)),
        [value_fields] if m::is(&mixop, m::STR) => E::Str(list(arena, *value_fields, expfield)?),
        [value_opt] if m::is(&mixop, m::OPT) => E::Opt(opt(arena, *value_opt, exp)?.map(Box::new)),
        [value_exps] if m::is(&mixop, m::LIST) => E::List(exps(arena, *value_exps)?),
        [value_l, value_r] if m::is(&mixop, m::CONS) => {
            E::Cons(Box::new(exp(arena, *value_l)?), Box::new(exp(arena, *value_r)?))
        }
        [value_l, value_r] if m::is(&mixop, m::CAT) => {
            E::Cat(Box::new(exp(arena, *value_l)?), Box::new(exp(arena, *value_r)?))
        }
        [value_l, value_r] if m::is(&mixop, m::MEM) => {
            E::Mem(Box::new(exp(arena, *value_l)?), Box::new(exp(arena, *value_r)?))
        }
        [value_exp] if m::is(&mixop, m::LEN) => E::Len(Box::new(exp(arena, *value_exp)?)),
        [value_exp, value_atom] if m::is(&mixop, m::DOT) => {
            E::Dot(Box::new(exp(arena, *value_exp)?), atom(arena, *value_atom)?)
        }
        [value_base, value_idx] if m::is(&mixop, m::IDX) => {
            E::Idx(Box::new(exp(arena, *value_base)?), Box::new(exp(arena, *value_idx)?))
        }
        [value_base, value_idx, value_len] if m::is(&mixop, m::SLICE) => E::Slice(
            Box::new(exp(arena, *value_base)?),
            Box::new(exp(arena, *value_idx)?),
            Box::new(exp(arena, *value_len)?),
        ),
        [value_base, value_path, value_exp] if m::is(&mixop, m::UPD) => E::Upd(
            Box::new(exp(arena, *value_base)?),
            Box::new(path(arena, *value_path)?),
            Box::new(exp(arena, *value_exp)?),
        ),
        [value_id, value_typs, value_args] if m::is(&mixop, m::CALL) => {
            E::Call(id(arena, *value_id)?, typs(arena, *value_typs)?, args(arena, *value_args)?)
        }
        [value_exp, value_iter] if m::is(&mixop, m::ITER_EXP) => {
            E::Iter(Box::new(exp(arena, *value_exp)?), iterexp(arena, *value_iter)?)
        }
        _ => {
            return Err(Error::unexpected(
                arena,
                value_meta,
                "an AL expression constructor with its declared arguments",
            ));
        }
    };
    Ok(note_phrase!(node: exp_kind, note: ast::TypKind::Bool, span: span))
}

/// Decodes an owned source expression list.
pub fn exps(arena: &Arena, value_meta: Value) -> Result<Vec<ast::Exp>, Error> {
    list(arena, value_meta, exp)
}

fn expcase(arena: &Arena, value_meta: Value) -> Result<ast::NotExp, Error> {
    let (mixop_meta, values, _) = case(arena, value_meta)?;
    match values.as_slice() {
        [value_mixop, value_exps] if m::is(&mixop_meta, m::EXPCASE) => {
            Ok(Mixfix::new(mixop(arena, *value_mixop)?, exps(arena, *value_exps)?)?)
        }
        _ => Err(Error::unexpected(arena, value_meta, "mixop exp*")),
    }
}

fn expfield(arena: &Arena, value_meta: Value) -> Result<ast::ExpField, Error> {
    let (mixop, values, _) = case(arena, value_meta)?;
    match values.as_slice() {
        [value_atom, value_exp] if m::is(&mixop, m::EXPFIELD) => {
            Ok(ast::ExpField { atom: atom(arena, *value_atom)?, exp: exp(arena, *value_exp)? })
        }
        _ => Err(Error::unexpected(arena, value_meta, "atom exp")),
    }
}

fn iterexp(arena: &Arena, value_meta: Value) -> Result<ast::ExpIter, Error> {
    let (mixop, values, _) = case(arena, value_meta)?;
    match values.as_slice() {
        [value_iter, value_vars] if m::is(&mixop, m::ITEREXP) => Ok(ast::ExpIter {
            iter: iter(arena, *value_iter)?,
            vars: list(arena, *value_vars, var)?,
        }),
        _ => Err(Error::unexpected(arena, value_meta, "iter vari*")),
    }
}

/// Decodes a value or function argument.
pub(super) fn arg(arena: &Arena, value_meta: Value) -> Result<ast::Arg, Error> {
    let (mixop, values, span) = case(arena, value_meta)?;
    let arg_kind = match values.as_slice() {
        [value_exp] if m::is(&mixop, m::EXP_ARG) => {
            ast::ArgKind::Exp(Box::new(exp(arena, *value_exp)?))
        }
        [value_id] if m::is(&mixop, m::FUN_ARG) => ast::ArgKind::Def(id(arena, *value_id)?),
        _ => return Err(Error::unexpected(arena, value_meta, "EXP exp or FUN id")),
    };
    Ok(phrase!(node: arg_kind, span: span))
}

/// Decodes an owned argument list.
pub(super) fn args(arena: &Arena, value_meta: Value) -> Result<Vec<ast::Arg>, Error> {
    list(arena, value_meta, arg)
}

fn pattern(arena: &Arena, value_meta: Value) -> Result<ast::Pattern, Error> {
    use ast::{ListPattern as L, OptPattern as O, Pattern as P};
    let (mixop_meta, values, _) = case(arena, value_meta)?;
    Ok(match values.as_slice() {
        [value_mixop] if m::is(&mixop_meta, m::CASE_PATTERN) => {
            P::Case(mixop(arena, *value_mixop)?)
        }
        [] if m::is(&mixop_meta, m::CONS_PATTERN) => P::List(L::Cons),
        [value_nat] if m::is(&mixop_meta, m::FIXED) => {
            let num::Number::Nat(nat) = get::num(arena, value_nat)? else {
                return Err(Error::unexpected(arena, *value_nat, "a natural list length"));
            };
            let len = nat.as_bigint().to_usize().ok_or_else(|| {
                Error::unexpected(arena, *value_nat, "a list length fitting usize")
            })?;
            P::List(L::Fixed(len))
        }
        [] if m::is(&mixop_meta, m::NIL) => P::List(L::Nil),
        [] if m::is(&mixop_meta, m::SOME) => P::Opt(O::Some),
        [] if m::is(&mixop_meta, m::NONE) => P::Opt(O::None),
        _ => return Err(Error::unexpected(arena, value_meta, "an AL shape pattern")),
    })
}

fn path(arena: &Arena, value_meta: Value) -> Result<ast::Path, Error> {
    let (mixop, values, span) = case(arena, value_meta)?;
    let path_kind = match values.as_slice() {
        [] if m::is(&mixop, m::ROOT) => ast::PathKind::Root,
        [value_path, value_idx] if m::is(&mixop, m::IDX) => ast::PathKind::Idx(
            Box::new(path(arena, *value_path)?),
            Box::new(exp(arena, *value_idx)?),
        ),
        [value_path, value_idx, value_len] if m::is(&mixop, m::SLICE) => ast::PathKind::Slice(
            Box::new(path(arena, *value_path)?),
            Box::new(exp(arena, *value_idx)?),
            Box::new(exp(arena, *value_len)?),
        ),
        [value_path, value_atom] if m::is(&mixop, m::DOT) => {
            ast::PathKind::Dot(Box::new(path(arena, *value_path)?), atom(arena, *value_atom)?)
        }
        _ => return Err(Error::unexpected(arena, value_meta, "an AL update path")),
    };
    Ok(note_phrase!(node: path_kind, note: ast::TypKind::Bool, span: span))
}

fn unop(arena: &Arena, value_meta: Value) -> Result<(ast::UnOp, ast::OpTyp), Error> {
    let (mixop, values, _) = case(arena, value_meta)?;
    match values.as_slice() {
        [] if m::is(&mixop, m::NOT) => Ok((ast::UnOp::Bool(boolean::UnOp::Not), ast::OpTyp::Bool)),
        [] if m::is(&mixop, m::PLUS) => Ok((ast::UnOp::Num(num::UnOp::Plus), ast::OpTyp::Nat)),
        [] if m::is(&mixop, m::MINUS) => Ok((ast::UnOp::Num(num::UnOp::Minus), ast::OpTyp::Nat)),
        _ => Err(Error::unexpected(arena, value_meta, "a nullary unop constructor")),
    }
}

fn binop(arena: &Arena, value_meta: Value) -> Result<(ast::BinOp, ast::OpTyp), Error> {
    let (mixop, values, _) = case(arena, value_meta)?;
    match values.as_slice() {
        [] if m::is(&mixop, m::AND) => {
            Ok((ast::BinOp::Bool(boolean::BinOp::And), ast::OpTyp::Bool))
        }
        [] if m::is(&mixop, m::OR) => Ok((ast::BinOp::Bool(boolean::BinOp::Or), ast::OpTyp::Bool)),
        [] if m::is(&mixop, m::IMPL) => {
            Ok((ast::BinOp::Bool(boolean::BinOp::Impl), ast::OpTyp::Bool))
        }
        [] if m::is(&mixop, m::EQUIV) => {
            Ok((ast::BinOp::Bool(boolean::BinOp::Equiv), ast::OpTyp::Bool))
        }
        [] if m::is(&mixop, m::ADD) => Ok((ast::BinOp::Num(num::BinOp::Add), ast::OpTyp::Nat)),
        [] if m::is(&mixop, m::SUB) => Ok((ast::BinOp::Num(num::BinOp::Sub), ast::OpTyp::Nat)),
        [] if m::is(&mixop, m::MUL) => Ok((ast::BinOp::Num(num::BinOp::Mul), ast::OpTyp::Nat)),
        [] if m::is(&mixop, m::DIV) => Ok((ast::BinOp::Num(num::BinOp::Div), ast::OpTyp::Nat)),
        [] if m::is(&mixop, m::MOD) => Ok((ast::BinOp::Num(num::BinOp::Mod), ast::OpTyp::Nat)),
        [] if m::is(&mixop, m::POW) => Ok((ast::BinOp::Num(num::BinOp::Pow), ast::OpTyp::Nat)),
        _ => Err(Error::unexpected(arena, value_meta, "a nullary binop constructor")),
    }
}

fn cmpop(arena: &Arena, value_meta: Value) -> Result<(ast::CmpOp, ast::OpTyp), Error> {
    let (mixop, values, _) = case(arena, value_meta)?;
    match values.as_slice() {
        [] if m::is(&mixop, m::EQ) => Ok((ast::CmpOp::Bool(boolean::CmpOp::Eq), ast::OpTyp::Bool)),
        [] if m::is(&mixop, m::NE) => Ok((ast::CmpOp::Bool(boolean::CmpOp::Ne), ast::OpTyp::Bool)),
        [] if m::is(&mixop, m::LT) => Ok((ast::CmpOp::Num(num::CmpOp::Lt), ast::OpTyp::Nat)),
        [] if m::is(&mixop, m::LE) => Ok((ast::CmpOp::Num(num::CmpOp::Le), ast::OpTyp::Nat)),
        [] if m::is(&mixop, m::GT) => Ok((ast::CmpOp::Num(num::CmpOp::Gt), ast::OpTyp::Nat)),
        [] if m::is(&mixop, m::GE) => Ok((ast::CmpOp::Num(num::CmpOp::Ge), ast::OpTyp::Nat)),
        _ => Err(Error::unexpected(arena, value_meta, "a nullary cmpop constructor")),
    }
}
