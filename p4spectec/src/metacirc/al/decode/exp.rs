//! Canonical reconstruction of source expressions and their children
//!
//! `exp` and `exps` rebuild source expressions with bool notes
//! Operator types follow the operator family (bool or nat)
//! A decoded SUB receives a conservative native Recurse plan

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
    let (mixop_meta, values_meta, span) = case(arena, value_meta)?;
    let exp_kind_al = match values_meta.as_slice() {
        [value_bool] if m::is(&mixop_meta, m::BOOL) => {
            ast::ExpKind::Bool(get::bool(arena, value_bool)?)
        }
        [value_num] if m::is(&mixop_meta, m::NAT) => decode_nat_exp(arena, *value_num)?,
        [value_num] if m::is(&mixop_meta, m::INT) => decode_int_exp(arena, *value_num)?,
        [value_text] if m::is(&mixop_meta, m::TEXT) => {
            ast::ExpKind::Text(get::text(arena, value_text)?.into())
        }
        [value_id] if m::is(&mixop_meta, m::VAR_EXP) => decode_id_exp(arena, *value_id)?,
        [value_op, value_exp] if m::is(&mixop_meta, m::UN) => {
            decode_un_exp(arena, *value_op, *value_exp)?
        }
        [value_op, value_l, value_r] if m::is(&mixop_meta, m::BIN) => {
            decode_bin_exp(arena, *value_op, *value_l, *value_r)?
        }
        [value_op, value_l, value_r] if m::is(&mixop_meta, m::CMP) => {
            decode_cmp_exp(arena, *value_op, *value_l, *value_r)?
        }
        [value_typ, value_exp] if m::is(&mixop_meta, m::UPCAST) => {
            decode_upcast_exp(arena, *value_typ, *value_exp)?
        }
        [value_typ, value_exp] if m::is(&mixop_meta, m::DOWNCAST) => {
            decode_downcast_exp(arena, *value_typ, *value_exp)?
        }
        [value_exp, value_typ] if m::is(&mixop_meta, m::SUB_EXP) => {
            decode_sub_exp(arena, *value_exp, *value_typ)?
        }
        [value_exp, value_pattern] if m::is(&mixop_meta, m::MATCH) => {
            decode_match_exp(arena, *value_exp, *value_pattern)?
        }
        [value_exps] if m::is(&mixop_meta, m::TUP) => decode_tuple_exp(arena, *value_exps)?,
        [value_case] if m::is(&mixop_meta, m::INJ) => decode_case_exp(arena, *value_case)?,
        [value_fields] if m::is(&mixop_meta, m::STR) => decode_struct_exp(arena, *value_fields)?,
        [value_opt] if m::is(&mixop_meta, m::OPT) => decode_opt_exp(arena, *value_opt)?,
        [value_exps] if m::is(&mixop_meta, m::LIST) => decode_list_exp(arena, *value_exps)?,
        [value_head, value_tail] if m::is(&mixop_meta, m::CONS) => {
            decode_cons_exp(arena, *value_head, *value_tail)?
        }
        [value_l, value_r] if m::is(&mixop_meta, m::CAT) => {
            decode_cat_exp(arena, *value_l, *value_r)?
        }
        [value_elem, value_list] if m::is(&mixop_meta, m::MEM) => {
            decode_mem_exp(arena, *value_elem, *value_list)?
        }
        [value_exp] if m::is(&mixop_meta, m::LEN) => decode_len_exp(arena, *value_exp)?,
        [value_base, value_atom] if m::is(&mixop_meta, m::DOT) => {
            decode_dot_exp(arena, *value_base, *value_atom)?
        }
        [value_base, value_idx] if m::is(&mixop_meta, m::IDX) => {
            decode_idx_exp(arena, *value_base, *value_idx)?
        }
        [value_base, value_idx, value_len] if m::is(&mixop_meta, m::SLICE) => {
            decode_slice_exp(arena, *value_base, *value_idx, *value_len)?
        }
        [value_base, value_path, value_upd] if m::is(&mixop_meta, m::UPD) => {
            decode_upd_exp(arena, *value_base, *value_path, *value_upd)?
        }
        [value_id, value_typs, value_args] if m::is(&mixop_meta, m::CALL) => {
            decode_call_exp(arena, *value_id, *value_typs, *value_args)?
        }
        [value_exp, value_iter] if m::is(&mixop_meta, m::ITER_EXP) => {
            decode_iter_exp(arena, *value_exp, *value_iter)?
        }
        _ => {
            return Err(Error::unexpected(
                arena,
                value_meta,
                "an AL expression constructor with its declared arguments",
            ));
        }
    };
    Ok(note_phrase!(node: exp_kind_al, note: ast::TypKind::Bool, span: span))
}

/// Rejects non-natural payloads before rebuilding a numeric expression.
fn decode_nat_exp(arena: &Arena, value_num: Value) -> Result<ast::ExpKind, Error> {
    let num::Number::Nat(nat) = get::num(arena, &value_num)? else {
        return Err(Error::unexpected(arena, value_num, "a natural-number payload"));
    };
    Ok(ast::ExpKind::Num(num::Number::Nat(nat.clone())))
}

/// Converts a numeric payload to the canonical integer expression.
fn decode_int_exp(arena: &Arena, value_num: Value) -> Result<ast::ExpKind, Error> {
    Ok(ast::ExpKind::Num(num::Number::Int(num::to_int(get::num(arena, &value_num)?).clone())))
}

fn decode_id_exp(arena: &Arena, value_id: Value) -> Result<ast::ExpKind, Error> {
    Ok(ast::ExpKind::Id(id(arena, value_id)?))
}

fn decode_un_exp(arena: &Arena, value_op: Value, value_exp: Value) -> Result<ast::ExpKind, Error> {
    let (op_al, typ_op_al) = unop(arena, value_op)?;
    Ok(ast::ExpKind::Un(op_al, typ_op_al, Box::new(exp(arena, value_exp)?)))
}

/// Reconstructs a binary expression with its canonical operator type.
fn decode_bin_exp(
    arena: &Arena,
    value_op: Value,
    value_l: Value,
    value_r: Value,
) -> Result<ast::ExpKind, Error> {
    let (op_al, typ_op_al) = binop(arena, value_op)?;
    Ok(ast::ExpKind::Bin(
        op_al,
        typ_op_al,
        Box::new(exp(arena, value_l)?),
        Box::new(exp(arena, value_r)?),
    ))
}

/// Reconstructs a comparison with its canonical operator type.
fn decode_cmp_exp(
    arena: &Arena,
    value_op: Value,
    value_l: Value,
    value_r: Value,
) -> Result<ast::ExpKind, Error> {
    let (op_al, typ_op_al) = cmpop(arena, value_op)?;
    Ok(ast::ExpKind::Cmp(
        op_al,
        typ_op_al,
        Box::new(exp(arena, value_l)?),
        Box::new(exp(arena, value_r)?),
    ))
}

fn decode_upcast_exp(
    arena: &Arena,
    value_typ: Value,
    value_exp: Value,
) -> Result<ast::ExpKind, Error> {
    Ok(ast::ExpKind::UpCast(Box::new(typ(arena, value_typ)?), Box::new(exp(arena, value_exp)?)))
}

fn decode_downcast_exp(
    arena: &Arena,
    value_typ: Value,
    value_exp: Value,
) -> Result<ast::ExpKind, Error> {
    Ok(ast::ExpKind::DownCast(Box::new(typ(arena, value_typ)?), Box::new(exp(arena, value_exp)?)))
}

/// Rebuilds a subtype test with a conservative recursive check.
fn decode_sub_exp(
    arena: &Arena,
    value_exp: Value,
    value_typ: Value,
) -> Result<ast::ExpKind, Error> {
    let typ_al = typ(arena, value_typ)?;
    let subcheck_al = ast::Subcheck::Recurse(typ_al.clone());
    Ok(ast::ExpKind::Sub(Box::new(exp(arena, value_exp)?), Box::new(typ_al), Box::new(subcheck_al)))
}

fn decode_match_exp(
    arena: &Arena,
    value_exp: Value,
    value_pattern: Value,
) -> Result<ast::ExpKind, Error> {
    Ok(ast::ExpKind::Match(Box::new(exp(arena, value_exp)?), pattern(arena, value_pattern)?))
}

fn decode_tuple_exp(arena: &Arena, value_exps: Value) -> Result<ast::ExpKind, Error> {
    Ok(ast::ExpKind::Tuple(exps(arena, value_exps)?))
}

fn decode_case_exp(arena: &Arena, value_case: Value) -> Result<ast::ExpKind, Error> {
    Ok(ast::ExpKind::Case(Box::new(expcase(arena, value_case)?)))
}

fn decode_struct_exp(arena: &Arena, value_fields: Value) -> Result<ast::ExpKind, Error> {
    Ok(ast::ExpKind::Str(list(arena, value_fields, expfield)?))
}

fn decode_opt_exp(arena: &Arena, value_opt: Value) -> Result<ast::ExpKind, Error> {
    Ok(ast::ExpKind::Opt(opt(arena, value_opt, exp)?.map(Box::new)))
}

fn decode_list_exp(arena: &Arena, value_exps: Value) -> Result<ast::ExpKind, Error> {
    Ok(ast::ExpKind::List(exps(arena, value_exps)?))
}

fn decode_cons_exp(
    arena: &Arena,
    value_head: Value,
    value_tail: Value,
) -> Result<ast::ExpKind, Error> {
    Ok(ast::ExpKind::Cons(Box::new(exp(arena, value_head)?), Box::new(exp(arena, value_tail)?)))
}

fn decode_cat_exp(arena: &Arena, value_l: Value, value_r: Value) -> Result<ast::ExpKind, Error> {
    Ok(ast::ExpKind::Cat(Box::new(exp(arena, value_l)?), Box::new(exp(arena, value_r)?)))
}

fn decode_mem_exp(
    arena: &Arena,
    value_elem: Value,
    value_list: Value,
) -> Result<ast::ExpKind, Error> {
    Ok(ast::ExpKind::Mem(Box::new(exp(arena, value_elem)?), Box::new(exp(arena, value_list)?)))
}

fn decode_len_exp(arena: &Arena, value_exp: Value) -> Result<ast::ExpKind, Error> {
    Ok(ast::ExpKind::Len(Box::new(exp(arena, value_exp)?)))
}

fn decode_dot_exp(
    arena: &Arena,
    value_base: Value,
    value_atom: Value,
) -> Result<ast::ExpKind, Error> {
    Ok(ast::ExpKind::Dot(Box::new(exp(arena, value_base)?), atom(arena, value_atom)?))
}

fn decode_idx_exp(
    arena: &Arena,
    value_base: Value,
    value_idx: Value,
) -> Result<ast::ExpKind, Error> {
    Ok(ast::ExpKind::Idx(Box::new(exp(arena, value_base)?), Box::new(exp(arena, value_idx)?)))
}

/// Reconstructs a slice from its base, index, and length expressions.
fn decode_slice_exp(
    arena: &Arena,
    value_base: Value,
    value_idx: Value,
    value_len: Value,
) -> Result<ast::ExpKind, Error> {
    Ok(ast::ExpKind::Slice(
        Box::new(exp(arena, value_base)?),
        Box::new(exp(arena, value_idx)?),
        Box::new(exp(arena, value_len)?),
    ))
}

/// Reconstructs an update from its base, path, and replacement.
fn decode_upd_exp(
    arena: &Arena,
    value_base: Value,
    value_path: Value,
    value_upd: Value,
) -> Result<ast::ExpKind, Error> {
    Ok(ast::ExpKind::Upd(
        Box::new(exp(arena, value_base)?),
        Box::new(path(arena, value_path)?),
        Box::new(exp(arena, value_upd)?),
    ))
}

fn decode_call_exp(
    arena: &Arena,
    value_id: Value,
    value_typs: Value,
    value_args: Value,
) -> Result<ast::ExpKind, Error> {
    Ok(ast::ExpKind::Call(id(arena, value_id)?, typs(arena, value_typs)?, args(arena, value_args)?))
}

fn decode_iter_exp(
    arena: &Arena,
    value_exp: Value,
    value_iter: Value,
) -> Result<ast::ExpKind, Error> {
    Ok(ast::ExpKind::Iter(Box::new(exp(arena, value_exp)?), iterexp(arena, value_iter)?))
}

/// Decodes an owned source expression list.
pub fn exps(arena: &Arena, value_meta: Value) -> Result<Vec<ast::Exp>, Error> {
    list(arena, value_meta, exp)
}

/// Decodes a notation and its expression arguments.
fn expcase(arena: &Arena, value_meta: Value) -> Result<ast::NotExp, Error> {
    let (mixop_meta, values_meta, _) = case(arena, value_meta)?;
    match values_meta.as_slice() {
        [value_mixop, value_exps] if m::is(&mixop_meta, m::EXPCASE) => {
            decode_expcase(arena, *value_mixop, *value_exps)
        }
        _ => Err(Error::unexpected(arena, value_meta, "mixop exp*")),
    }
}

fn decode_expcase(
    arena: &Arena,
    value_mixop: Value,
    value_exps: Value,
) -> Result<ast::NotExp, Error> {
    Ok(Mixfix::new(mixop(arena, value_mixop)?, exps(arena, value_exps)?)?)
}

/// Decodes one source struct-expression field.
fn expfield(arena: &Arena, value_meta: Value) -> Result<ast::ExpField, Error> {
    let (mixop_meta, values_meta, _) = case(arena, value_meta)?;
    match values_meta.as_slice() {
        [value_atom, value_exp] if m::is(&mixop_meta, m::EXPFIELD) => {
            decode_expfield(arena, *value_atom, *value_exp)
        }
        _ => Err(Error::unexpected(arena, value_meta, "atom exp")),
    }
}

fn decode_expfield(
    arena: &Arena,
    value_atom: Value,
    value_exp: Value,
) -> Result<ast::ExpField, Error> {
    Ok(ast::ExpField { atom: atom(arena, value_atom)?, exp: exp(arena, value_exp)? })
}

/// Decodes an expression iterator and its variables.
fn iterexp(arena: &Arena, value_meta: Value) -> Result<ast::ExpIter, Error> {
    let (mixop_meta, values_meta, _) = case(arena, value_meta)?;
    match values_meta.as_slice() {
        [value_iter, value_vars] if m::is(&mixop_meta, m::ITEREXP) => {
            decode_iterexp(arena, *value_iter, *value_vars)
        }
        _ => Err(Error::unexpected(arena, value_meta, "iter vari*")),
    }
}

fn decode_iterexp(
    arena: &Arena,
    value_iter: Value,
    value_vars: Value,
) -> Result<ast::ExpIter, Error> {
    Ok(ast::ExpIter { iter: iter(arena, value_iter)?, vars: list(arena, value_vars, var)? })
}

/// Decodes a value or function argument.
pub(super) fn arg(arena: &Arena, value_meta: Value) -> Result<ast::Arg, Error> {
    let (mixop_meta, values_meta, span) = case(arena, value_meta)?;
    let arg_kind_al = match values_meta.as_slice() {
        [value_exp] if m::is(&mixop_meta, m::EXP_ARG) => decode_exp_arg(arena, *value_exp)?,
        [value_id] if m::is(&mixop_meta, m::FUN_ARG) => decode_func_arg(arena, *value_id)?,
        _ => return Err(Error::unexpected(arena, value_meta, "EXP exp or FUN id")),
    };
    Ok(phrase!(node: arg_kind_al, span: span))
}

fn decode_exp_arg(arena: &Arena, value_exp: Value) -> Result<ast::ArgKind, Error> {
    Ok(ast::ArgKind::Exp(Box::new(exp(arena, value_exp)?)))
}

fn decode_func_arg(arena: &Arena, value_id: Value) -> Result<ast::ArgKind, Error> {
    Ok(ast::ArgKind::Def(id(arena, value_id)?))
}

/// Decodes an owned argument list.
pub(super) fn args(arena: &Arena, value_meta: Value) -> Result<Vec<ast::Arg>, Error> {
    list(arena, value_meta, arg)
}

/// Decodes a case, list, or option shape pattern.
fn pattern(arena: &Arena, value_meta: Value) -> Result<ast::Pattern, Error> {
    let (mixop_meta, values_meta, _) = case(arena, value_meta)?;
    Ok(match values_meta.as_slice() {
        [value_mixop] if m::is(&mixop_meta, m::CASE_PATTERN) => {
            decode_case_pattern(arena, *value_mixop)?
        }
        [] if m::is(&mixop_meta, m::CONS_PATTERN) => ast::Pattern::List(ast::ListPattern::Cons),
        [value_nat] if m::is(&mixop_meta, m::FIXED) => decode_fixed_pattern(arena, *value_nat)?,
        [] if m::is(&mixop_meta, m::NIL) => ast::Pattern::List(ast::ListPattern::Nil),
        [] if m::is(&mixop_meta, m::SOME) => ast::Pattern::Opt(ast::OptPattern::Some),
        [] if m::is(&mixop_meta, m::NONE) => ast::Pattern::Opt(ast::OptPattern::None),
        _ => return Err(Error::unexpected(arena, value_meta, "an AL shape pattern")),
    })
}

fn decode_case_pattern(arena: &Arena, value_mixop: Value) -> Result<ast::Pattern, Error> {
    Ok(ast::Pattern::Case(mixop(arena, value_mixop)?))
}

/// Checks that a fixed-list length fits the native pattern representation.
fn decode_fixed_pattern(arena: &Arena, value_nat: Value) -> Result<ast::Pattern, Error> {
    // Require a natural payload before checking its native size
    let num::Number::Nat(nat) = get::num(arena, &value_nat)? else {
        return Err(Error::unexpected(arena, value_nat, "a natural list length"));
    };
    // Reject lengths that cannot be represented by the host
    let num_len = nat
        .as_bigint()
        .to_usize()
        .ok_or_else(|| Error::unexpected(arena, value_nat, "a list length fitting usize"))?;
    Ok(ast::Pattern::List(ast::ListPattern::Fixed(num_len)))
}

/// Decodes a source update path.
fn path(arena: &Arena, value_meta: Value) -> Result<ast::Path, Error> {
    let (mixop_meta, values_meta, span) = case(arena, value_meta)?;
    let path_kind_al = match values_meta.as_slice() {
        [] if m::is(&mixop_meta, m::ROOT) => ast::PathKind::Root,
        [value_path, value_idx] if m::is(&mixop_meta, m::IDX) => {
            decode_idx_path(arena, *value_path, *value_idx)?
        }
        [value_path, value_idx, value_len] if m::is(&mixop_meta, m::SLICE) => {
            decode_slice_path(arena, *value_path, *value_idx, *value_len)?
        }
        [value_path, value_atom] if m::is(&mixop_meta, m::DOT) => {
            decode_dot_path(arena, *value_path, *value_atom)?
        }
        _ => return Err(Error::unexpected(arena, value_meta, "an AL update path")),
    };
    Ok(note_phrase!(node: path_kind_al, note: ast::TypKind::Bool, span: span))
}

fn decode_idx_path(
    arena: &Arena,
    value_path: Value,
    value_idx: Value,
) -> Result<ast::PathKind, Error> {
    Ok(ast::PathKind::Idx(Box::new(path(arena, value_path)?), Box::new(exp(arena, value_idx)?)))
}

/// Reconstructs a slice path and its bound expressions.
fn decode_slice_path(
    arena: &Arena,
    value_path: Value,
    value_idx: Value,
    value_len: Value,
) -> Result<ast::PathKind, Error> {
    Ok(ast::PathKind::Slice(
        Box::new(path(arena, value_path)?),
        Box::new(exp(arena, value_idx)?),
        Box::new(exp(arena, value_len)?),
    ))
}

fn decode_dot_path(
    arena: &Arena,
    value_path: Value,
    value_atom: Value,
) -> Result<ast::PathKind, Error> {
    Ok(ast::PathKind::Dot(Box::new(path(arena, value_path)?), atom(arena, value_atom)?))
}

/// Decodes a unary operator.
fn unop(arena: &Arena, value_meta: Value) -> Result<(ast::UnOp, ast::OpTyp), Error> {
    let (mixop_meta, values_meta, _) = case(arena, value_meta)?;
    match values_meta.as_slice() {
        [] if m::is(&mixop_meta, m::NOT) => {
            Ok((ast::UnOp::Bool(boolean::UnOp::Not), ast::OpTyp::Bool))
        }
        [] if m::is(&mixop_meta, m::PLUS) => Ok((ast::UnOp::Num(num::UnOp::Plus), ast::OpTyp::Nat)),
        [] if m::is(&mixop_meta, m::MINUS) => {
            Ok((ast::UnOp::Num(num::UnOp::Minus), ast::OpTyp::Nat))
        }
        _ => Err(Error::unexpected(arena, value_meta, "a nullary unop constructor")),
    }
}

/// Decodes a binary operator.
fn binop(arena: &Arena, value_meta: Value) -> Result<(ast::BinOp, ast::OpTyp), Error> {
    let (mixop_meta, values_meta, _) = case(arena, value_meta)?;
    match values_meta.as_slice() {
        [] if m::is(&mixop_meta, m::AND) => {
            Ok((ast::BinOp::Bool(boolean::BinOp::And), ast::OpTyp::Bool))
        }
        [] if m::is(&mixop_meta, m::OR) => {
            Ok((ast::BinOp::Bool(boolean::BinOp::Or), ast::OpTyp::Bool))
        }
        [] if m::is(&mixop_meta, m::IMPL) => {
            Ok((ast::BinOp::Bool(boolean::BinOp::Impl), ast::OpTyp::Bool))
        }
        [] if m::is(&mixop_meta, m::EQUIV) => {
            Ok((ast::BinOp::Bool(boolean::BinOp::Equiv), ast::OpTyp::Bool))
        }
        [] if m::is(&mixop_meta, m::ADD) => Ok((ast::BinOp::Num(num::BinOp::Add), ast::OpTyp::Nat)),
        [] if m::is(&mixop_meta, m::SUB) => Ok((ast::BinOp::Num(num::BinOp::Sub), ast::OpTyp::Nat)),
        [] if m::is(&mixop_meta, m::MUL) => Ok((ast::BinOp::Num(num::BinOp::Mul), ast::OpTyp::Nat)),
        [] if m::is(&mixop_meta, m::DIV) => Ok((ast::BinOp::Num(num::BinOp::Div), ast::OpTyp::Nat)),
        [] if m::is(&mixop_meta, m::MOD) => Ok((ast::BinOp::Num(num::BinOp::Mod), ast::OpTyp::Nat)),
        [] if m::is(&mixop_meta, m::POW) => Ok((ast::BinOp::Num(num::BinOp::Pow), ast::OpTyp::Nat)),
        _ => Err(Error::unexpected(arena, value_meta, "a nullary binop constructor")),
    }
}

/// Decodes a comparison operator.
fn cmpop(arena: &Arena, value_meta: Value) -> Result<(ast::CmpOp, ast::OpTyp), Error> {
    let (mixop_meta, values_meta, _) = case(arena, value_meta)?;
    match values_meta.as_slice() {
        [] if m::is(&mixop_meta, m::EQ) => {
            Ok((ast::CmpOp::Bool(boolean::CmpOp::Eq), ast::OpTyp::Bool))
        }
        [] if m::is(&mixop_meta, m::NE) => {
            Ok((ast::CmpOp::Bool(boolean::CmpOp::Ne), ast::OpTyp::Bool))
        }
        [] if m::is(&mixop_meta, m::LT) => Ok((ast::CmpOp::Num(num::CmpOp::Lt), ast::OpTyp::Nat)),
        [] if m::is(&mixop_meta, m::LE) => Ok((ast::CmpOp::Num(num::CmpOp::Le), ast::OpTyp::Nat)),
        [] if m::is(&mixop_meta, m::GT) => Ok((ast::CmpOp::Num(num::CmpOp::Gt), ast::OpTyp::Nat)),
        [] if m::is(&mixop_meta, m::GE) => Ok((ast::CmpOp::Num(num::CmpOp::Ge), ast::OpTyp::Nat)),
        _ => Err(Error::unexpected(arena, value_meta, "a nullary cmpop constructor")),
    }
}
