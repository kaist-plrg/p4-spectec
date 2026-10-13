//! Encoding of source expressions, arguments, paths, and patterns
//!
//! Expression notes and resolved operator types are omitted. Subtype checks
//! encode only their operand and type; native Subcheck plans are not syntax.

use super::super::mixops as m;
use super::{atom, case, id, iter, list, mixop, opt, typ, typs, var};
use crate::{
    lang::{
        common::{
            prim::{bool as boolean, num},
            source::Span,
        },
        data::{
            arena::Arena,
            value::{
                ValueError,
                flat::{Value, make},
            },
        },
        il::ast,
    },
    metacirc::Error,
};

/// Encodes every source expression form without native optimization data.
pub fn exp(arena: &mut Arena, exp_al: &ast::Exp) -> Result<Value, Error> {
    use ast::ExpKind as E;
    let (shape, values) = match &exp_al.node {
        E::Bool(b) => (m::BOOL, vec![make::bool(arena, *b, exp_al.span)?]),
        E::Num(num) => {
            let shape = match num {
                num::Number::Nat(_) => m::NAT,
                num::Number::Int(_) => m::INT,
            };
            (shape, vec![make::num(arena, num.clone(), exp_al.span)?])
        }
        E::Text(text) => (m::TEXT, vec![make::text(arena, text.clone(), exp_al.span)?]),
        E::Id(id_al) => (m::VAR_EXP, vec![id(arena, id_al)?]),
        E::Un(op, _, exp_inner) => (m::UN, vec![unop(arena, *op)?, exp(arena, exp_inner)?]),
        E::Bin(op, _, exp_l, exp_r) => {
            (m::BIN, vec![binop(arena, *op)?, exp(arena, exp_l)?, exp(arena, exp_r)?])
        }
        E::Cmp(op, _, exp_l, exp_r) => {
            (m::CMP, vec![cmpop(arena, *op)?, exp(arena, exp_l)?, exp(arena, exp_r)?])
        }
        E::UpCast(typ_al, exp_inner) => {
            (m::UPCAST, vec![typ(arena, typ_al)?, exp(arena, exp_inner)?])
        }
        E::DownCast(typ_al, exp_inner) => {
            (m::DOWNCAST, vec![typ(arena, typ_al)?, exp(arena, exp_inner)?])
        }
        E::Sub(exp_inner, typ_al, _) => {
            (m::SUB_EXP, vec![exp(arena, exp_inner)?, typ(arena, typ_al)?])
        }
        E::Match(exp_inner, pattern_al) => {
            (m::MATCH, vec![exp(arena, exp_inner)?, pattern(arena, pattern_al)?])
        }
        E::Tuple(exps_al) => (m::TUP, vec![exps(arena, exps_al)?]),
        E::Case(not_exp) => {
            let values = vec![mixop(arena, not_exp.mixop())?, exps(arena, not_exp.args())?];
            (m::INJ, vec![case(arena, m::EXPCASE, "expcase", values, exp_al.span)?])
        }
        E::Str(exp_fields) => (m::STR, vec![list(arena, "expfield", exp_fields, expfield)?]),
        E::Opt(exp_inner) => (m::OPT, vec![opt(arena, "exp", exp_inner.as_deref(), exp)?]),
        E::List(exps_al) => (m::LIST, vec![exps(arena, exps_al)?]),
        E::Cons(exp_l, exp_r) => (m::CONS, vec![exp(arena, exp_l)?, exp(arena, exp_r)?]),
        E::Cat(exp_l, exp_r) => (m::CAT, vec![exp(arena, exp_l)?, exp(arena, exp_r)?]),
        E::Mem(exp_l, exp_r) => (m::MEM, vec![exp(arena, exp_l)?, exp(arena, exp_r)?]),
        E::Len(exp_inner) => (m::LEN, vec![exp(arena, exp_inner)?]),
        E::Dot(exp_base, atom_al) => (m::DOT, vec![exp(arena, exp_base)?, atom(arena, atom_al)?]),
        E::Idx(exp_base, exp_idx) => (m::IDX, vec![exp(arena, exp_base)?, exp(arena, exp_idx)?]),
        E::Slice(exp_base, exp_idx, exp_len) => {
            (m::SLICE, vec![exp(arena, exp_base)?, exp(arena, exp_idx)?, exp(arena, exp_len)?])
        }
        E::Upd(exp_base, path_al, exp_inner) => {
            (m::UPD, vec![exp(arena, exp_base)?, path(arena, path_al)?, exp(arena, exp_inner)?])
        }
        E::Call(id_al, targs, args_al) => {
            (m::CALL, vec![id(arena, id_al)?, typs(arena, targs)?, args(arena, args_al)?])
        }
        E::Iter(exp_inner, exp_iter) => {
            (m::ITER_EXP, vec![exp(arena, exp_inner)?, iterexp(arena, exp_iter)?])
        }
    };
    case(arena, shape, "exp", values, exp_al.span)
}

/// Encodes a list of source expressions.
pub fn exps(arena: &mut Arena, exps_al: &[ast::Exp]) -> Result<Value, Error> {
    list(arena, "exp", exps_al, exp)
}

fn expfield(arena: &mut Arena, exp_field: &ast::ExpField) -> Result<Value, Error> {
    let values = vec![atom(arena, &exp_field.atom)?, exp(arena, &exp_field.exp)?];
    case(arena, m::EXPFIELD, "expfield", values, exp_field.atom.span)
}

fn iterexp(arena: &mut Arena, exp_iter: &ast::ExpIter) -> Result<Value, Error> {
    let values = vec![iter(arena, &exp_iter.iter)?, list(arena, "vari", &exp_iter.vars, var)?];
    case(arena, m::ITEREXP, "iterexp", values, Span::default())
}

/// Encodes a value or function argument.
pub(super) fn arg(arena: &mut Arena, arg_al: &ast::Arg) -> Result<Value, Error> {
    let (shape, value) = match &arg_al.node {
        ast::ArgKind::Exp(exp_al) => (m::EXP_ARG, exp(arena, exp_al)?),
        ast::ArgKind::Def(id_al) => (m::FUN_ARG, id(arena, id_al)?),
    };
    case(arena, shape, "arg", vec![value], arg_al.span)
}

/// Encodes a source argument list.
pub(super) fn args(arena: &mut Arena, args_al: &[ast::Arg]) -> Result<Value, Error> {
    list(arena, "arg", args_al, arg)
}

fn pattern(arena: &mut Arena, pattern_al: &ast::Pattern) -> Result<Value, Error> {
    let (shape, values) = match pattern_al {
        ast::Pattern::Case(mixop_al) => (m::CASE_PATTERN, vec![mixop(arena, mixop_al)?]),
        ast::Pattern::List(ast::ListPattern::Cons) => (m::CONS_PATTERN, vec![]),
        ast::Pattern::List(ast::ListPattern::Fixed(len)) => {
            let nat = u64::try_from(*len).map_err(ValueError::from)?.into();
            (m::FIXED, vec![make::nat(arena, nat, Span::default())?])
        }
        ast::Pattern::List(ast::ListPattern::Nil) => (m::NIL, vec![]),
        ast::Pattern::Opt(ast::OptPattern::Some) => (m::SOME, vec![]),
        ast::Pattern::Opt(ast::OptPattern::None) => (m::NONE, vec![]),
    };
    case(arena, shape, "pattern", values, Span::default())
}

fn path(arena: &mut Arena, path_al: &ast::Path) -> Result<Value, Error> {
    let (shape, values) = match &path_al.node {
        ast::PathKind::Root => (m::ROOT, vec![]),
        ast::PathKind::Idx(path_base, exp_idx) => {
            (m::IDX, vec![path(arena, path_base)?, exp(arena, exp_idx)?])
        }
        ast::PathKind::Slice(path_base, exp_idx, exp_len) => {
            (m::SLICE, vec![path(arena, path_base)?, exp(arena, exp_idx)?, exp(arena, exp_len)?])
        }
        ast::PathKind::Dot(path_base, atom_al) => {
            (m::DOT, vec![path(arena, path_base)?, atom(arena, atom_al)?])
        }
    };
    case(arena, shape, "path", values, path_al.span)
}

fn unop(arena: &mut Arena, op: ast::UnOp) -> Result<Value, Error> {
    let shape = match op {
        ast::UnOp::Bool(boolean::UnOp::Not) => m::NOT,
        ast::UnOp::Num(num::UnOp::Plus) => m::PLUS,
        ast::UnOp::Num(num::UnOp::Minus) => m::MINUS,
    };
    case(arena, shape, "unop", vec![], Span::default())
}

fn binop(arena: &mut Arena, op: ast::BinOp) -> Result<Value, Error> {
    let shape = match op {
        ast::BinOp::Bool(boolean::BinOp::And) => m::AND,
        ast::BinOp::Bool(boolean::BinOp::Or) => m::OR,
        ast::BinOp::Bool(boolean::BinOp::Impl) => m::IMPL,
        ast::BinOp::Bool(boolean::BinOp::Equiv) => m::EQUIV,
        ast::BinOp::Num(num::BinOp::Add) => m::ADD,
        ast::BinOp::Num(num::BinOp::Sub) => m::SUB,
        ast::BinOp::Num(num::BinOp::Mul) => m::MUL,
        ast::BinOp::Num(num::BinOp::Div) => m::DIV,
        ast::BinOp::Num(num::BinOp::Mod) => m::MOD,
        ast::BinOp::Num(num::BinOp::Pow) => m::POW,
    };
    case(arena, shape, "binop", vec![], Span::default())
}

fn cmpop(arena: &mut Arena, op: ast::CmpOp) -> Result<Value, Error> {
    let shape = match op {
        ast::CmpOp::Bool(boolean::CmpOp::Eq) => m::EQ,
        ast::CmpOp::Bool(boolean::CmpOp::Ne) => m::NE,
        ast::CmpOp::Num(num::CmpOp::Lt) => m::LT,
        ast::CmpOp::Num(num::CmpOp::Le) => m::LE,
        ast::CmpOp::Num(num::CmpOp::Gt) => m::GT,
        ast::CmpOp::Num(num::CmpOp::Ge) => m::GE,
    };
    case(arena, shape, "cmpop", vec![], Span::default())
}
