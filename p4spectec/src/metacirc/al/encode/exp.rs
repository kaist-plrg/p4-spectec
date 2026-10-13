//! Encoding of source expressions, arguments, paths, and patterns
//!
//! `exp` and `exps` omit expression notes and resolved operator types
//! Subtype checks encode only their operand and type
//! Native Subcheck plans are not part of the source syntax

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
    let span = exp_al.span;
    match &exp_al.node {
        ast::ExpKind::Bool(value_bool) => encode_bool_exp(arena, *value_bool, span),
        ast::ExpKind::Num(num) => encode_num_exp(arena, num, span),
        ast::ExpKind::Text(text) => encode_text_exp(arena, text, span),
        ast::ExpKind::Id(id_al) => encode_id_exp(arena, id_al, span),
        ast::ExpKind::Un(op_al, _, exp_inner_al) => {
            encode_un_exp(arena, *op_al, exp_inner_al, span)
        }
        ast::ExpKind::Bin(op_al, _, exp_l_al, exp_r_al) => {
            encode_bin_exp(arena, *op_al, exp_l_al, exp_r_al, span)
        }
        ast::ExpKind::Cmp(op_al, _, exp_l_al, exp_r_al) => {
            encode_cmp_exp(arena, *op_al, exp_l_al, exp_r_al, span)
        }
        ast::ExpKind::UpCast(typ_al, exp_inner_al) => {
            encode_upcast_exp(arena, typ_al, exp_inner_al, span)
        }
        ast::ExpKind::DownCast(typ_al, exp_inner_al) => {
            encode_downcast_exp(arena, typ_al, exp_inner_al, span)
        }
        ast::ExpKind::Sub(exp_inner_al, typ_al, _) => {
            encode_sub_exp(arena, exp_inner_al, typ_al, span)
        }
        ast::ExpKind::Match(exp_inner_al, pattern_al) => {
            encode_match_exp(arena, exp_inner_al, pattern_al, span)
        }
        ast::ExpKind::Tuple(exps_al) => encode_tuple_exp(arena, exps_al, span),
        ast::ExpKind::Case(exp_case_al) => encode_case_exp(arena, exp_case_al, span),
        ast::ExpKind::Str(exp_fields_al) => encode_str_exp(arena, exp_fields_al, span),
        ast::ExpKind::Opt(exp_inner_al) => encode_opt_exp(arena, exp_inner_al.as_deref(), span),
        ast::ExpKind::List(exps_al) => encode_list_exp(arena, exps_al, span),
        ast::ExpKind::Cons(exp_head_al, exp_tail_al) => {
            encode_cons_exp(arena, exp_head_al, exp_tail_al, span)
        }
        ast::ExpKind::Cat(exp_l_al, exp_r_al) => encode_cat_exp(arena, exp_l_al, exp_r_al, span),
        ast::ExpKind::Mem(exp_elem_al, exp_list_al) => {
            encode_mem_exp(arena, exp_elem_al, exp_list_al, span)
        }
        ast::ExpKind::Len(exp_inner_al) => encode_len_exp(arena, exp_inner_al, span),
        ast::ExpKind::Dot(exp_base_al, atom_al) => {
            encode_dot_exp(arena, exp_base_al, atom_al, span)
        }
        ast::ExpKind::Idx(exp_base_al, exp_idx_al) => {
            encode_idx_exp(arena, exp_base_al, exp_idx_al, span)
        }
        ast::ExpKind::Slice(exp_base_al, exp_idx_al, exp_len_al) => {
            encode_slice_exp(arena, exp_base_al, exp_idx_al, exp_len_al, span)
        }
        ast::ExpKind::Upd(exp_base_al, path_al, exp_upd_al) => {
            encode_upd_exp(arena, exp_base_al, path_al, exp_upd_al, span)
        }
        ast::ExpKind::Call(id_al, targs_al, args_al) => {
            encode_call_exp(arena, id_al, targs_al, args_al, span)
        }
        ast::ExpKind::Iter(exp_inner_al, exp_iter_al) => {
            encode_iter_exp(arena, exp_inner_al, exp_iter_al, span)
        }
    }
}

/// Encodes a boolean literal.
fn encode_bool_exp(arena: &mut Arena, value_bool: bool, span: Span) -> Result<Value, Error> {
    let (shape, values_meta) = (m::BOOL, vec![make::bool(arena, value_bool, span)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes a numeric literal with its natural or integer constructor.
fn encode_num_exp(arena: &mut Arena, num: &num::Number, span: Span) -> Result<Value, Error> {
    let (shape, values_meta) = {
        let shape = match num {
            num::Number::Nat(_) => m::NAT,
            num::Number::Int(_) => m::INT,
        };
        (shape, vec![make::num(arena, num.clone(), span)?])
    };
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes a text literal.
fn encode_text_exp(arena: &mut Arena, text: &str, span: Span) -> Result<Value, Error> {
    let (shape, values_meta) = (m::TEXT, vec![make::text(arena, text.into(), span)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes a variable identifier.
fn encode_id_exp(arena: &mut Arena, id_al: &ast::Id, span: Span) -> Result<Value, Error> {
    let (shape, values_meta) = (m::VAR_EXP, vec![id(arena, id_al)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes a unary operator and its operand.
fn encode_un_exp(
    arena: &mut Arena,
    op_al: ast::UnOp,
    exp_inner_al: &ast::Exp,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) = (m::UN, vec![unop(arena, op_al)?, exp(arena, exp_inner_al)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes a binary operator and its operands.
fn encode_bin_exp(
    arena: &mut Arena,
    op_al: ast::BinOp,
    exp_l_al: &ast::Exp,
    exp_r_al: &ast::Exp,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) =
        (m::BIN, vec![binop(arena, op_al)?, exp(arena, exp_l_al)?, exp(arena, exp_r_al)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes a comparison operator and its operands.
fn encode_cmp_exp(
    arena: &mut Arena,
    op_al: ast::CmpOp,
    exp_l_al: &ast::Exp,
    exp_r_al: &ast::Exp,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) =
        (m::CMP, vec![cmpop(arena, op_al)?, exp(arena, exp_l_al)?, exp(arena, exp_r_al)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes an upward cast and its target type.
fn encode_upcast_exp(
    arena: &mut Arena,
    typ_al: &ast::Typ,
    exp_inner_al: &ast::Exp,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) = (m::UPCAST, vec![typ(arena, typ_al)?, exp(arena, exp_inner_al)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes a downward cast and its target type.
fn encode_downcast_exp(
    arena: &mut Arena,
    typ_al: &ast::Typ,
    exp_inner_al: &ast::Exp,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) = (m::DOWNCAST, vec![typ(arena, typ_al)?, exp(arena, exp_inner_al)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes a subtype test without its native check plan.
fn encode_sub_exp(
    arena: &mut Arena,
    exp_inner_al: &ast::Exp,
    typ_al: &ast::Typ,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) = (m::SUB_EXP, vec![exp(arena, exp_inner_al)?, typ(arena, typ_al)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes a shape test and its pattern.
fn encode_match_exp(
    arena: &mut Arena,
    exp_inner_al: &ast::Exp,
    pattern_al: &ast::Pattern,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) =
        (m::MATCH, vec![exp(arena, exp_inner_al)?, pattern(arena, pattern_al)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes the expressions in a tuple.
fn encode_tuple_exp(arena: &mut Arena, exps_al: &[ast::Exp], span: Span) -> Result<Value, Error> {
    let (shape, values_meta) = (m::TUP, vec![exps(arena, exps_al)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes a case notation together with its expression arguments.
fn encode_case_exp(
    arena: &mut Arena,
    exp_case_al: &ast::NotExp,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) = {
        let values_meta =
            vec![mixop(arena, exp_case_al.mixop())?, exps(arena, exp_case_al.args())?];
        (m::INJ, vec![case(arena, m::EXPCASE, "expcase", values_meta, span)?])
    };
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes the fields of a struct expression.
fn encode_str_exp(
    arena: &mut Arena,
    exp_fields_al: &[ast::ExpField],
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) = (m::STR, vec![list(arena, "expfield", exp_fields_al, expfield)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes an optional expression.
fn encode_opt_exp(
    arena: &mut Arena,
    exp_inner_al: Option<&ast::Exp>,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) = (m::OPT, vec![opt(arena, "exp", exp_inner_al, exp)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes the expressions in a list.
fn encode_list_exp(arena: &mut Arena, exps_al: &[ast::Exp], span: Span) -> Result<Value, Error> {
    let (shape, values_meta) = (m::LIST, vec![exps(arena, exps_al)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes a list head and tail.
fn encode_cons_exp(
    arena: &mut Arena,
    exp_head_al: &ast::Exp,
    exp_tail_al: &ast::Exp,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) = (m::CONS, vec![exp(arena, exp_head_al)?, exp(arena, exp_tail_al)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes the operands of a concatenation.
fn encode_cat_exp(
    arena: &mut Arena,
    exp_l_al: &ast::Exp,
    exp_r_al: &ast::Exp,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) = (m::CAT, vec![exp(arena, exp_l_al)?, exp(arena, exp_r_al)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes a list membership test.
fn encode_mem_exp(
    arena: &mut Arena,
    exp_elem_al: &ast::Exp,
    exp_list_al: &ast::Exp,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) = (m::MEM, vec![exp(arena, exp_elem_al)?, exp(arena, exp_list_al)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes the operand of a length expression.
fn encode_len_exp(arena: &mut Arena, exp_inner_al: &ast::Exp, span: Span) -> Result<Value, Error> {
    let (shape, values_meta) = (m::LEN, vec![exp(arena, exp_inner_al)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes a field access and its base expression.
fn encode_dot_exp(
    arena: &mut Arena,
    exp_base_al: &ast::Exp,
    atom_al: &ast::Atom,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) = (m::DOT, vec![exp(arena, exp_base_al)?, atom(arena, atom_al)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes an index access and its base expression.
fn encode_idx_exp(
    arena: &mut Arena,
    exp_base_al: &ast::Exp,
    exp_idx_al: &ast::Exp,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) = (m::IDX, vec![exp(arena, exp_base_al)?, exp(arena, exp_idx_al)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes a slice with its base, index, and length.
fn encode_slice_exp(
    arena: &mut Arena,
    exp_base_al: &ast::Exp,
    exp_idx_al: &ast::Exp,
    exp_len_al: &ast::Exp,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) = (
        m::SLICE,
        vec![exp(arena, exp_base_al)?, exp(arena, exp_idx_al)?, exp(arena, exp_len_al)?],
    );
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes an update with its base, path, and replacement.
fn encode_upd_exp(
    arena: &mut Arena,
    exp_base_al: &ast::Exp,
    path_al: &ast::Path,
    exp_upd_al: &ast::Exp,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) =
        (m::UPD, vec![exp(arena, exp_base_al)?, path(arena, path_al)?, exp(arena, exp_upd_al)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes a call with its type arguments and runtime arguments.
fn encode_call_exp(
    arena: &mut Arena,
    id_al: &ast::Id,
    targs_al: &[ast::Typ],
    args_al: &[ast::Arg],
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) =
        (m::CALL, vec![id(arena, id_al)?, typs(arena, targs_al)?, args(arena, args_al)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes an iterated expression and its bound variables.
fn encode_iter_exp(
    arena: &mut Arena,
    exp_inner_al: &ast::Exp,
    exp_iter_al: &ast::ExpIter,
    span: Span,
) -> Result<Value, Error> {
    let (shape, values_meta) =
        (m::ITER_EXP, vec![exp(arena, exp_inner_al)?, iterexp(arena, exp_iter_al)?]);
    case(arena, shape, "exp", values_meta, span)
}

/// Encodes a list of source expressions.
pub fn exps(arena: &mut Arena, exps_al: &[ast::Exp]) -> Result<Value, Error> {
    list(arena, "exp", exps_al, exp)
}

/// Encodes one source struct-expression field.
fn expfield(arena: &mut Arena, exp_field_al: &ast::ExpField) -> Result<Value, Error> {
    let values_meta = vec![atom(arena, &exp_field_al.atom)?, exp(arena, &exp_field_al.exp)?];
    case(arena, m::EXPFIELD, "expfield", values_meta, exp_field_al.atom.span)
}

/// Encodes an expression iterator and its variables.
fn iterexp(arena: &mut Arena, exp_iter_al: &ast::ExpIter) -> Result<Value, Error> {
    let values_meta =
        vec![iter(arena, &exp_iter_al.iter)?, list(arena, "vari", &exp_iter_al.vars, var)?];
    case(arena, m::ITEREXP, "iterexp", values_meta, Span::default())
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

/// Encodes a case, list, or option shape pattern.
fn pattern(arena: &mut Arena, pattern_al: &ast::Pattern) -> Result<Value, Error> {
    let (shape, values_meta) = match pattern_al {
        ast::Pattern::Case(mixop_al) => (m::CASE_PATTERN, vec![mixop(arena, mixop_al)?]),
        ast::Pattern::List(ast::ListPattern::Cons) => (m::CONS_PATTERN, vec![]),
        ast::Pattern::List(ast::ListPattern::Fixed(num_len)) => {
            return encode_fixed_pattern(arena, *num_len);
        }
        ast::Pattern::List(ast::ListPattern::Nil) => (m::NIL, vec![]),
        ast::Pattern::Opt(ast::OptPattern::Some) => (m::SOME, vec![]),
        ast::Pattern::Opt(ast::OptPattern::None) => (m::NONE, vec![]),
    };
    case(arena, shape, "pattern", values_meta, Span::default())
}

/// Encodes a source update path.
fn path(arena: &mut Arena, path_al: &ast::Path) -> Result<Value, Error> {
    let span = path_al.span;
    match &path_al.node {
        ast::PathKind::Root => case(arena, m::ROOT, "path", vec![], span),
        ast::PathKind::Idx(path_base_al, exp_idx_al) => {
            encode_idx_path(arena, path_base_al, exp_idx_al, span)
        }
        ast::PathKind::Slice(path_base_al, exp_idx_al, exp_len_al) => {
            encode_slice_path(arena, path_base_al, exp_idx_al, exp_len_al, span)
        }
        ast::PathKind::Dot(path_base_al, atom_al) => {
            encode_dot_path(arena, path_base_al, atom_al, span)
        }
    }
}

/// Encodes an index step in an update path.
fn encode_idx_path(
    arena: &mut Arena,
    path_base_al: &ast::Path,
    exp_idx_al: &ast::Exp,
    span: Span,
) -> Result<Value, Error> {
    let values_meta = vec![path(arena, path_base_al)?, exp(arena, exp_idx_al)?];
    case(arena, m::IDX, "path", values_meta, span)
}

/// Encodes a slice step in an update path.
fn encode_slice_path(
    arena: &mut Arena,
    path_base_al: &ast::Path,
    exp_idx_al: &ast::Exp,
    exp_len_al: &ast::Exp,
    span: Span,
) -> Result<Value, Error> {
    let values_meta =
        vec![path(arena, path_base_al)?, exp(arena, exp_idx_al)?, exp(arena, exp_len_al)?];
    case(arena, m::SLICE, "path", values_meta, span)
}

/// Encodes a field step in an update path.
fn encode_dot_path(
    arena: &mut Arena,
    path_base_al: &ast::Path,
    atom_al: &ast::Atom,
    span: Span,
) -> Result<Value, Error> {
    let values_meta = vec![path(arena, path_base_al)?, atom(arena, atom_al)?];
    case(arena, m::DOT, "path", values_meta, span)
}

/// Encodes a unary operator.
fn unop(arena: &mut Arena, op_al: ast::UnOp) -> Result<Value, Error> {
    let shape = match op_al {
        ast::UnOp::Bool(boolean::UnOp::Not) => m::NOT,
        ast::UnOp::Num(num::UnOp::Plus) => m::PLUS,
        ast::UnOp::Num(num::UnOp::Minus) => m::MINUS,
    };
    case(arena, shape, "unop", vec![], Span::default())
}

/// Encodes a binary operator.
fn binop(arena: &mut Arena, op_al: ast::BinOp) -> Result<Value, Error> {
    let shape = match op_al {
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

/// Encodes a comparison operator.
fn cmpop(arena: &mut Arena, op_al: ast::CmpOp) -> Result<Value, Error> {
    let shape = match op_al {
        ast::CmpOp::Bool(boolean::CmpOp::Eq) => m::EQ,
        ast::CmpOp::Bool(boolean::CmpOp::Ne) => m::NE,
        ast::CmpOp::Num(num::CmpOp::Lt) => m::LT,
        ast::CmpOp::Num(num::CmpOp::Le) => m::LE,
        ast::CmpOp::Num(num::CmpOp::Gt) => m::GT,
        ast::CmpOp::Num(num::CmpOp::Ge) => m::GE,
    };
    case(arena, shape, "cmpop", vec![], Span::default())
}

/// Encodes a fixed-length list pattern.
fn encode_fixed_pattern(arena: &mut Arena, num_len: usize) -> Result<Value, Error> {
    let nat = u64::try_from(num_len).map_err(ValueError::from)?.into();
    let value_nat = make::nat(arena, nat, Span::default())?;
    case(arena, m::FIXED, "pattern", vec![value_nat], Span::default())
}
