//! AL specifications as the JSON of `spec-meta-k/3-execution/3.04.3-spec-json.k`
//!
//! Each node is tagged with the symbol of its constructor in spec-meta-k's syntax,
//! as `metacross/kast.rs` of #387 made its KAST terms: the type arguments of `VAR`
//! and `CALL` are types, and `var` definitions, signatures, hints, and operand
//! types are omitted.

use super::wire::{Writer, atom, id, iter, mixop_rows, number, rows_tree, typ};
use crate::lang::{
    al::ast as al,
    data::typ::Typ,
    hints::input::{self, InputHint},
    il::ast as il,
};

/// The input and output positions of a relation's notation.
///
/// Elaboration validates every input hint against its relation's arity.
fn split_inputs<'a, T>(hint: &InputHint, items: &'a [T]) -> (Vec<&'a T>, Vec<&'a T>) {
    input::split(hint, items.iter().collect()).expect("input hint must fit relation")
}

// == Specification and definitions

pub(super) fn spec(writer: &mut Writer, spec: &al::Spec) {
    writer.list(spec, def);
}

/// A definition; `var` definitions have none.
fn def(writer: &mut Writer, def: &al::Def) {
    match &def.node {
        al::DefKind::Var(_) => {}
        al::DefKind::Typ(al::TypDef::Extern(typdef)) => {
            writer.tag("extTypD", |writer| id(writer, &typdef.id));
        }
        al::DefKind::Typ(al::TypDef::Defined(typdef)) => writer.tag("typD", |writer| {
            id(writer, &typdef.id);
            writer.list(&typdef.tparams, id);
            def_typ(writer, &typdef.def_typ);
        }),
        al::DefKind::Rel(al::RelDef::Extern(rel)) => writer.tag("extRelD", |writer| {
            id(writer, &rel.id);
            rel_typs(writer, &rel.input_hint, &rel.not_typ);
        }),
        al::DefKind::Rel(al::RelDef::Defined(rel)) => writer.tag("relD", |writer| {
            id(writer, &rel.id);
            rel_typs(writer, &rel.input_hint, &rel.not_typ);
            writer.list(&rel.rule_groups, rule_group);
            match &rel.else_group {
                Some(group) => else_group(writer, group),
                None => writer.null(),
            }
        }),
        al::DefKind::MetaFunc(al::MetaFuncDef::Extern(func)) => writer.tag("extFuncD", |writer| {
            signature(writer, &func.id, &func.tparams, &func.params, &func.typ);
        }),
        al::DefKind::MetaFunc(al::MetaFuncDef::Builtin(func)) => {
            writer.tag("builtinFuncD", |writer| {
                signature(writer, &func.id, &func.tparams, &func.params, &func.typ);
            });
        }
        al::DefKind::MetaFunc(al::MetaFuncDef::Table(func)) => writer.tag("tableFuncD", |writer| {
            id(writer, &func.id);
            writer.list(&func.params, param);
            typ(writer, &func.typ);
            writer.list(&func.table_rows, |writer, row| {
                let row = &row.node;
                clause_parts(writer, &row.args, &row.exp, &row.prems);
            });
        }),
        al::DefKind::MetaFunc(al::MetaFuncDef::Defined(func)) => writer.tag("funcD", |writer| {
            signature(writer, &func.id, &func.tparams, &func.params, &func.typ);
            writer.list(&func.clauses, |writer, clause| clause_kind(writer, &clause.node));
            match &func.else_clause {
                Some(clause) => clause_kind(writer, &clause.node.clause),
                None => writer.null(),
            }
        }),
    }
}

/// The identifier, type parameters, parameters, and result type of a function.
fn signature(
    writer: &mut Writer,
    id_func: &il::Id,
    tparams: &[il::Id],
    params: &[il::Param],
    typ_ret: &Typ,
) {
    id(writer, id_func);
    writer.list(tparams, id);
    writer.list(params, param);
    typ(writer, typ_ret);
}

/// The input and output types of a relation's notation.
fn rel_typs(writer: &mut Writer, hint: &InputHint, not_typ: &il::NotTyp) {
    let (typs_in, typs_out) = split_inputs(hint, not_typ.node.args());
    writer.list(typs_in, typ);
    writer.list(typs_out, typ);
}

// == Types

fn def_typ(writer: &mut Writer, def_typ: &il::DefTyp) {
    match &def_typ.node {
        il::DefTypKind::Plain(typ_plain) => writer.tag("aliasDT", |writer| typ(writer, typ_plain)),
        il::DefTypKind::Struct(fields) => writer.tag("structDT", |writer| {
            writer.list(fields, |writer, field| {
                writer.array(|writer| {
                    atom(writer, &field.atom);
                    typ(writer, &field.typ);
                });
            });
        }),
        il::DefTypKind::Variant(cases) => writer.tag("variantDT", |writer| {
            writer.list(cases, |writer, case| {
                let not_typ = &case.not_typ.node;
                writer.array(|writer| {
                    mixop_rows(writer, rows_tree(not_typ.mixop()));
                    writer.list(not_typ.args(), typ);
                });
            });
        }),
    }
}

fn var(writer: &mut Writer, var: &il::Var) {
    writer.array(|writer| {
        id(writer, &var.id);
        typ(writer, &var.typ);
        writer.list(&var.iters, iter);
    });
}

// == Operators and patterns

fn un_op(op: &il::UnOp) -> &'static str {
    use crate::lang::common::prim::{bool as boolean, num};
    match op {
        il::UnOp::Bool(boolean::UnOp::Not) => "notOp",
        il::UnOp::Num(num::UnOp::Plus) => "plusOp",
        il::UnOp::Num(num::UnOp::Minus) => "minusOp",
    }
}

fn bin_op(op: &il::BinOp) -> &'static str {
    use crate::lang::common::prim::{bool as boolean, num};
    match op {
        il::BinOp::Bool(boolean::BinOp::And) => "andOp",
        il::BinOp::Bool(boolean::BinOp::Or) => "orOp",
        il::BinOp::Bool(boolean::BinOp::Impl) => "implOp",
        il::BinOp::Bool(boolean::BinOp::Equiv) => "equivOp",
        il::BinOp::Num(num::BinOp::Add) => "addOp",
        il::BinOp::Num(num::BinOp::Sub) => "subOp",
        il::BinOp::Num(num::BinOp::Mul) => "mulOp",
        il::BinOp::Num(num::BinOp::Div) => "divOp",
        il::BinOp::Num(num::BinOp::Mod) => "modOp",
        il::BinOp::Num(num::BinOp::Pow) => "powOp",
    }
}

fn cmp_op(op: &il::CmpOp) -> &'static str {
    use crate::lang::common::prim::{bool as boolean, num};
    match op {
        il::CmpOp::Bool(boolean::CmpOp::Eq) => "eqOp",
        il::CmpOp::Bool(boolean::CmpOp::Ne) => "neOp",
        il::CmpOp::Num(num::CmpOp::Lt) => "ltOp",
        il::CmpOp::Num(num::CmpOp::Le) => "leOp",
        il::CmpOp::Num(num::CmpOp::Gt) => "gtOp",
        il::CmpOp::Num(num::CmpOp::Ge) => "geOp",
    }
}

fn pattern(writer: &mut Writer, pattern: &il::Pattern) {
    match pattern {
        il::Pattern::Case(mixop) => {
            writer.tag("injPat", |writer| mixop_rows(writer, rows_tree(mixop)));
        }
        il::Pattern::List(il::ListPattern::Cons) => writer.tag("consPat", |_| {}),
        il::Pattern::List(il::ListPattern::Fixed(len)) => {
            writer.tag("fixedPat", |writer| writer.number(len));
        }
        il::Pattern::List(il::ListPattern::Nil) => writer.tag("nilPat", |_| {}),
        il::Pattern::Opt(il::OptPattern::Some) => writer.tag("somePat", |_| {}),
        il::Pattern::Opt(il::OptPattern::None) => writer.tag("nonePat", |_| {}),
    }
}

// == Expressions

fn exp(writer: &mut Writer, exp: &il::Exp) {
    match &exp.node {
        il::ExpKind::Bool(value) => writer.tag("boolE", |writer| writer.bool(*value)),
        il::ExpKind::Num(num) => number(writer, num),
        il::ExpKind::Text(text) => writer.tag("textE", |writer| writer.text(text)),
        il::ExpKind::Id(id_var) => writer.tag("varE", |writer| id(writer, id_var)),
        il::ExpKind::Un(op, _, exp_inner) => writer.tag("unE", |writer| {
            writer.text(un_op(op));
            self::exp(writer, exp_inner);
        }),
        il::ExpKind::Bin(op, _, exp_l, exp_r) => writer.tag("binE", |writer| {
            writer.text(bin_op(op));
            self::exp(writer, exp_l);
            self::exp(writer, exp_r);
        }),
        il::ExpKind::Cmp(op, _, exp_l, exp_r) => writer.tag("cmpE", |writer| {
            writer.text(cmp_op(op));
            self::exp(writer, exp_l);
            self::exp(writer, exp_r);
        }),
        il::ExpKind::UpCast(typ_cast, exp_inner) => writer.tag("upCastE", |writer| {
            typ(writer, typ_cast);
            self::exp(writer, exp_inner);
        }),
        il::ExpKind::DownCast(typ_cast, exp_inner) => writer.tag("downCastE", |writer| {
            typ(writer, typ_cast);
            self::exp(writer, exp_inner);
        }),
        il::ExpKind::Sub(exp_inner, typ_sub) => writer.tag("subE", |writer| {
            self::exp(writer, exp_inner);
            typ(writer, typ_sub);
        }),
        il::ExpKind::Match(exp_inner, pattern_match) => writer.tag("matchE", |writer| {
            self::exp(writer, exp_inner);
            pattern(writer, pattern_match);
        }),
        il::ExpKind::Tuple(exps) => writer.tag("tupE", |writer| writer.list(exps, self::exp)),
        il::ExpKind::Case(not_exp) => writer.tag("injE", |writer| {
            mixop_rows(writer, rows_tree(not_exp.mixop()));
            writer.list(not_exp.args(), self::exp);
        }),
        il::ExpKind::Str(fields) => writer.tag("strE", |writer| {
            writer.list(fields, |writer, field| {
                writer.array(|writer| {
                    atom(writer, &field.atom);
                    self::exp(writer, &field.exp);
                });
            });
        }),
        il::ExpKind::Opt(exp_inner) => writer.tag("optE", |writer| match exp_inner {
            Some(exp_inner) => self::exp(writer, exp_inner),
            None => writer.null(),
        }),
        il::ExpKind::List(exps) => writer.tag("listE", |writer| writer.list(exps, self::exp)),
        il::ExpKind::Cons(exp_h, exp_t) => writer.tag("consE", |writer| {
            self::exp(writer, exp_h);
            self::exp(writer, exp_t);
        }),
        il::ExpKind::Cat(exp_l, exp_r) => writer.tag("catE", |writer| {
            self::exp(writer, exp_l);
            self::exp(writer, exp_r);
        }),
        il::ExpKind::Mem(exp_e, exp_s) => writer.tag("memE", |writer| {
            self::exp(writer, exp_e);
            self::exp(writer, exp_s);
        }),
        il::ExpKind::Len(exp_inner) => writer.tag("lenE", |writer| self::exp(writer, exp_inner)),
        il::ExpKind::Dot(exp_inner, atom_dot) => writer.tag("dotE", |writer| {
            self::exp(writer, exp_inner);
            atom(writer, atom_dot);
        }),
        il::ExpKind::Idx(exp_b, exp_i) => writer.tag("idxE", |writer| {
            self::exp(writer, exp_b);
            self::exp(writer, exp_i);
        }),
        il::ExpKind::Slice(exp_b, exp_i, exp_n) => writer.tag("sliceE", |writer| {
            self::exp(writer, exp_b);
            self::exp(writer, exp_i);
            self::exp(writer, exp_n);
        }),
        il::ExpKind::Upd(exp_b, path_upd, exp_n) => writer.tag("updE", |writer| {
            self::exp(writer, exp_b);
            path(writer, path_upd);
            self::exp(writer, exp_n);
        }),
        il::ExpKind::Call(id_func, targs, args) => writer.tag("callE", |writer| {
            id(writer, id_func);
            writer.list(targs, typ);
            writer.list(args, arg);
        }),
        il::ExpKind::Iter(exp_inner, exp_iter) => writer.tag("iterE", |writer| {
            self::exp(writer, exp_inner);
            iter(writer, &exp_iter.iter);
            writer.list(&exp_iter.vars, var);
        }),
    }
}

fn path(writer: &mut Writer, path: &il::Path) {
    match &path.node {
        il::PathKind::Root => writer.tag("rootPath", |_| {}),
        il::PathKind::Idx(path_inner, exp_i) => writer.tag("idxPath", |writer| {
            self::path(writer, path_inner);
            exp(writer, exp_i);
        }),
        il::PathKind::Slice(path_inner, exp_i, exp_n) => writer.tag("slicePath", |writer| {
            self::path(writer, path_inner);
            exp(writer, exp_i);
            exp(writer, exp_n);
        }),
        il::PathKind::Dot(path_inner, atom_dot) => writer.tag("dotPath", |writer| {
            self::path(writer, path_inner);
            atom(writer, atom_dot);
        }),
    }
}

// == Arguments and parameters

fn arg(writer: &mut Writer, arg: &il::Arg) {
    match &arg.node {
        il::ArgKind::Exp(exp_arg) => writer.tag("expA", |writer| exp(writer, exp_arg)),
        il::ArgKind::Def(id_func) => writer.tag("funA", |writer| id(writer, id_func)),
    }
}

fn param(writer: &mut Writer, param: &il::Param) {
    match &param.node {
        il::ParamKind::Exp(typ_param) => writer.tag("expParam", |writer| typ(writer, typ_param)),
        il::ParamKind::Def(id_func, tparams, params, typ_ret) => {
            writer.tag("funParam", |writer| signature(writer, id_func, tparams, params, typ_ret));
        }
    }
}

// == Premises

fn prem(writer: &mut Writer, prem: &al::Prem) {
    match &prem.node {
        al::PremKind::Rule(prem) => writer.tag("relPr", |writer| {
            let (exps_in, exps_out) = split_inputs(&prem.input_hint, prem.not_exp.args());
            id(writer, &prem.id);
            writer.list(exps_in, exp);
            writer.list(exps_out, exp);
        }),
        al::PremKind::If(prem) => writer.tag("ifPr", |writer| exp(writer, &prem.exp)),
        al::PremKind::IfHold(prem) => writer.tag("ifHoldPr", |writer| {
            id(writer, &prem.id);
            writer.list(prem.not_exp.args(), exp);
        }),
        al::PremKind::IfNotHold(prem) => writer.tag("ifNotHoldPr", |writer| {
            id(writer, &prem.id);
            writer.list(prem.not_exp.args(), exp);
        }),
        al::PremKind::Let(prem) => writer.tag("letPr", |writer| {
            exp(writer, &prem.exp_l);
            exp(writer, &prem.exp_r);
        }),
        al::PremKind::Iter(prem) => writer.tag("iterPr", |writer| {
            let prem_iter = &prem.prem_iter;
            self::prem(writer, &prem.prem);
            iter(writer, &prem_iter.iter);
            writer.list(&prem_iter.vars_bound, var);
            writer.list(&prem_iter.vars_bind, var);
        }),
        al::PremKind::Debug(prem) => writer.tag("debugPr", |writer| exp(writer, &prem.exp)),
    }
}

// == Rules, clauses, and table rows

fn rule_match(writer: &mut Writer, rule_match: &al::RuleMatch) {
    writer.array(|writer| {
        writer.list(&rule_match.exps_input, exp);
        writer.list(&rule_match.prems, prem);
    });
}

/// Outputs precede premises, unlike the field order.
fn rule_path(writer: &mut Writer, rule_path: &al::RulePath) {
    writer.array(|writer| {
        id(writer, &rule_path.id);
        writer.list(&rule_path.exps_output, exp);
        writer.list(&rule_path.prems, prem);
    });
}

fn rule_group(writer: &mut Writer, group: &al::RuleGroup) {
    let group = &group.node;
    writer.array(|writer| {
        id(writer, &group.id);
        rule_match(writer, &group.rule_match);
        writer.list(&group.rule_paths, rule_path);
    });
}

fn else_group(writer: &mut Writer, group: &al::ElseGroup) {
    let group = &group.node;
    writer.array(|writer| {
        id(writer, &group.id);
        rule_match(writer, &group.rule_match);
        rule_path(writer, &group.rule_path);
    });
}

fn clause_kind(writer: &mut Writer, clause: &al::ClauseKind) {
    clause_parts(writer, &clause.args, &clause.exp, &clause.prems);
}

/// A clause or a table row: its arguments, result, and premises.
fn clause_parts(writer: &mut Writer, args: &[il::Arg], exp_out: &il::Exp, prems: &[al::Prem]) {
    writer.array(|writer| {
        writer.list(args, arg);
        exp(writer, exp_out);
        writer.list(prems, prem);
    });
}
