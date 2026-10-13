//! Canonical reconstruction of AL definitions and scripts
//!
//! Relation notation places inputs before outputs and rebuilds the input
//! hint accordingly. Printing signatures, origins, and general hints are
//! omitted; runtime argument patterns, premises, and result expressions remain.

use super::super::mixops as m;
use super::exp::args;
use super::{case, deftyp, exp, exps, id, iter, list, opt, typ, typs, var};
use crate::{
    lang::{
        al::ast,
        common::{notation::atom::Atom, source::Span},
        data::{
            arena::Arena,
            notation::tree::{Mixfix, Mixop},
            value::flat::Value,
        },
        hints::input::{self, InputHint},
    },
    metacirc::Error,
    phrase,
};
use std::rc::Rc;

/// Decodes a source AL script in definition order.
pub fn spec(arena: &Arena, value_meta: Value) -> Result<ast::Spec, Error> {
    list(arena, value_meta, def)
}

/// Decodes one definition without inventing omitted general hints.
pub fn def(arena: &Arena, value_meta: Value) -> Result<ast::Def, Error> {
    use ast::{DefKind as D, MetaFuncDef as F, RelDef as R, TypDef as T};
    let (mixop, values, span) = case(arena, value_meta)?;
    let def_kind = match values.as_slice() {
        [value_id] if m::is(&mixop, m::EXTTYP) => {
            D::Typ(T::Extern(ast::ExternTyp { id: id(arena, *value_id)?, hints: vec![] }))
        }
        [value_id, value_ids, value_typ] if m::is(&mixop, m::TYP_DEF) => {
            D::Typ(T::Defined(Box::new(ast::DefinedTyp {
                id: id(arena, *value_id)?,
                tparams: list(arena, *value_ids, id)?,
                def_typ: deftyp(arena, *value_typ)?,
                hints: vec![],
            })))
        }
        [value_id, value_input, value_output] if m::is(&mixop, m::EXTREL) => {
            let (not_typ, input_hint) =
                notation(typs(arena, *value_input)?, typs(arena, *value_output)?, span)?;
            D::Rel(R::Extern(Box::new(ast::ExternRel {
                id: id(arena, *value_id)?,
                not_typ: phrase!(node: not_typ, span: span),
                input_hint,
                hints: vec![],
            })))
        }
        [value_id, value_input, value_output, value_groups, value_else]
            if m::is(&mixop, m::REL_DEF) =>
        {
            let (not_typ, input_hint) =
                notation(typs(arena, *value_input)?, typs(arena, *value_output)?, span)?;
            D::Rel(R::Defined(Box::new(ast::DefinedRel {
                id: id(arena, *value_id)?,
                not_typ: phrase!(node: not_typ, span: span),
                input_hint,
                rule_groups: list(arena, *value_groups, rulegroup)?,
                else_group: opt(arena, *value_else, elsegroup)?,
                hints: vec![],
            })))
        }
        [value_id, value_ids, value_params, value_typ] if m::is(&mixop, m::EXTFUNC) => {
            D::MetaFunc(F::Extern(ast::ExternFunc {
                id: id(arena, *value_id)?,
                tparams: list(arena, *value_ids, id)?,
                params: list(arena, *value_params, param)?,
                typ: typ(arena, *value_typ)?,
                hints: vec![],
            }))
        }
        [value_id, value_ids, value_params, value_typ] if m::is(&mixop, m::BUILTINFUNC) => {
            D::MetaFunc(F::Builtin(ast::BuiltinFunc {
                id: id(arena, *value_id)?,
                tparams: list(arena, *value_ids, id)?,
                params: list(arena, *value_params, param)?,
                typ: typ(arena, *value_typ)?,
                hints: vec![],
            }))
        }
        [value_id, value_params, value_typ, value_rows] if m::is(&mixop, m::TABLEFUNC) => {
            D::MetaFunc(F::Table(ast::TableFunc {
                id: id(arena, *value_id)?,
                params: list(arena, *value_params, param)?,
                typ: typ(arena, *value_typ)?,
                table_rows: list(arena, *value_rows, tablerow)?,
                hints: vec![],
            }))
        }
        [value_id, value_ids, value_params, value_typ, value_clauses, value_else]
            if m::is(&mixop, m::FUNC_DEF) =>
        {
            D::MetaFunc(F::Defined(Box::new(ast::DefinedFunc {
                id: id(arena, *value_id)?,
                tparams: list(arena, *value_ids, id)?,
                params: list(arena, *value_params, param)?,
                typ: typ(arena, *value_typ)?,
                clauses: list(arena, *value_clauses, clause)?,
                else_clause: opt(arena, *value_else, clause)?,
                hints: vec![],
            })))
        }
        _ => {
            return Err(Error::unexpected(
                arena,
                value_meta,
                "an AL definition constructor with its declared arguments",
            ));
        }
    };
    Ok(phrase!(node: def_kind, span: span))
}

/// Rebuilds relation notation with contiguous input positions.
fn notation<T>(
    args_input: Vec<T>,
    args_output: Vec<T>,
    span: Span,
) -> Result<(Mixfix<T>, InputHint), Error> {
    // Input positions are canonical, independently of the original notation
    let input_hint = InputHint::new(
        (0..args_input.len())
            .map(|idx| phrase!(node: idx, span: span))
            .collect(),
    );
    let mixop_input = Mixop::Seq(vec![Mixop::Arg; args_input.len()]);
    let mixop_output = Mixop::Seq(vec![Mixop::Arg; args_output.len()]);
    let atom = phrase!(node: Atom::Operator("->".into()), span: span);
    let mixop = Rc::new(Mixop::Infix(Box::new(mixop_input), atom, Box::new(mixop_output)));
    // Validate the operational hint, including the zero-arity case
    let args = input::combine(&input_hint, args_input, args_output)?;
    Ok((Mixfix::new(mixop, args)?, input_hint))
}

fn param(arena: &Arena, value_meta: Value) -> Result<ast::Param, Error> {
    let (mixop, values, span) = case(arena, value_meta)?;
    let param_kind = match values.as_slice() {
        [value_typ] if m::is(&mixop, m::EXP_PARAM) => ast::ParamKind::Exp(typ(arena, *value_typ)?),
        [value_id, value_ids, value_params, value_typ] if m::is(&mixop, m::FUN_PARAM) => {
            ast::ParamKind::Def(
                id(arena, *value_id)?,
                list(arena, *value_ids, id)?,
                list(arena, *value_params, param)?,
                typ(arena, *value_typ)?,
            )
        }
        _ => return Err(Error::unexpected(arena, value_meta, "an AL parameter")),
    };
    Ok(phrase!(node: param_kind, span: span))
}

fn prem(arena: &Arena, value_meta: Value) -> Result<ast::Prem, Error> {
    use ast::PremKind as P;
    let (mixop, values, span) = case(arena, value_meta)?;
    let prem_kind = match values.as_slice() {
        [value_id, value_input, value_output] if m::is(&mixop, m::REL_PREM) => {
            let (not_exp, input_hint) =
                notation(exps(arena, *value_input)?, exps(arena, *value_output)?, span)?;
            P::Rule(ast::RulePrem { id: id(arena, *value_id)?, not_exp, input_hint })
        }
        [value_exp] if m::is(&mixop, m::IF) => P::If(ast::IfPrem { exp: exp(arena, *value_exp)? }),
        [value_id, value_exps] if m::is(&mixop, m::IFHOLD) => {
            let (not_exp, _) = notation(exps(arena, *value_exps)?, vec![], span)?;
            P::IfHold(ast::IfHoldPrem { id: id(arena, *value_id)?, not_exp })
        }
        [value_id, value_exps] if m::is(&mixop, m::IFNOTHOLD) => {
            let (not_exp, _) = notation(exps(arena, *value_exps)?, vec![], span)?;
            P::IfNotHold(ast::IfNotHoldPrem { id: id(arena, *value_id)?, not_exp })
        }
        [value_l, value_r] if m::is(&mixop, m::LET) => {
            P::Let(ast::LetPrem { exp_l: exp(arena, *value_l)?, exp_r: exp(arena, *value_r)? })
        }
        [value_prem, value_iter] if m::is(&mixop, m::ITER_PREM) => P::Iter(ast::IterPrem {
            prem: Box::new(prem(arena, *value_prem)?),
            prem_iter: iterprem(arena, *value_iter)?,
        }),
        [value_exp] if m::is(&mixop, m::DEBUG) => {
            P::Debug(ast::DebugPrem { exp: exp(arena, *value_exp)? })
        }
        _ => return Err(Error::unexpected(arena, value_meta, "an AL premise")),
    };
    Ok(phrase!(node: prem_kind, span: span))
}

fn iterprem(arena: &Arena, value_meta: Value) -> Result<ast::PremIter, Error> {
    let (mixop, values, _) = case(arena, value_meta)?;
    match values.as_slice() {
        [value_iter, value_bound, value_bind] if m::is(&mixop, m::ITERPREM) => Ok(ast::PremIter {
            iter: iter(arena, *value_iter)?,
            vars_bound: list(arena, *value_bound, var)?,
            vars_bind: list(arena, *value_bind, var)?,
        }),
        _ => Err(Error::unexpected(arena, value_meta, "iter vari* vari*")),
    }
}

fn rulematch(arena: &Arena, value_meta: Value) -> Result<ast::RuleMatch, Error> {
    let (mixop, values, _) = case(arena, value_meta)?;
    match values.as_slice() {
        [value_exps, value_prems] if m::is(&mixop, m::RULMATCH) => {
            let exps_input = exps(arena, *value_exps)?;
            Ok(ast::RuleMatch {
                exps_signature: exps_input.clone(),
                exps_input,
                prems: list(arena, *value_prems, prem)?,
            })
        }
        _ => Err(Error::unexpected(arena, value_meta, "exp* '-' prem*")),
    }
}

fn rulepath(arena: &Arena, value_meta: Value) -> Result<ast::RulePath, Error> {
    let (mixop, values, _) = case(arena, value_meta)?;
    match values.as_slice() {
        [value_id, value_exps, value_prems] if m::is(&mixop, m::RULPATH) => Ok(ast::RulePath {
            id: id(arena, *value_id)?,
            exps_output: exps(arena, *value_exps)?,
            prems: list(arena, *value_prems, prem)?,
        }),
        _ => Err(Error::unexpected(arena, value_meta, "id '=' exp* '-' prem*")),
    }
}

fn rulegroup(arena: &Arena, value_meta: Value) -> Result<ast::RuleGroup, Error> {
    let (mixop, values, span) = case(arena, value_meta)?;
    match values.as_slice() {
        [value_id, value_match, value_paths] if m::is(&mixop, m::RULGROUP) => {
            Ok(phrase!(node: ast::RuleGroupKind {
            id: id(arena, *value_id)?, rule_match: rulematch(arena, *value_match)?, rule_paths: list(arena, *value_paths, rulepath)?,
        }, span: span))
        }
        _ => Err(Error::unexpected(arena, value_meta, "id ':' rulmatch '=' rulpath*")),
    }
}

fn elsegroup(arena: &Arena, value_meta: Value) -> Result<ast::ElseGroup, Error> {
    let (mixop, values, span) = case(arena, value_meta)?;
    match values.as_slice() {
        [value_id, value_match, value_path] if m::is(&mixop, m::ELSGROUP) => {
            Ok(phrase!(node: ast::ElseGroupKind {
            id: id(arena, *value_id)?, rule_match: rulematch(arena, *value_match)?, rule_path: rulepath(arena, *value_path)?,
        }, span: span))
        }
        _ => Err(Error::unexpected(arena, value_meta, "id ':' rulmatch '=' rulpath")),
    }
}

fn clause(arena: &Arena, value_meta: Value) -> Result<ast::Clause, Error> {
    let (mixop, values, span) = case(arena, value_meta)?;
    match values.as_slice() {
        [value_args, value_exp, value_prems] if m::is(&mixop, m::CLAUSE) => {
            Ok(phrase!(node: ast::ClauseKind {
            args: args(arena, *value_args)?, exp: exp(arena, *value_exp)?, prems: list(arena, *value_prems, prem)?,
        }, span: span))
        }
        _ => Err(Error::unexpected(arena, value_meta, "arg* '=' exp '-' prem*")),
    }
}

fn tablerow(arena: &Arena, value_meta: Value) -> Result<ast::TableRow, Error> {
    let clause_al = clause(arena, value_meta)?;
    let ast::ClauseKind { args, exp, prems } = clause_al.node;
    Ok(
        phrase!(node: ast::TableRowKind { exps_signature: vec![], args, exp, prems }, span: clause_al.span),
    )
}
