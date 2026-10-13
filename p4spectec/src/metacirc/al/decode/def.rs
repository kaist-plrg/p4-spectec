//! Canonical reconstruction of AL definitions and scripts
//!
//! `def` and `spec` rebuild relation notation with inputs before outputs
//! Printing signatures, origins, and general hints are omitted,
//! while runtime argument patterns, premises, and result expressions remain

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
    let (mixop_meta, values_meta, span) = case(arena, value_meta)?;
    let def_kind_al = match values_meta.as_slice() {
        [value_id] if m::is(&mixop_meta, m::EXTTYP) => decode_extern_typ_def(arena, *value_id)?,
        [value_id, value_ids, value_typ] if m::is(&mixop_meta, m::TYP_DEF) => {
            decode_defined_typ_def(arena, *value_id, *value_ids, *value_typ)?
        }
        [value_id, value_input, value_output] if m::is(&mixop_meta, m::EXTREL) => {
            decode_extern_rel_def(arena, *value_id, *value_input, *value_output, span)?
        }
        [value_id, value_input, value_output, value_groups, value_else]
            if m::is(&mixop_meta, m::REL_DEF) =>
        {
            decode_defined_rel_def(
                arena,
                *value_id,
                *value_input,
                *value_output,
                *value_groups,
                *value_else,
                span,
            )?
        }
        [value_id, value_ids, value_params, value_typ] if m::is(&mixop_meta, m::EXTFUNC) => {
            decode_extern_func_def(arena, *value_id, *value_ids, *value_params, *value_typ)?
        }
        [value_id, value_ids, value_params, value_typ] if m::is(&mixop_meta, m::BUILTINFUNC) => {
            decode_builtin_func_def(arena, *value_id, *value_ids, *value_params, *value_typ)?
        }
        [value_id, value_params, value_typ, value_rows] if m::is(&mixop_meta, m::TABLEFUNC) => {
            decode_table_func_def(arena, *value_id, *value_params, *value_typ, *value_rows)?
        }
        [value_id, value_ids, value_params, value_typ, value_clauses, value_else]
            if m::is(&mixop_meta, m::FUNC_DEF) =>
        {
            decode_defined_func_def(
                arena,
                *value_id,
                *value_ids,
                *value_params,
                *value_typ,
                *value_clauses,
                *value_else,
            )?
        }
        _ => {
            return Err(Error::unexpected(
                arena,
                value_meta,
                "an AL definition constructor with its declared arguments",
            ));
        }
    };
    Ok(phrase!(node: def_kind_al, span: span))
}

/// Rebuilds an external type without general hints.
fn decode_extern_typ_def(arena: &Arena, value_id: Value) -> Result<ast::DefKind, Error> {
    Ok(ast::DefKind::Typ(ast::TypDef::Extern(ast::ExternTyp {
        id: id(arena, value_id)?,
        hints: vec![],
    })))
}

/// Rebuilds a type definition without general hints.
fn decode_defined_typ_def(
    arena: &Arena,
    value_id: Value,
    value_ids: Value,
    value_typ: Value,
) -> Result<ast::DefKind, Error> {
    Ok(ast::DefKind::Typ(ast::TypDef::Defined(Box::new(ast::DefinedTyp {
        id: id(arena, value_id)?,
        tparams: list(arena, value_ids, id)?,
        def_typ: deftyp(arena, value_typ)?,
        hints: vec![],
    }))))
}

/// Rebuilds an external relation with canonical input positions.
fn decode_extern_rel_def(
    arena: &Arena,
    value_id: Value,
    value_input: Value,
    value_output: Value,
    span: Span,
) -> Result<ast::DefKind, Error> {
    // Reconstruct the input hint from contiguous argument positions
    let (mixfix_typ_al, input_hint) =
        notation(typs(arena, value_input)?, typs(arena, value_output)?, span)?;
    // Decode the remaining definition fields without general hints
    Ok(ast::DefKind::Rel(ast::RelDef::Extern(Box::new(ast::ExternRel {
        id: id(arena, value_id)?,
        not_typ: phrase!(node: mixfix_typ_al, span: span),
        input_hint,
        hints: vec![],
    }))))
}

/// Rebuilds a relation and its rule groups with canonical notation.
fn decode_defined_rel_def(
    arena: &Arena,
    value_id: Value,
    value_input: Value,
    value_output: Value,
    value_groups: Value,
    value_else: Value,
    span: Span,
) -> Result<ast::DefKind, Error> {
    // Reconstruct the input hint from contiguous argument positions
    let (mixfix_typ_al, input_hint) =
        notation(typs(arena, value_input)?, typs(arena, value_output)?, span)?;
    // Decode the remaining definition fields without general hints
    Ok(ast::DefKind::Rel(ast::RelDef::Defined(Box::new(ast::DefinedRel {
        id: id(arena, value_id)?,
        not_typ: phrase!(node: mixfix_typ_al, span: span),
        input_hint,
        rule_groups: list(arena, value_groups, rulegroup)?,
        else_group: opt(arena, value_else, elsegroup)?,
        hints: vec![],
    }))))
}

/// Rebuilds an external function without general hints.
fn decode_extern_func_def(
    arena: &Arena,
    value_id: Value,
    value_ids: Value,
    value_params: Value,
    value_typ: Value,
) -> Result<ast::DefKind, Error> {
    Ok(ast::DefKind::MetaFunc(ast::MetaFuncDef::Extern(ast::ExternFunc {
        id: id(arena, value_id)?,
        tparams: list(arena, value_ids, id)?,
        params: list(arena, value_params, param)?,
        typ: typ(arena, value_typ)?,
        hints: vec![],
    })))
}

/// Rebuilds a builtin function without general hints.
fn decode_builtin_func_def(
    arena: &Arena,
    value_id: Value,
    value_ids: Value,
    value_params: Value,
    value_typ: Value,
) -> Result<ast::DefKind, Error> {
    Ok(ast::DefKind::MetaFunc(ast::MetaFuncDef::Builtin(ast::BuiltinFunc {
        id: id(arena, value_id)?,
        tparams: list(arena, value_ids, id)?,
        params: list(arena, value_params, param)?,
        typ: typ(arena, value_typ)?,
        hints: vec![],
    })))
}

/// Rebuilds a table function without printing signatures or hints.
fn decode_table_func_def(
    arena: &Arena,
    value_id: Value,
    value_params: Value,
    value_typ: Value,
    value_rows: Value,
) -> Result<ast::DefKind, Error> {
    Ok(ast::DefKind::MetaFunc(ast::MetaFuncDef::Table(ast::TableFunc {
        id: id(arena, value_id)?,
        params: list(arena, value_params, param)?,
        typ: typ(arena, value_typ)?,
        table_rows: list(arena, value_rows, tablerow)?,
        hints: vec![],
    })))
}

/// Rebuilds function clauses and the otherwise clause without hints.
fn decode_defined_func_def(
    arena: &Arena,
    value_id: Value,
    value_ids: Value,
    value_params: Value,
    value_typ: Value,
    value_clauses: Value,
    value_else: Value,
) -> Result<ast::DefKind, Error> {
    Ok(ast::DefKind::MetaFunc(ast::MetaFuncDef::Defined(Box::new(ast::DefinedFunc {
        id: id(arena, value_id)?,
        tparams: list(arena, value_ids, id)?,
        params: list(arena, value_params, param)?,
        typ: typ(arena, value_typ)?,
        clauses: list(arena, value_clauses, clause)?,
        else_clause: opt(arena, value_else, clause)?,
        hints: vec![],
    }))))
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
    // Separate canonical input and output positions with the relation arrow
    let mixop_input = Mixop::Seq(vec![Mixop::Arg; args_input.len()]);
    let mixop_output = Mixop::Seq(vec![Mixop::Arg; args_output.len()]);
    let atom_arrow = phrase!(node: Atom::Operator("->".into()), span: span);
    let mixop_notation =
        Rc::new(Mixop::Infix(Box::new(mixop_input), atom_arrow, Box::new(mixop_output)));
    // Validate the operational hint, including the zero-arity case
    let args_notation = input::combine(&input_hint, args_input, args_output)?;
    Ok((Mixfix::new(mixop_notation, args_notation)?, input_hint))
}

/// Decodes a value or higher-order function parameter.
fn param(arena: &Arena, value_meta: Value) -> Result<ast::Param, Error> {
    let (mixop_meta, values_meta, span) = case(arena, value_meta)?;
    let param_kind_al = match values_meta.as_slice() {
        [value_typ] if m::is(&mixop_meta, m::EXP_PARAM) => decode_exp_param(arena, *value_typ)?,
        [value_id, value_ids, value_params, value_typ] if m::is(&mixop_meta, m::FUN_PARAM) => {
            decode_func_param(arena, *value_id, *value_ids, *value_params, *value_typ)?
        }
        _ => return Err(Error::unexpected(arena, value_meta, "an AL parameter")),
    };
    Ok(phrase!(node: param_kind_al, span: span))
}

fn decode_exp_param(arena: &Arena, value_typ: Value) -> Result<ast::ParamKind, Error> {
    Ok(ast::ParamKind::Exp(typ(arena, value_typ)?))
}

/// Reconstructs a higher-order parameter and its function signature.
fn decode_func_param(
    arena: &Arena,
    value_id: Value,
    value_ids: Value,
    value_params: Value,
    value_typ: Value,
) -> Result<ast::ParamKind, Error> {
    Ok(ast::ParamKind::Def(
        id(arena, value_id)?,
        list(arena, value_ids, id)?,
        list(arena, value_params, param)?,
        typ(arena, value_typ)?,
    ))
}

/// Decodes an AL premise.
fn prem(arena: &Arena, value_meta: Value) -> Result<ast::Prem, Error> {
    let (mixop_meta, values_meta, span) = case(arena, value_meta)?;
    let prem_kind_al = match values_meta.as_slice() {
        [value_id, value_input, value_output] if m::is(&mixop_meta, m::REL_PREM) => {
            decode_rule_prem(arena, *value_id, *value_input, *value_output, span)?
        }
        [value_exp] if m::is(&mixop_meta, m::IF) => decode_if_prem(arena, *value_exp)?,
        [value_id, value_exps] if m::is(&mixop_meta, m::IFHOLD) => {
            decode_if_hold_prem(arena, *value_id, *value_exps, span)?
        }
        [value_id, value_exps] if m::is(&mixop_meta, m::IFNOTHOLD) => {
            decode_if_not_hold_prem(arena, *value_id, *value_exps, span)?
        }
        [value_l, value_r] if m::is(&mixop_meta, m::LET) => {
            decode_let_prem(arena, *value_l, *value_r)?
        }
        [value_prem, value_iter] if m::is(&mixop_meta, m::ITER_PREM) => {
            decode_iter_prem(arena, *value_prem, *value_iter)?
        }
        [value_exp] if m::is(&mixop_meta, m::DEBUG) => decode_debug_prem(arena, *value_exp)?,
        _ => return Err(Error::unexpected(arena, value_meta, "an AL premise")),
    };
    Ok(phrase!(node: prem_kind_al, span: span))
}

/// Rebuilds relation notation with canonical input positions.
fn decode_rule_prem(
    arena: &Arena,
    value_id: Value,
    value_input: Value,
    value_output: Value,
    span: Span,
) -> Result<ast::PremKind, Error> {
    let (mixfix_exp_al, input_hint) =
        notation(exps(arena, value_input)?, exps(arena, value_output)?, span)?;
    Ok(ast::PremKind::Rule(ast::RulePrem {
        id: id(arena, value_id)?,
        not_exp: mixfix_exp_al,
        input_hint,
    }))
}

fn decode_if_prem(arena: &Arena, value_exp: Value) -> Result<ast::PremKind, Error> {
    Ok(ast::PremKind::If(ast::IfPrem { exp: exp(arena, value_exp)? }))
}

/// Rebuilds the relation test with all arguments as inputs.
fn decode_if_hold_prem(
    arena: &Arena,
    value_id: Value,
    value_exps: Value,
    span: Span,
) -> Result<ast::PremKind, Error> {
    let (mixfix_exp_al, _) = notation(exps(arena, value_exps)?, vec![], span)?;
    Ok(ast::PremKind::IfHold(ast::IfHoldPrem { id: id(arena, value_id)?, not_exp: mixfix_exp_al }))
}

/// Rebuilds the negative relation test with all arguments as inputs.
fn decode_if_not_hold_prem(
    arena: &Arena,
    value_id: Value,
    value_exps: Value,
    span: Span,
) -> Result<ast::PremKind, Error> {
    let (mixfix_exp_al, _) = notation(exps(arena, value_exps)?, vec![], span)?;
    Ok(ast::PremKind::IfNotHold(ast::IfNotHoldPrem {
        id: id(arena, value_id)?,
        not_exp: mixfix_exp_al,
    }))
}

fn decode_let_prem(arena: &Arena, value_l: Value, value_r: Value) -> Result<ast::PremKind, Error> {
    Ok(ast::PremKind::Let(ast::LetPrem {
        exp_l: exp(arena, value_l)?,
        exp_r: exp(arena, value_r)?,
    }))
}

fn decode_iter_prem(
    arena: &Arena,
    value_prem: Value,
    value_iter: Value,
) -> Result<ast::PremKind, Error> {
    Ok(ast::PremKind::Iter(ast::IterPrem {
        prem: Box::new(prem(arena, value_prem)?),
        prem_iter: iterprem(arena, value_iter)?,
    }))
}

fn decode_debug_prem(arena: &Arena, value_exp: Value) -> Result<ast::PremKind, Error> {
    Ok(ast::PremKind::Debug(ast::DebugPrem { exp: exp(arena, value_exp)? }))
}

/// Decodes bound and bound-by-iteration variable lists in order.
fn iterprem(arena: &Arena, value_meta: Value) -> Result<ast::PremIter, Error> {
    let (mixop_meta, values_meta, _) = case(arena, value_meta)?;
    match values_meta.as_slice() {
        [value_iter, value_bound, value_bind] if m::is(&mixop_meta, m::ITERPREM) => {
            decode_iterprem(arena, *value_iter, *value_bound, *value_bind)
        }
        _ => Err(Error::unexpected(arena, value_meta, "iter vari* vari*")),
    }
}

/// Reconstructs a premise iterator with both variable lists.
fn decode_iterprem(
    arena: &Arena,
    value_iter: Value,
    value_bound: Value,
    value_bind: Value,
) -> Result<ast::PremIter, Error> {
    Ok(ast::PremIter {
        iter: iter(arena, value_iter)?,
        vars_bound: list(arena, value_bound, var)?,
        vars_bind: list(arena, value_bind, var)?,
    })
}

/// Decodes a rule match without its printing signature.
fn rulematch(arena: &Arena, value_meta: Value) -> Result<ast::RuleMatch, Error> {
    let (mixop_meta, values_meta, _) = case(arena, value_meta)?;
    match values_meta.as_slice() {
        [value_exps, value_prems] if m::is(&mixop_meta, m::RULMATCH) => {
            decode_rulematch(arena, *value_exps, *value_prems)
        }
        _ => Err(Error::unexpected(arena, value_meta, "exp* '-' prem*")),
    }
}

/// Uses input patterns as the omitted rule printing signature.
fn decode_rulematch(
    arena: &Arena,
    value_exps: Value,
    value_prems: Value,
) -> Result<ast::RuleMatch, Error> {
    let exps_input_al = exps(arena, value_exps)?;
    Ok(ast::RuleMatch {
        exps_signature: exps_input_al.clone(),
        exps_input: exps_input_al,
        prems: list(arena, value_prems, prem)?,
    })
}

/// Decodes a rule path and its outputs.
fn rulepath(arena: &Arena, value_meta: Value) -> Result<ast::RulePath, Error> {
    let (mixop_meta, values_meta, _) = case(arena, value_meta)?;
    match values_meta.as_slice() {
        [value_id, value_exps, value_prems] if m::is(&mixop_meta, m::RULPATH) => {
            decode_rulepath(arena, *value_id, *value_exps, *value_prems)
        }
        _ => Err(Error::unexpected(arena, value_meta, "id '=' exp* '-' prem*")),
    }
}

/// Reconstructs a rule path with its outputs and premises.
fn decode_rulepath(
    arena: &Arena,
    value_id: Value,
    value_exps: Value,
    value_prems: Value,
) -> Result<ast::RulePath, Error> {
    Ok(ast::RulePath {
        id: id(arena, value_id)?,
        exps_output: exps(arena, value_exps)?,
        prems: list(arena, value_prems, prem)?,
    })
}

/// Decodes a rule group with its shared match.
fn rulegroup(arena: &Arena, value_meta: Value) -> Result<ast::RuleGroup, Error> {
    let (mixop_meta, values_meta, span) = case(arena, value_meta)?;
    match values_meta.as_slice() {
        [value_id, value_match, value_paths] if m::is(&mixop_meta, m::RULGROUP) => {
            decode_rulegroup(arena, *value_id, *value_match, *value_paths, span)
        }
        _ => Err(Error::unexpected(arena, value_meta, "id ':' rulmatch '=' rulpath*")),
    }
}

/// Reconstructs a rule group with its shared match and paths.
fn decode_rulegroup(
    arena: &Arena,
    value_id: Value,
    value_match: Value,
    value_paths: Value,
    span: Span,
) -> Result<ast::RuleGroup, Error> {
    Ok(phrase!(node: ast::RuleGroupKind {
        id: id(arena, value_id)?,
        rule_match: rulematch(arena, value_match)?,
        rule_paths: list(arena, value_paths, rulepath)?,
    }, span: span))
}

/// Decodes an otherwise group with one rule path.
fn elsegroup(arena: &Arena, value_meta: Value) -> Result<ast::ElseGroup, Error> {
    let (mixop_meta, values_meta, span) = case(arena, value_meta)?;
    match values_meta.as_slice() {
        [value_id, value_match, value_path] if m::is(&mixop_meta, m::ELSGROUP) => {
            decode_elsegroup(arena, *value_id, *value_match, *value_path, span)
        }
        _ => Err(Error::unexpected(arena, value_meta, "id ':' rulmatch '=' rulpath")),
    }
}

/// Reconstructs an otherwise group with its single path.
fn decode_elsegroup(
    arena: &Arena,
    value_id: Value,
    value_match: Value,
    value_path: Value,
    span: Span,
) -> Result<ast::ElseGroup, Error> {
    Ok(phrase!(node: ast::ElseGroupKind {
        id: id(arena, value_id)?,
        rule_match: rulematch(arena, value_match)?,
        rule_path: rulepath(arena, value_path)?,
    }, span: span))
}

/// Decodes a function clause.
fn clause(arena: &Arena, value_meta: Value) -> Result<ast::Clause, Error> {
    let (mixop_meta, values_meta, span) = case(arena, value_meta)?;
    match values_meta.as_slice() {
        [value_args, value_exp, value_prems] if m::is(&mixop_meta, m::CLAUSE) => {
            decode_clause(arena, *value_args, *value_exp, *value_prems, span)
        }
        _ => Err(Error::unexpected(arena, value_meta, "arg* '=' exp '-' prem*")),
    }
}

/// Reconstructs a function clause and retains its source span.
fn decode_clause(
    arena: &Arena,
    value_args: Value,
    value_exp: Value,
    value_prems: Value,
    span: Span,
) -> Result<ast::Clause, Error> {
    Ok(phrase!(node: ast::ClauseKind {
        args: args(arena, value_args)?,
        exp: exp(arena, value_exp)?,
        prems: list(arena, value_prems, prem)?,
    }, span: span))
}

/// Decodes a table row without its printing signature.
fn tablerow(arena: &Arena, value_meta: Value) -> Result<ast::TableRow, Error> {
    let clause_al = clause(arena, value_meta)?;
    let ast::ClauseKind { args: args_al, exp: exp_body_al, prems: prems_al } = clause_al.node;
    Ok(phrase!(node: ast::TableRowKind { exps_signature: vec![],
            args: args_al,
            exp: exp_body_al,
            prems: prems_al, }, span: clause_al.span))
}
