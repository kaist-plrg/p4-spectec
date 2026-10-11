//! Anti-unification of rule input expressions
//!
//! Overlap corresponding inputs to obtain a shared template,
//! then add equality premises for each rule.
//! For example, `(true, x)` and `(false, x)` become `(b, x)`,
//! with `if b == true` and `if b == false` on the respective paths.
//!
//! A failed structural overlap falls back to one fresh variable
//! if the types are equivalent.
//! Fresh names from the failed attempt are discarded.

use std::rc::Rc;

use crate::lang::{
    common::{ds::set::IdSet, prim, source::Span},
    data::notation::tree as notation,
    traits::{at::At, eq::SyntaxEq},
};

use crate::lang::il::{ast, fresh, var};

use crate::runtime::{
    envs::algo::{MEnv, TDEnv},
    ops::typ::{TypeError, equiv_typ},
};

use crate::{note_phrase, phrase};

use super::super::{AlgoError, error};

use super::context::Context;

/// Separates structural fallback from failures of type operations.
enum OverlapFailure {
    Mismatch,
    Type(TypeError),
}

// == Template overlap

// - Expressions

/// Overlaps one input against the template, structurally or by a fresh name.
fn overlap_exp(
    tdenv: &TDEnv,
    menv: &MEnv,
    ids_free: &mut IdSet,
    ids_unifier: &mut IdSet,
    exp_template: &ast::Exp,
    exp: &ast::Exp,
) -> Result<ast::Exp, OverlapFailure> {
    if exp_template.syntax_eq(exp) {
        return Ok(exp_template.clone());
    }

    // Keep fresh names only when the entire structural overlap succeeds
    let mut ids_free_structural = ids_free.clone();
    let mut ids_unifier_structural = ids_unifier.clone();
    let exp_kind_template = overlap_exp_kind(
        tdenv,
        menv,
        &mut ids_free_structural,
        &mut ids_unifier_structural,
        exp_template,
        exp,
    );
    match exp_kind_template {
        Ok(exp_kind_template) => {
            // Commit only the names belonging to the complete shared structure
            *ids_free = ids_free_structural;
            *ids_unifier = ids_unifier_structural;
            let exp_template = note_phrase! {
                node: exp_kind_template,
                note: exp_template.note.clone(),
                span: exp_template.span.clone(),
            };
            return Ok(exp_template);
        }
        // A shape mismatch may still overlap through a fresh variable
        Err(OverlapFailure::Mismatch) => {}
        // A type operation failure must not trigger structural fallback
        Err(error) => return Err(error),
    }

    // Fall back to a fresh unifier variable when the types agree
    let typ_template =
        phrase!(node: exp_template.note.as_ref().clone(), span: exp_template.span.clone());
    let typ = phrase!(node: exp.note.as_ref().clone(), span: exp.span.clone());
    let is_equivalent = equiv_typ(tdenv, &typ_template, &typ).map_err(OverlapFailure::Type)?;
    if !is_equivalent {
        return Err(OverlapFailure::Mismatch);
    }
    let var_fresh = fresh::var_from_typ(menv, ids_free, exp_template.span.clone(), &typ_template);
    ids_free.insert(var_fresh.id.clone());
    ids_unifier.insert(var_fresh.id.clone());
    let exp_template = var::as_exp(true, &var_fresh);
    Ok(exp_template)
}

/// Overlaps two expressions of the same shape node by node.
fn overlap_exp_kind(
    tdenv: &TDEnv,
    menv: &MEnv,
    ids_free: &mut IdSet,
    ids_unifier: &mut IdSet,
    exp_template: &ast::Exp,
    exp: &ast::Exp,
) -> Result<ast::ExpKind, OverlapFailure> {
    match (&exp_template.node, &exp.node) {
        // An existing unifier variable absorbs any input
        (ast::ExpKind::Id(id_template), _) if ids_unifier.contains(id_template) => {
            Ok(exp_template.node.clone())
        }
        // Upcasts with the same type overlap their operands
        (
            ast::ExpKind::UpCast(typ_template, exp_template_inner),
            ast::ExpKind::UpCast(typ, exp_inner),
        ) if typ_template.syntax_eq(typ) => {
            let exp_template_inner =
                overlap_exp(tdenv, menv, ids_free, ids_unifier, exp_template_inner, exp_inner)?;
            let exp_template_inner = Box::new(exp_template_inner);
            Ok(ast::ExpKind::UpCast(typ_template.clone(), exp_template_inner))
        }
        // Tuples overlap componentwise
        (ast::ExpKind::Tuple(exps_template), ast::ExpKind::Tuple(exps)) => {
            let exps_template = overlap_exps(
                tdenv,
                menv,
                ids_free,
                ids_unifier,
                exps_template.iter(),
                exps.iter(),
            )?;
            Ok(ast::ExpKind::Tuple(exps_template))
        }
        // Cases with the same mixfix overlap their arguments
        (ast::ExpKind::Case(not_exp_template), ast::ExpKind::Case(not_exp))
            if not_exp_template.eq_mixop(not_exp) =>
        {
            overlap_case_exp(tdenv, menv, ids_free, ids_unifier, not_exp_template, not_exp)
        }
        // Structs with the same atoms overlap their fields
        (ast::ExpKind::Str(exp_fields_template), ast::ExpKind::Str(exp_fields))
            if exp_fields_template.len() == exp_fields.len()
                && exp_fields_template.iter().zip(exp_fields).all(
                    |(ast::ExpField { atom: atom_template, .. }, ast::ExpField { atom, .. })| {
                        atom_template.syntax_eq(atom)
                    },
                ) =>
        {
            overlap_str_exp(tdenv, menv, ids_free, ids_unifier, exp_fields_template, exp_fields)
        }
        // Different shapes cannot overlap
        _ => Err(OverlapFailure::Mismatch),
    }
}

/// Overlaps expression lists pairwise, requiring equal length.
fn overlap_exps<'a>(
    tdenv: &TDEnv,
    menv: &MEnv,
    ids_free: &mut IdSet,
    ids_unifier: &mut IdSet,
    exps_template: impl ExactSizeIterator<Item = &'a ast::Exp>,
    exps: impl ExactSizeIterator<Item = &'a ast::Exp>,
) -> Result<Vec<ast::Exp>, OverlapFailure> {
    if exps_template.len() != exps.len() {
        return Err(OverlapFailure::Mismatch);
    }
    let mut exps_overlapped = Vec::with_capacity(exps_template.len());
    for (exp_template, exp) in exps_template.zip(exps) {
        let exp_overlapped = overlap_exp(tdenv, menv, ids_free, ids_unifier, exp_template, exp)?;
        exps_overlapped.push(exp_overlapped);
    }
    Ok(exps_overlapped)
}

// - Case expression

/// Overlaps the arguments of two case expressions with the same mixfix.
fn overlap_case_exp(
    tdenv: &TDEnv,
    menv: &MEnv,
    ids_free: &mut IdSet,
    ids_unifier: &mut IdSet,
    not_exp_template: &ast::NotExp,
    not_exp: &ast::NotExp,
) -> Result<ast::ExpKind, OverlapFailure> {
    let exps_template = overlap_exps(
        tdenv,
        menv,
        ids_free,
        ids_unifier,
        not_exp_template.args().iter(),
        not_exp.args().iter(),
    )?;
    let not_exp_template =
        notation::Mixfix::new(Rc::clone(not_exp_template.mixop()), exps_template)
            .expect("overlapped arguments must preserve the template mixfix arity");
    let not_exp_template = Box::new(not_exp_template);
    Ok(ast::ExpKind::Case(not_exp_template))
}

// - Record expression

/// Overlaps the fields of two struct expressions with the same atoms.
fn overlap_str_exp(
    tdenv: &TDEnv,
    menv: &MEnv,
    ids_free: &mut IdSet,
    ids_unifier: &mut IdSet,
    exp_fields_template: &[ast::ExpField],
    exp_fields: &[ast::ExpField],
) -> Result<ast::ExpKind, OverlapFailure> {
    let exps_template = exp_fields_template
        .iter()
        .map(|ast::ExpField { exp, .. }| exp);
    let exps = exp_fields.iter().map(|ast::ExpField { exp, .. }| exp);
    let exps_template = overlap_exps(tdenv, menv, ids_free, ids_unifier, exps_template, exps)?;
    let exp_fields_template = exp_fields_template
        .iter()
        .map(|ast::ExpField { atom, .. }| atom.clone())
        .zip(exps_template)
        .map(|(atom, exp)| ast::ExpField { atom, exp })
        .collect();
    Ok(ast::ExpKind::Str(exp_fields_template))
}

// - Expressions across rules

/// Folds one input position across all rules into a template.
fn overlap_exp_across_rules<'a>(
    tdenv: &TDEnv,
    menv: &MEnv,
    ids_free: &mut IdSet,
    exp_template: &ast::Exp,
    exps: impl Iterator<Item = &'a ast::Exp>,
) -> Result<(IdSet, ast::Exp), AlgoError> {
    let mut ids_unifier = IdSet::new();
    let mut exp_template = exp_template.clone();
    for exp in exps {
        exp_template = overlap_exp(tdenv, menv, ids_free, &mut ids_unifier, &exp_template, exp)
            .map_err(|failure| match failure {
                OverlapFailure::Mismatch => error::rule::rule_input_mismatch(&exp.span),
                OverlapFailure::Type(error) => error::typ::type_operation_invalid(error),
            })?;
    }
    Ok((ids_unifier, exp_template))
}

/// Builds the template for every input position across rules.
fn overlap_exps_across_rules(
    tdenv: &TDEnv,
    menv: &MEnv,
    ids_free: &mut IdSet,
    exps_by_rule: &[Vec<ast::Exp>],
) -> Result<(IdSet, Vec<ast::Exp>), AlgoError> {
    let Some((exps_head, exps_tail)) = exps_by_rule.split_first() else {
        return Ok((IdSet::new(), vec![]));
    };
    // All rules must supply the same number of inputs
    for exps in exps_tail {
        if exps.len() != exps_head.len() {
            let span = Span::over_iter(exps.iter().chain(exps_head).map(|exp| exp.span.clone()));
            return Err(error::rule::rule_input_mismatch(&span));
        }
    }
    // A single rule is its own template
    if exps_tail.is_empty() {
        return Ok((IdSet::new(), exps_head.clone()));
    }

    let mut ids_unifier = IdSet::new();
    let mut exps_template = Vec::with_capacity(exps_head.len());
    for (idx, exp_head) in exps_head.iter().enumerate() {
        let exps_at_idx = exps_tail.iter().map(|exps| &exps[idx]);
        let (ids_unifier_exp, exp_template) =
            overlap_exp_across_rules(tdenv, menv, ids_free, exp_head, exps_at_idx)?;
        ids_unifier.append(ids_unifier_exp);
        exps_template.push(exp_template);
    }
    Ok((ids_unifier, exps_template))
}

// == Template population

// - Expressions

/// Emits the equality premises a rule needs to match the template.
fn populate_exp(ids_unifier: &IdSet, exp_template: &ast::Exp, exp: &ast::Exp) -> Vec<ast::Prem> {
    if exp_template.syntax_eq(exp) {
        return vec![];
    }
    match (&exp_template.node, &exp.node) {
        // A unifier variable is fixed by equating it with the rule's input
        (ast::ExpKind::Id(id_template), _) if ids_unifier.contains(id_template) => {
            let prem = populate_equality_prem(exp_template, exp);
            vec![prem]
        }
        (
            ast::ExpKind::UpCast(typ_template, exp_template_inner),
            ast::ExpKind::UpCast(typ, exp_inner),
        ) if typ_template.syntax_eq(typ) => {
            populate_exp(ids_unifier, exp_template_inner, exp_inner)
        }
        (ast::ExpKind::Tuple(exps_template), ast::ExpKind::Tuple(exps)) => {
            populate_exps(ids_unifier, exps_template.iter(), exps.iter())
        }
        (ast::ExpKind::Case(not_exp_template), ast::ExpKind::Case(not_exp))
            if not_exp_template.eq_mixop(not_exp) =>
        {
            populate_exps(ids_unifier, not_exp_template.args().iter(), not_exp.args().iter())
        }
        (ast::ExpKind::Str(exp_fields_template), ast::ExpKind::Str(exp_fields)) => {
            let exps_template = exp_fields_template
                .iter()
                .map(|ast::ExpField { exp, .. }| exp);
            let exps = exp_fields.iter().map(|ast::ExpField { exp, .. }| exp);
            populate_exps(ids_unifier, exps_template, exps)
        }
        // Any other mismatch becomes an equality on the whole sub-expression
        _ => {
            let prem = populate_equality_prem(exp_template, exp);
            vec![prem]
        }
    }
}

fn populate_exps<'a>(
    ids_unifier: &IdSet,
    exps_template: impl Iterator<Item = &'a ast::Exp>,
    exps: impl Iterator<Item = &'a ast::Exp>,
) -> Vec<ast::Prem> {
    exps_template
        .zip(exps)
        .flat_map(|(exp_template, exp)| populate_exp(ids_unifier, exp_template, exp))
        .collect()
}

// - Equality premise

/// Builds `if template = exp` spanning both operands.
fn populate_equality_prem(exp_template: &ast::Exp, exp: &ast::Exp) -> ast::Prem {
    let span = [&exp_template, &exp].at();
    let op = ast::CmpOp::Bool(prim::bool::CmpOp::Eq);
    let exp_template = Box::new(exp_template.clone());
    let exp = Box::new(exp.clone());
    let exp_kind = ast::ExpKind::Cmp(op, ast::OpTyp::Bool, exp_template, exp);
    let exp_match = note_phrase! {
        node: exp_kind,
        note: ast::TypKind::Bool,
        span: span.clone(),
    };
    let if_prem = ast::IfPrem { exp: exp_match };
    let prem_kind = ast::PremKind::If(if_prem);
    phrase!(node: prem_kind, span: span)
}

// - Expressions by rule

/// Emits the equality premises of every rule.
fn populate_exps_by_rule(
    ids_unifier: &IdSet,
    exps_template: &[ast::Exp],
    exps_by_rule: &[Vec<ast::Exp>],
) -> Vec<Vec<ast::Prem>> {
    exps_by_rule
        .iter()
        .map(|exps| populate_exps(ids_unifier, exps_template.iter(), exps.iter()))
        .collect()
}

// == Entry point

/// Anti-unifies input paths into shared templates plus per-path premises.
#[allow(clippy::type_complexity)]
pub fn antiunify(
    ctx: &mut Context,
    exps_by_rule: Vec<Vec<ast::Exp>>,
) -> Result<(Vec<ast::Exp>, Vec<Vec<ast::Prem>>), AlgoError> {
    let mut ids_free = ctx.frees.clone();
    let (ids_unifier, exps_template) =
        overlap_exps_across_rules(&ctx.tdenv, &ctx.menv, &mut ids_free, &exps_by_rule)?;
    let prems_by_rule = populate_exps_by_rule(&ids_unifier, &exps_template, &exps_by_rule);
    // Only unifier names from the successful overlap become free
    ctx.add_frees(&ids_unifier);
    Ok((exps_template, prems_by_rule))
}
