//! Otherwise admission after binding normalization and guard insertion
//!
//! `check_spec` checks complete otherwise branches with one purity criterion.
//! IL type annotations prove singleton constructor matches irrefutable;
//! calls and all other conditions are rejected wherever they occur.

use crate::lang::{al::ast, common::source::Span, traits::has_call::HasCall};
use crate::phrase;
use crate::runtime::typdef::TypeDef;

use super::{AlgoError, binding::context::Context, error};

/// Checks that the operand type has exactly the matched constructor.
fn singleton_match(
    ctx: &Context,
    exp: &ast::Exp,
    pattern: &ast::Pattern,
) -> Result<bool, AlgoError> {
    // Option and list shape tests are always refutable
    let ast::Pattern::Case(mixop) = pattern else {
        return Ok(false);
    };
    let typ = phrase!(node: exp.note.as_ref().clone(), span: exp.span);
    ctx.is_singleton_case(&typ, mixop)
}

/// Rejects calls nested in expressions, arguments, and update paths.
fn check_exp(exp: &ast::Exp, span_otherwise: &Span) -> Result<(), AlgoError> {
    if let Some(exp_call) = exp.nested_call().first() {
        return Err(error::otherwise::otherwise_function_call_invalid(
            &exp_call.span,
            span_otherwise,
        ));
    }
    Ok(())
}

/// Applies the same purity criterion to source and generated premises.
fn check_prem(ctx: &Context, prem: &ast::Prem, span_otherwise: &Span) -> Result<(), AlgoError> {
    match &prem.node {
        // Both the pattern and the value must be call-free
        ast::PremKind::Let(prem) => {
            check_exp(&prem.exp_l, span_otherwise)?;
            check_exp(&prem.exp_r, span_otherwise)
        }
        // Debug evaluates its expression without testing it
        ast::PremKind::Debug(prem) => check_exp(&prem.exp, span_otherwise),
        // Iteration retains the criterion of its inner premise
        ast::PremKind::Iter(prem) => check_prem(ctx, &prem.prem, span_otherwise),
        // Only a type-proven singleton constructor condition is irrefutable
        ast::PremKind::If(prem_if) => {
            if let ast::ExpKind::Match(exp, pattern) = &prem_if.exp.node {
                check_exp(exp, span_otherwise)?;
                if singleton_match(ctx, exp, pattern)? {
                    return Ok(());
                }
            }
            Err(error::otherwise::otherwise_condition_invalid(&prem.span, span_otherwise))
        }
        // Relation premises may fail even when their arguments are pure
        ast::PremKind::Rule(_) | ast::PremKind::IfHold(_) | ast::PremKind::IfNotHold(_) => {
            Err(error::otherwise::otherwise_relation_call_invalid(&prem.span, span_otherwise))
        }
    }
}

/// Checks inputs, outputs, and all premises after `guard::insert_spec`.
pub(super) fn check_spec(ctx: &Context, spec_al: &[ast::Def]) -> Result<(), AlgoError> {
    for def_al in spec_al {
        match &def_al.node {
            // Relation fallbacks have separate match and path premises
            ast::DefKind::Rel(ast::RelDef::Defined(defined_rel)) => {
                let Some(else_group) = &defined_rel.else_group else {
                    continue;
                };
                let rule_match = &else_group.node.rule_match;
                let rule_path = &else_group.node.rule_path;
                for exp in rule_match.exps_input.iter().chain(&rule_path.exps_output) {
                    check_exp(exp, &else_group.span)?;
                }
                for prem in rule_match.prems.iter().chain(&rule_path.prems) {
                    check_prem(ctx, prem, &else_group.span)?;
                }
            }
            // Function type parameters are opaque within their own scope
            ast::DefKind::MetaFunc(ast::MetaFuncDef::Defined(defined_func)) => {
                let Some(clause) = &defined_func.else_clause else {
                    continue;
                };
                let mut ctx = ctx.clone();
                for tparam in &defined_func.tparams {
                    ctx.tdenv.insert(tparam.clone(), TypeDef::Parameter);
                }
                for arg in &clause.node.args {
                    if let ast::ArgKind::Exp(exp) = &arg.node {
                        check_exp(exp, &clause.span)?;
                    }
                }
                check_exp(&clause.node.exp, &clause.span)?;
                for prem in &clause.node.prems {
                    check_prem(&ctx, prem, &clause.span)?;
                }
            }
            // Other definitions have no otherwise branch
            _ => {}
        }
    }
    Ok(())
}
