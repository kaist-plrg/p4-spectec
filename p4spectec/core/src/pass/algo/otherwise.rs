//! Common purity checks for otherwise rules and clauses
//!
//! Binding lowering checks normalized inputs, outputs, and premises
//! while their rewrite provenance is available for diagnostics.
//! Guard insertion checks the final premise lists with the same criterion.
//! Only type-proven singleton constructor conditions are pure.

use crate::lang::{al::ast, il::ast::Otherwise, traits::has_call::HasCall};
use crate::phrase;
use crate::runtime::ops::typ::has_single_constructor;

use super::{AlgoError, binding::context::Context, error};

// == Impurities

/// The first operation rejected by the common purity criterion.
pub(super) enum Impurity<'a> {
    /// A condition not proved to be a singleton constructor match.
    Condition(&'a ast::Prem),
    /// A call nested in an expression, argument, or path.
    FunctionCall(&'a ast::Exp),
    /// A relation premise that may fail.
    RelationCall(&'a ast::Prem),
}

impl Impurity<'_> {
    /// Reports the operation using its source span and the otherwise marker.
    pub(super) fn into_error(self, otherwise: &Otherwise) -> AlgoError {
        match self {
            Self::Condition(prem) => {
                error::otherwise::otherwise_condition_invalid(&prem.span, otherwise)
            }
            Self::FunctionCall(exp) => {
                error::otherwise::otherwise_function_call_invalid(&exp.span, otherwise)
            }
            Self::RelationCall(prem) => {
                error::otherwise::otherwise_relation_call_invalid(&prem.span, otherwise)
            }
        }
    }
}

// == Expression purity

fn impure_exp(exp: &ast::Exp) -> Option<Impurity<'_>> {
    exp.nested_call()
        .into_iter()
        .next()
        .map(Impurity::FunctionCall)
}

/// Rejects calls nested in expressions, arguments, and update paths.
pub(super) fn check_exp(exp: &ast::Exp, otherwise: &Otherwise) -> Result<(), AlgoError> {
    if let Some(impurity) = impure_exp(exp) {
        return Err(impurity.into_error(otherwise));
    }
    Ok(())
}

// == Premise purity

/// Checks whether a well-typed constructor condition must succeed.
fn singleton_match(
    ctx: &Context,
    exp: &ast::Exp,
    pattern: &ast::Pattern,
) -> Result<bool, AlgoError> {
    // Option and list shape tests are always refutable
    let ast::Pattern::Case(_) = pattern else {
        return Ok(false);
    };
    let typ = phrase!(node: exp.note.as_ref().clone(), span: exp.span);
    has_single_constructor(&ctx.tdenv, &typ).map_err(error::typ::type_operation_invalid)
}

/// Classifies a premise independently of its source or generated provenance.
pub(super) fn impure_prem<'a>(
    ctx: &Context,
    prem: &'a ast::Prem,
) -> Result<Option<Impurity<'a>>, AlgoError> {
    let impurity = match &prem.node {
        // Both the pattern and the value must be call-free
        ast::PremKind::Let(prem) => impure_exp(&prem.exp_l).or_else(|| impure_exp(&prem.exp_r)),
        // Debug evaluates its expression without testing it
        ast::PremKind::Debug(prem) => impure_exp(&prem.exp),
        // Iteration retains the criterion of its inner premise
        ast::PremKind::Iter(prem) => return impure_prem(ctx, &prem.prem),
        // Only a type-proven singleton constructor condition is irrefutable
        ast::PremKind::If(prem_if) => {
            if let ast::ExpKind::Match(exp, pattern) = &prem_if.exp.node {
                if let Some(impurity) = impure_exp(exp) {
                    return Ok(Some(impurity));
                }
                if singleton_match(ctx, exp, pattern)? {
                    return Ok(None);
                }
            }
            Some(Impurity::Condition(prem))
        }
        // Relation premises may fail even when their arguments are pure
        ast::PremKind::Rule(_) | ast::PremKind::IfHold(_) | ast::PremKind::IfNotHold(_) => {
            Some(Impurity::RelationCall(prem))
        }
    };
    Ok(impurity)
}

/// Checks final premise lists after guard insertion has filtered redundant guards.
pub(super) fn check_prems<'a>(
    ctx: &Context,
    prems: impl IntoIterator<Item = &'a ast::Prem>,
    otherwise: &Otherwise,
) -> Result<(), AlgoError> {
    for prem in prems {
        if let Some(impurity) = impure_prem(ctx, prem)? {
            return Err(impurity.into_error(otherwise));
        }
    }
    Ok(())
}
