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

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::{frontend::parse::parse_text, note_phrase, pass::elaborate};

    use super::*;

    /// Builds a typed constructor condition independently of match omission.
    fn constructor_check(source: &str, id_typ: &str) -> (Context, ast::Prem) {
        let spec_el = parse_text("otherwise-test".into(), source).expect("valid source");
        let spec_il = elaborate::convert(spec_el).expect("well-typed source");
        let mut ctx = Context::new();
        ctx.load(&spec_il);
        let mixop = ctx
            .tdenv
            .iter()
            .find_map(|(id, typdef)| {
                let TypeDef::Defined(_, def_typ) = typdef else {
                    return None;
                };
                let ast::DefTypKind::Variant(cases) = &def_typ.node else {
                    return None;
                };
                (id.node.as_ref() == "box").then(|| Rc::clone(cases[0].not_typ.node.mixop()))
            })
            .expect("box constructor");
        let span = Span::default();
        let id_typ = phrase!(node: id_typ.into(), span: span);
        let exp = note_phrase! {
            node: ast::ExpKind::Id(phrase!(node: "value".into(), span: span)),
            note: ast::TypKind::Var(id_typ, vec![]),
            span: span,
        };
        let exp = note_phrase! {
            node: ast::ExpKind::Match(Box::new(exp), ast::Pattern::Case(mixop)),
            note: ast::TypKind::Bool,
            span: span,
        };
        let prem = phrase!(node: ast::PremKind::If(ast::IfPrem { exp }), span: span);
        (ctx, prem)
    }

    #[test]
    fn singleton_match_admission_does_not_depend_on_guard_omission() {
        let (ctx, prem) = constructor_check("syntax box = BOX nat", "box");
        assert!(check_prem(&ctx, &prem, &Span::default()).is_ok());
    }

    #[test]
    fn constructor_match_uses_the_operand_type_and_exact_constructor() {
        for source in [
            "syntax box = BOX nat | EMPTY",
            "syntax box = BOX nat\nsyntax other = OTHER nat",
            "syntax box = BOX nat\nsyntax other = BOX nat | EMPTY",
        ] {
            let id_typ = if source.contains("other") { "other" } else { "box" };
            let (ctx, prem) = constructor_check(source, id_typ);
            assert!(check_prem(&ctx, &prem, &Span::default()).is_err(), "{source}");
        }
    }

    #[test]
    fn singleton_alias_substitutes_type_arguments() {
        let source = "syntax box = BOX nat\nsyntax alias<X> = X\nsyntax wrapped = alias<box>";
        let (ctx, prem) = constructor_check(source, "wrapped");
        assert!(check_prem(&ctx, &prem, &Span::default()).is_ok());
    }

    #[test]
    fn local_type_parameters_cannot_prove_singleton_matches() {
        let (mut ctx, prem) = constructor_check("syntax box = BOX nat", "box");
        let id = phrase!(node: "box".into(), span: Span::default());
        ctx.tdenv.insert(id, TypeDef::Parameter);
        assert!(check_prem(&ctx, &prem, &Span::default()).is_err());
    }

    #[test]
    fn let_checks_calls_on_both_sides() {
        let span = Span::default();
        let exp_pure =
            note_phrase!(node: ast::ExpKind::Bool(true), note: ast::TypKind::Bool, span: span);
        let exp_call = note_phrase! {
            node: ast::ExpKind::Call(phrase!(node: "f".into(), span: span), vec![], vec![]),
            note: ast::TypKind::Bool,
            span: span,
        };
        for (exp_l, exp_r) in [(exp_call.clone(), exp_pure.clone()), (exp_pure, exp_call)] {
            let prem = phrase!(node: ast::PremKind::Let(ast::LetPrem { exp_l, exp_r }), span: span);
            assert!(check_prem(&Context::new(), &prem, &span).is_err());
        }
    }

    #[test]
    fn function_type_parameter_scope_is_local_to_each_definition() {
        let source = "syntax box = BOX nat\ndec $f<X>(X) : X\ndef $f<X>(X) = X -- otherwise\nsyntax X = box\ndec $g(X) : X\ndef $g(X) = X -- otherwise";
        let (ctx, prem) = constructor_check(source, "X");
        let spec_el = parse_text("scope-test".into(), source).expect("valid source");
        let spec_il = elaborate::convert(spec_el).expect("well-typed source");
        let mut spec_al =
            crate::pass::algo::binding::transform::lower_spec(spec_il).expect("bound clauses");
        // The same condition is unprovable for f<X> but guaranteed for g
        for def_al in &mut spec_al {
            if let ast::DefKind::MetaFunc(ast::MetaFuncDef::Defined(defined_func)) =
                &mut def_al.node
            {
                defined_func
                    .else_clause
                    .as_mut()
                    .unwrap()
                    .node
                    .prems
                    .push(prem.clone());
                assert_eq!(
                    check_spec(&ctx, std::slice::from_ref(def_al)).is_ok(),
                    matches!(&def_al.node, ast::DefKind::MetaFunc(ast::MetaFuncDef::Defined(defined_func)) if defined_func.id.node.as_ref() == "g")
                );
            }
        }
    }
}
