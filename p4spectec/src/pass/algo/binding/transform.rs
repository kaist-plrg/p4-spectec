//! Binding lowering from IL to AL
//!
//! 1. Collect all binding occurrences of variables in an IL construct
//!    - Check that all binding occurrences reside in invertible constructs
//! 2. Rename multi/parallel binding occurrences
//!
//!    -- let (int, int) = ...
//!
//!    becomes
//!
//!    -- let (int, int') = ..., -- if int = int'
//!
//! 3. Desugar partial bindings, occurring as either:
//!    1. Bound values occurring inside binder patterns
//!
//!       -- let PATTERN (a, 1 + 2) = ...
//!
//!       becomes
//!
//!       -- let PATTERN (a, int) = ..., -- if int == 1 + 2
//!
//!    2. Injection of a variant case
//!
//!       -- let PATTERN (a, int) = pat
//!
//!       becomes
//!
//!       -- if pat matches PATTERN, -- let PATTERN (a, b) = pat
//!
//!    3. Injection of a subtype case
//!
//!       -- let ((typ) child) = parent
//!
//!       becomes
//!
//!       -- if parent <: child, -- let child = parent as child
//!
//! At this point, binder patterns are one of:
//!
//! - `VarE`, `TupleE`, `CaseE` of a singleton case, or `StrE`
//! - `IterE` of the above cases

use std::rc::Rc;

use crate::lang::{
    common::{
        prim,
        source::{Phrase, Span},
    },
    data::notation::tree as notation,
    hints::input::{self, InputHint},
    traits::{
        at::At,
        free::{FreeIds, FreeVars},
        has_call::HasCall,
    },
};

use crate::lang::il::ast;

use crate::lang::al;

use crate::runtime::{dim::Dim, envs::algo::VEnv, typdef::TypeDef};

use crate::phrase;

use super::super::{AlgoError, error};

use super::{
    antiunify,
    bind::BEnv,
    collect,
    context::Context,
    dimension,
    iteration::{ICtx, Iteration},
    multiple, partial,
    pattern::{self, PatternSet},
    shallow,
};

// == Helpers

// - Errors

fn input_error(error: input::InputError, span: Span) -> AlgoError {
    error::rule::relation_input_hint_invalid(error, span)
}

// - Environments

/// Extends the bound variables with the renames of the multiple pass.
///
/// After `let (int, int) = e` became `let (int, int') = e`,
/// `int'` is bound too, at the dimension of `int`.
fn update_venv_multiple(venv: &mut VEnv, renv: &multiple::RenameEnv) {
    for (id, ids_rename) in renv.iter() {
        let dim = venv
            .get(id)
            .expect("multiple-bound variable must exist in the variable environment")
            .clone();
        for id_rename in ids_rename {
            venv.insert(id_rename.clone(), dim.clone());
        }
    }
}

/// Extends the bound variables with the fresh destinations of the partial pass.
///
/// After `let (a, 1 + 2) = e` became `let (a, int) = e` with `if int = 1 + 2`,
/// `int` is bound too, at its own dimension under the iterations enclosing it.
fn update_venv_partial(venv: &mut VEnv, renv: &partial::RenameEnv) {
    for rename in &renv.renames {
        let mut iters = rename.destination.iters.clone();
        iters.extend(rename.iter_ctx.iters());
        venv.insert(rename.destination.id.clone(), Dim::new(rename.destination.typ.clone(), iters));
    }
}

/// A lowered premise with its source or binding rewrite provenance.
enum AnalyzedPrem {
    Original(al::ast::Prem),
    Generated(GeneratedPrem),
}

enum GeneratedPrem {
    Multibind(multiple::AnalyzedPrem),
    Partialbind(partial::AnalyzedPrem),
}

impl AnalyzedPrem {
    fn as_prem(&self) -> &al::ast::Prem {
        match self {
            Self::Original(prem_al) => prem_al,
            Self::Generated(GeneratedPrem::Multibind(prem_analyzed)) => &prem_analyzed.prem_al,
            Self::Generated(GeneratedPrem::Partialbind(prem_analyzed)) => match prem_analyzed {
                partial::AnalyzedPrem::Condition { prem_al, .. }
                | partial::AnalyzedPrem::Binding { prem_al } => prem_al,
            },
        }
    }

    fn into_prem(self) -> al::ast::Prem {
        match self {
            Self::Original(prem_al) => prem_al,
            Self::Generated(GeneratedPrem::Multibind(prem_analyzed)) => prem_analyzed.prem_al,
            Self::Generated(GeneratedPrem::Partialbind(prem_analyzed)) => prem_analyzed.into_prem(),
        }
    }
}

/// Orders partialbind premises before multibind checks.
fn generated_prems(
    prems_partial: Vec<partial::AnalyzedPrem>,
    prems_multiple: Vec<multiple::AnalyzedPrem>,
) -> Vec<AnalyzedPrem> {
    let mut prems_analyzed = prems_partial
        .into_iter()
        .map(|prem_analyzed| AnalyzedPrem::Generated(GeneratedPrem::Partialbind(prem_analyzed)))
        .collect::<Vec<_>>();
    prems_analyzed.extend(
        prems_multiple
            .into_iter()
            .map(|prem_analyzed| AnalyzedPrem::Generated(GeneratedPrem::Multibind(prem_analyzed))),
    );
    prems_analyzed
}

// == Expression binding analysis

/// Analyzes binding expressions: collect, rename repeats, desugar partials.
///
/// Returns the bound variables, the rewritten expressions,
/// and the premises the rewrites require.
fn analyze_exps_as_bind(
    ctx: &mut Context,
    iter_ctx: &ICtx,
    exps_il: &[ast::Exp],
) -> Result<(VEnv, Vec<ast::Exp>, Vec<AnalyzedPrem>), AlgoError> {
    // Collect binders and check invertibility
    let benv = collect::collect_exps(ctx, exps_il)?;
    let mut venv = benv.flatten();

    // Rename repeated occurrences and add their equalities
    let mut renv_multiple = multiple::RenameEnv::from_bindings(&benv);
    let exps_al = multiple::rename_exps(ctx, &mut renv_multiple, exps_il);
    update_venv_multiple(&mut venv, &renv_multiple);
    let prems_multiple = multiple::generate_side_conditions(iter_ctx, &renv_multiple);

    // Desugar partially bound patterns into guards and lets
    let mut renv_partial = partial::RenameEnv::new();
    let mut iter_ctx_exp = ICtx::new();
    let exps_al =
        partial::rename_exps(ctx, &mut renv_partial, &mut iter_ctx_exp, &venv.domain(), exps_al)?;
    update_venv_partial(&mut venv, &renv_partial);
    let prems_partial = partial::gen_prems(ctx, iter_ctx, &renv_partial)?;
    Ok((venv, exps_al, generated_prems(prems_partial, prems_multiple)))
}

/// Requires an expression in bound position to bind nothing.
fn analyze_exp_as_bound(ctx: &Context, exp: &ast::Exp) -> Result<(), AlgoError> {
    if let Some(var) = exp
        .free_vars()
        .iter()
        .find(|var| !ctx.venv.contains_key(&var.id))
    {
        Err(error::binding::expression_variable_unbound(&var.id))
    } else {
        Ok(())
    }
}

/// Checks a relation input as a read-only expression at its input hint.
fn analyze_relation_input_as_bound(
    ctx: &Context,
    exp_input_il: &ast::Exp,
    id_relation: &ast::Id,
    idx_input: &Phrase<usize>,
) -> Result<(), AlgoError> {
    if let Some(var) = exp_input_il
        .free_vars()
        .iter()
        .find(|var| !ctx.venv.contains_key(&var.id))
    {
        Err(error::binding::relation_input_variable_unbound(&var.id, id_relation, idx_input))
    } else {
        Ok(())
    }
}

fn analyze_exps_as_bound(ctx: &Context, exps: &[ast::Exp]) -> Result<(), AlgoError> {
    for exp in exps {
        analyze_exp_as_bound(ctx, exp)?;
    }
    Ok(())
}

// == Argument binding analysis

/// Analyzes binding arguments like `analyze_exps_as_bind`, with no iteration.
fn analyze_args_as_bind(
    ctx: &mut Context,
    args_il: &[ast::Arg],
) -> Result<(VEnv, Vec<ast::Arg>, Vec<AnalyzedPrem>), AlgoError> {
    // Collect binders, then rename repeated occurrences
    let benv = collect::collect_args(ctx, args_il)?;
    let mut venv = benv.flatten();

    let mut renv_multiple = multiple::RenameEnv::from_bindings(&benv);
    let args_al = multiple::rename_args(ctx, &mut renv_multiple, args_il);
    update_venv_multiple(&mut venv, &renv_multiple);
    let prems_multiple = multiple::generate_side_conditions(&ICtx::new(), &renv_multiple);

    // Desugar partially bound patterns
    let mut renv_partial = partial::RenameEnv::new();
    let mut iter_ctx_arg = ICtx::new();
    let args_al =
        partial::rename_args(ctx, &mut renv_partial, &mut iter_ctx_arg, &venv.domain(), args_al)?;
    update_venv_partial(&mut venv, &renv_partial);
    let prems_partial = partial::gen_prems(ctx, &ICtx::new(), &renv_partial)?;
    Ok((venv, args_al, generated_prems(prems_partial, prems_multiple)))
}

/// Analyzes table row arguments, which must be shallow and free of repeats.
fn analyze_args_as_bind_shallow(
    ctx: &mut Context,
    args_il: &[ast::Arg],
) -> Result<(VEnv, Vec<ast::Arg>, Vec<al::ast::Prem>), AlgoError> {
    // Translate shallow binding failures into table diagnostics
    shallow::check_args(&ctx.venv, args_il).map_err(|failure| match failure {
        shallow::ShallowFailure::ShapeInvalid(arg) => {
            error::table::table_binding_shape_invalid(arg)
        }
        shallow::ShallowFailure::BindingRepeated { id, id_previous } => {
            error::table::table_binding_repeated(id, &id_previous.at())
        }
    })?;

    // Collect binders and rename repeated occurrences
    let benv = collect::collect_args(ctx, args_il)?;
    let mut venv = benv.flatten();
    let mut renv_multiple = multiple::RenameEnv::from_bindings(&benv);
    let args_al = multiple::rename_args(ctx, &mut renv_multiple, args_il);
    update_venv_multiple(&mut venv, &renv_multiple);
    // Table validation rejects repeated binders before renaming
    let prems_multiple = multiple::generate_side_conditions(&ICtx::new(), &renv_multiple);
    assert!(
        prems_multiple.is_empty(),
        "validated table bindings generated equality side conditions"
    );

    // Desugar partially bound patterns
    let mut renv_partial = partial::RenameEnv::new();
    let mut iter_ctx_arg = ICtx::new();
    let args_al =
        partial::rename_args(ctx, &mut renv_partial, &mut iter_ctx_arg, &venv.domain(), args_al)?;
    update_venv_partial(&mut venv, &renv_partial);
    let prems_al = partial::gen_prems(ctx, &ICtx::new(), &renv_partial)?
        .into_iter()
        .map(partial::AnalyzedPrem::into_prem)
        .collect();
    Ok((venv, args_al, prems_al))
}

/// Checks the postcondition after all row binders have entered the context.
fn analyze_args_as_bound_shallow(ctx: &Context, args: &[ast::Arg]) -> Result<(), AlgoError> {
    for arg in args {
        // Row admission validated the shapes before binding analysis
        assert!(shallow::check_arg(arg), "validated table argument must remain shallow");
        let benv = collect::collect_arg(ctx, arg)?;
        // lower_table_row inserted every collected binder into the context
        assert!(benv.is_empty(), "validated table binders must be bound");
    }
    Ok(())
}

// == Premise binding analysis

// - Helpers

/// Reports generated checks before scanning other otherwise premises.
fn check_analyzed_prems_in_else(
    prems_analyzed: &[AnalyzedPrem],
    otherwise: &ast::Otherwise,
) -> Result<(), AlgoError> {
    for prem_analyzed in prems_analyzed {
        let error_opt = match prem_analyzed {
            // Multibind introduces an equality for repeated binders
            AnalyzedPrem::Generated(GeneratedPrem::Multibind(multiple::AnalyzedPrem {
                origin: multiple::Origin { id_bound, id_repeated },
                ..
            })) => Some(error::otherwise::otherwise_multibind_invalid(
                id_bound,
                id_repeated,
                otherwise,
            )),
            // Partialbind introduces a match or value check
            AnalyzedPrem::Generated(GeneratedPrem::Partialbind(
                partial::AnalyzedPrem::Condition { origin, .. },
            )) => Some(error::otherwise::otherwise_partialbind_invalid(origin, otherwise)),
            // Original and generated binding premises use the ordinary check
            _ => None,
        };
        if let Some(error) = error_opt {
            return Err(error);
        }
    }
    for prem_analyzed in prems_analyzed {
        if let Some(error) = check_pure_prem_in_else(prem_analyzed.as_prem(), otherwise) {
            return Err(error);
        }
    }
    Ok(())
}

/// Locates a forbidden operation inside a possibly iterated premise.
fn check_pure_prem_in_else(prem: &al::ast::Prem, otherwise: &ast::Otherwise) -> Option<AlgoError> {
    match &prem.node {
        al::ast::PremKind::Rule(_)
        | al::ast::PremKind::IfHold(_)
        | al::ast::PremKind::IfNotHold(_) => {
            Some(error::otherwise::otherwise_relation_call_invalid(&prem.span, otherwise))
        }
        al::ast::PremKind::If(_) => {
            Some(error::otherwise::otherwise_condition_invalid(&prem.span, otherwise))
        }
        al::ast::PremKind::Let(prem) => prem
            .exp_r
            .nested_call()
            .first()
            .map(|exp| error::otherwise::otherwise_function_call_invalid(&exp.at(), otherwise)),
        al::ast::PremKind::Debug(prem) => prem
            .exp
            .nested_call()
            .first()
            .map(|exp| error::otherwise::otherwise_function_call_invalid(&exp.at(), otherwise)),
        al::ast::PremKind::Iter(prem) => check_pure_prem_in_else(&prem.prem, otherwise),
    }
}

// - Premise dispatch

/// Analyzes one premise: bound variables, the AL premise, and side conditions.
fn lower_prem(
    ctx: &mut Context,
    iter_ctx: ICtx,
    prem_il: &ast::Prem,
) -> Result<(VEnv, al::ast::Prem, Vec<AnalyzedPrem>), AlgoError> {
    match &prem_il.node {
        ast::PremKind::Rule(rule_prem_il) => {
            lower_rule_prem(ctx, iter_ctx, &prem_il.span, rule_prem_il)
        }
        ast::PremKind::If(if_prem_il) => lower_if_prem(ctx, iter_ctx, &prem_il.span, if_prem_il),
        ast::PremKind::IfHold(if_prem_il) => {
            lower_if_hold_prem(ctx, iter_ctx, &prem_il.span, if_prem_il)
        }
        ast::PremKind::IfNotHold(if_prem_il) => {
            lower_if_not_hold_prem(ctx, iter_ctx, &prem_il.span, if_prem_il)
        }
        ast::PremKind::Iter(iter_prem_il) => {
            lower_iter_prem(ctx, iter_ctx, &prem_il.span, iter_prem_il)
        }
        ast::PremKind::Debug(debug_prem_il) => {
            lower_debug_prem(ctx, iter_ctx, &prem_il.span, debug_prem_il)
        }
    }
}

// - Rule premises

/// Analyzes a rule premise: inputs must be bound, outputs bind.
fn lower_rule_prem(
    ctx: &mut Context,
    iter_ctx: ICtx,
    span: &Span,
    rule_prem_il: &ast::RulePrem,
) -> Result<(VEnv, al::ast::Prem, Vec<AnalyzedPrem>), AlgoError> {
    let mixop = Rc::clone(rule_prem_il.not_exp.mixop());
    let exps_il = rule_prem_il.not_exp.args().to_vec();
    let (exps_input_il, exps_output_il) = input::split(&rule_prem_il.input_hint, exps_il)
        .map_err(|error| input_error(error, span.clone()))?;
    // Inputs are bound, outputs are binders
    let mut idxs_input = rule_prem_il.input_hint.indices().iter().collect::<Vec<_>>();
    idxs_input.sort_by_key(|idx| idx.node);
    for (exp_input_il, idx_input) in exps_input_il.iter().zip(idxs_input) {
        analyze_relation_input_as_bound(ctx, exp_input_il, &rule_prem_il.id, idx_input)?;
    }
    let (venv, exps_output_al, prem_sideconditions_al) =
        analyze_exps_as_bind(ctx, &iter_ctx, &exps_output_il)?;
    let exps_al =
        input::combine(&rule_prem_il.input_hint, exps_input_il.clone(), exps_output_al.clone())
            .map_err(|error| input_error(error, span.clone()))?;
    let not_exp_al = notation::Mixfix::new(mixop, exps_al)
        .expect("arguments obtained from the same mixfix must match its arity");
    let prem_al = phrase! {
        node: al::ast::PremKind::Rule(al::ast::RulePrem {
            id: rule_prem_il.id.clone(),
            not_exp: not_exp_al,
            input_hint: rule_prem_il.input_hint.clone(),
        }),
        span: span.clone(),
    };
    // Iterations use input variables as sources and bind output variables
    let venv_bound = dimension::infer_exps(&exps_input_il);
    let mut iter_ctx = iter_ctx;
    iter_ctx.filter_bound(|var| {
        venv_bound
            .get(&var.id)
            .is_some_and(|dim_bound| dim_bound.sub(&Dim::new(var.typ.clone(), var.iters.clone())))
    });
    iter_ctx.add_vars_bind(dimension::infer_exps(&exps_output_al));
    iter_ctx.validate(span.clone())?;
    let prem_al = iter_ctx.iterate_prem(prem_al);
    Ok((venv, prem_al, prem_sideconditions_al))
}

// - Conditional premises

/// Turns `if a = b` into a let when exactly one side binds.
fn lower_if_eq_prem(
    ctx: &mut Context,
    iter_ctx: ICtx,
    span: &Span,
    if_prem_il: &ast::IfPrem,
    exp_l_il: &ast::Exp,
    exp_r_il: &ast::Exp,
) -> Result<(VEnv, al::ast::Prem, Vec<AnalyzedPrem>), AlgoError> {
    let benv_l = collect::collect_exp(ctx, exp_l_il)?;
    let benv_r = collect::collect_exp(ctx, exp_r_il)?;
    match (benv_l.is_empty(), benv_r.is_empty()) {
        // Neither side binds: keep the condition
        (true, true) => {
            let prem_al = phrase! {
                node: al::ast::PremKind::If(al::ast::IfPrem {
                    exp: if_prem_il.exp.clone(),
                }),
                span: span.clone(),
            };
            Ok((VEnv::new(), iter_ctx.iterate_prem(prem_al), vec![]))
        }
        // Both sides binding is ambiguous
        (false, true) => lower_let_prem(ctx, iter_ctx, span, exp_l_il, &benv_l, exp_r_il),
        (true, false) => lower_let_prem(ctx, iter_ctx, span, exp_r_il, &benv_r, exp_l_il),
        (false, false) => {
            Err(error::binding::equality_binding_invalid(&if_prem_il.exp.span, &benv_l, &benv_r))
        }
    }
}

/// Analyzes a condition, which may become a let when it is an equality.
fn lower_if_prem(
    ctx: &mut Context,
    iter_ctx: ICtx,
    span: &Span,
    if_prem_il: &ast::IfPrem,
) -> Result<(VEnv, al::ast::Prem, Vec<AnalyzedPrem>), AlgoError> {
    // An equality may bind one of its sides
    if let ast::ExpKind::Cmp(ast::CmpOp::Bool(prim::bool::CmpOp::Eq), _, exp_l_il, exp_r_il) =
        &if_prem_il.exp.node
    {
        lower_if_eq_prem(ctx, iter_ctx, span, if_prem_il, exp_l_il, exp_r_il)
    } else {
        analyze_exp_as_bound(ctx, &if_prem_il.exp)?;
        let prem_al = phrase! {
            node: al::ast::PremKind::If(al::ast::IfPrem {
                exp: if_prem_il.exp.clone(),
            }),
            span: span.clone(),
        };
        Ok((VEnv::new(), iter_ctx.iterate_prem(prem_al), vec![]))
    }
}

// - Holding premises

/// Analyzes a holding check, whose arguments must all be bound.
fn lower_if_hold_prem(
    ctx: &mut Context,
    iter_ctx: ICtx,
    span: &Span,
    if_prem_il: &ast::IfHoldPrem,
) -> Result<(VEnv, al::ast::Prem, Vec<AnalyzedPrem>), AlgoError> {
    // Every argument must already be bound
    for exp_il in if_prem_il.not_exp.args() {
        analyze_exp_as_bound(ctx, exp_il)?;
    }
    let prem_al = phrase! {
        node: al::ast::PremKind::IfHold(al::ast::IfHoldPrem {
            id: if_prem_il.id.clone(),
            not_exp: if_prem_il.not_exp.clone(),
        }),
        span: span.clone(),
    };
    Ok((VEnv::new(), iter_ctx.iterate_prem(prem_al), vec![]))
}

// - Non-holding premises

/// Analyzes a non-holding check, whose arguments must all be bound.
fn lower_if_not_hold_prem(
    ctx: &mut Context,
    iter_ctx: ICtx,
    span: &Span,
    if_prem_il: &ast::IfNotHoldPrem,
) -> Result<(VEnv, al::ast::Prem, Vec<AnalyzedPrem>), AlgoError> {
    // Every argument must already be bound
    for exp_il in if_prem_il.not_exp.args() {
        analyze_exp_as_bound(ctx, exp_il)?;
    }
    let prem_al = phrase! {
        node: al::ast::PremKind::IfNotHold(al::ast::IfNotHoldPrem {
            id: if_prem_il.id.clone(),
            not_exp: if_prem_il.not_exp.clone(),
        }),
        span: span.clone(),
    };
    Ok((VEnv::new(), iter_ctx.iterate_prem(prem_al), vec![]))
}

// - Let premises

/// Analyzes `let pattern = exp`, rewriting the pattern side.
fn lower_let_prem(
    ctx: &mut Context,
    iter_ctx: ICtx,
    span: &Span,
    exp_l_il: &ast::Exp,
    benv_l: &BEnv,
    exp_r_il: &ast::Exp,
) -> Result<(VEnv, al::ast::Prem, Vec<AnalyzedPrem>), AlgoError> {
    // Rename repeated binders in the pattern
    let mut venv = benv_l.flatten();
    let mut renv_multiple = multiple::RenameEnv::from_bindings(benv_l);
    let exp_l_al = multiple::rename_exp(ctx, &mut renv_multiple, exp_l_il);
    update_venv_multiple(&mut venv, &renv_multiple);
    let prems_multiple = multiple::generate_side_conditions(&iter_ctx, &renv_multiple);

    // Desugar partially bound patterns
    let mut renv_partial = partial::RenameEnv::new();
    let mut iter_ctx_exp = ICtx::new();
    let exp_l_al =
        partial::rename_exp(ctx, &mut renv_partial, &mut iter_ctx_exp, &venv.domain(), exp_l_al)?;
    update_venv_partial(&mut venv, &renv_partial);
    let prems_partial = partial::gen_prems(ctx, &iter_ctx, &renv_partial)?;
    let prems_analyzed = generated_prems(prems_partial, prems_multiple);

    let prem_al = phrase! {
        node: al::ast::PremKind::Let(al::ast::LetPrem {
            exp_l: exp_l_al.clone(),
            exp_r: exp_r_il.clone(),
        }),
        span: span.clone(),
    };
    // Iterations use the right side's variables as sources and bind the left side's
    let venv_l = dimension::infer_exp(&exp_l_al);
    let venv_r = dimension::infer_exp(exp_r_il);
    let mut iter_ctx = iter_ctx;
    iter_ctx.filter_bound(|var| {
        venv_r
            .get(&var.id)
            .is_some_and(|dim_r| dim_r.sub(&Dim::new(var.typ.clone(), var.iters.clone())))
    });
    iter_ctx.add_vars_bind(venv_l);
    iter_ctx.validate(span.clone())?;
    let prem_al = iter_ctx.iterate_prem(prem_al);
    Ok((venv, prem_al, prems_analyzed))
}

// - Iteration premises

/// Pushes the iteration onto the context and analyzes the inner premise.
fn lower_iter_prem(
    ctx: &mut Context,
    iter_ctx: ICtx,
    span: &Span,
    iter_prem_il: &ast::IterPrem,
) -> Result<(VEnv, al::ast::Prem, Vec<AnalyzedPrem>), AlgoError> {
    if !iter_prem_il.prem_iter.vars_bind.is_empty() {
        return Err(error::binding::iteration_binding_invalid(span));
    }
    let mut iterations = vec![Iteration {
        iter: iter_prem_il.prem_iter.iter,
        vars_bound: iter_prem_il.prem_iter.vars_bound.clone(),
        vars_bind: vec![],
    }];
    iterations.extend(iter_ctx.as_slice().iter().cloned());
    lower_prem(ctx, ICtx::from_iterations(iterations), &iter_prem_il.prem)
}

// - Debug premises

fn lower_debug_prem(
    ctx: &mut Context,
    iter_ctx: ICtx,
    span: &Span,
    debug_prem_il: &ast::DebugPrem,
) -> Result<(VEnv, al::ast::Prem, Vec<AnalyzedPrem>), AlgoError> {
    analyze_exp_as_bound(ctx, &debug_prem_il.exp)?;
    let prem_al = phrase! {
        node: al::ast::PremKind::Debug(al::ast::DebugPrem {
            exp: debug_prem_il.exp.clone(),
        }),
        span: span.clone(),
    };
    Ok((VEnv::new(), iter_ctx.iterate_prem(prem_al), vec![]))
}

// - Premise lists

/// Analyzes premises in order, binding each one's variables for the next.
fn lower_prems(
    ctx: &mut Context,
    prems_il: Vec<ast::Prem>,
) -> Result<Vec<AnalyzedPrem>, AlgoError> {
    let mut prems_analyzed = Vec::new();
    for prem_il in &prems_il {
        // Variables bound here are visible to later premises
        let (venv, prem_al, prem_sideconditions_al) = lower_prem(ctx, ICtx::new(), prem_il)?;
        ctx.add_bounds(&venv);
        prems_analyzed.push(AnalyzedPrem::Original(prem_al));
        prems_analyzed.extend(prem_sideconditions_al);
    }
    Ok(prems_analyzed)
}

// == Rule lowering

/// Anti-unifies rule inputs into one signature and binds its variables.
#[allow(clippy::type_complexity)]
fn lower_rule_match(
    ctx: &mut Context,
    exps_input_by_rule_il: Vec<Vec<ast::Exp>>,
    otherwise_opt: Option<&ast::Otherwise>,
) -> Result<(al::ast::RuleMatch, Vec<Vec<ast::Prem>>), AlgoError> {
    let (exps_signature_al, prems_unified_by_rule_il) =
        antiunify::antiunify(ctx, exps_input_by_rule_il)?;
    let (venv, exps_input_al, prems_analyzed) =
        analyze_exps_as_bind(ctx, &ICtx::new(), &exps_signature_al)?;
    // Nothing may remain free in the shared signature
    ctx.add_bounds(&venv);
    analyze_exps_as_bound(ctx, &exps_signature_al)?;
    if let Some(otherwise) = otherwise_opt {
        check_analyzed_prems_in_else(&prems_analyzed, otherwise)?;
    }

    let rule_match_al = al::ast::RuleMatch {
        exps_signature: exps_signature_al,
        exps_input: exps_input_al,
        prems: prems_analyzed
            .into_iter()
            .map(AnalyzedPrem::into_prem)
            .collect(),
    };
    Ok((rule_match_al, prems_unified_by_rule_il))
}

/// Analyzes one rule's own premises and checks that its outputs are bound.
fn lower_rule_path(
    ctx: &mut Context,
    id: ast::Id,
    prems_unified_analyzed: Vec<AnalyzedPrem>,
    prems_il: Vec<ast::Prem>,
    exps_output_il: Vec<ast::Exp>,
    otherwise_opt: Option<&ast::Otherwise>,
) -> Result<al::ast::RulePath, AlgoError> {
    let prems_analyzed = lower_prems(ctx, prems_il)?;
    let mut prems_all_analyzed = prems_unified_analyzed;
    prems_all_analyzed.extend(prems_analyzed);
    if let Some(otherwise) = otherwise_opt {
        check_analyzed_prems_in_else(&prems_all_analyzed, otherwise)?;
    }
    analyze_exps_as_bound(ctx, &exps_output_il)?;
    let prems_al = prems_all_analyzed
        .into_iter()
        .map(AnalyzedPrem::into_prem)
        .collect();
    Ok(al::ast::RulePath { id, prems: prems_al, exps_output: exps_output_il })
}

/// Shares the input match across rules, then analyzes each rule's path.
fn lower_rule_group(
    ctx: &mut Context,
    inputs: &InputHint,
    rule_group_il: ast::RuleGroup,
    otherwise_opt: Option<&ast::Otherwise>,
) -> Result<al::ast::RuleGroup, AlgoError> {
    let mut ctx = ctx.clone();
    let span = rule_group_il.span;
    let ast::RuleGroupKind { id: id_group, rules: rules_il } = rule_group_il.node;
    let mut ids = Vec::with_capacity(rules_il.len());
    let mut prems_by_rule_il = Vec::with_capacity(rules_il.len());
    let mut exps_input_by_rule_il = Vec::with_capacity(rules_il.len());
    let mut exps_output_by_rule_il = Vec::with_capacity(rules_il.len());
    // Split every rule's conclusion into inputs and outputs by the hint
    for rule_il in rules_il {
        ctx.add_frees(&rule_il.free_ids());
        let rule_span = rule_il.span;
        let ast::RuleKind { id, not_exp, prems } = rule_il.node;
        ids.push(id);
        prems_by_rule_il.push(prems);
        let exps_il = not_exp.into_args();
        let (exps_input_il, exps_output_il) =
            input::split(inputs, exps_il).map_err(|error| input_error(error, rule_span))?;
        exps_input_by_rule_il.push(exps_input_il);
        exps_output_by_rule_il.push(exps_output_il);
    }

    // The match is shared; each path continues from a copy of the context
    let (rule_match_al, prems_unified_by_rule_il) =
        lower_rule_match(&mut ctx, exps_input_by_rule_il, otherwise_opt)?;
    let mut rule_paths_al = Vec::with_capacity(prems_by_rule_il.len());
    for (((id, prems_unified_il), prems_il), exps_output_il) in ids
        .into_iter()
        .zip(prems_unified_by_rule_il)
        .zip(prems_by_rule_il)
        .zip(exps_output_by_rule_il)
    {
        let mut ctx_local = ctx.clone();
        let prems_unified_analyzed = lower_prems(&mut ctx_local, prems_unified_il)?;
        rule_paths_al.push(lower_rule_path(
            &mut ctx_local,
            id,
            prems_unified_analyzed,
            prems_il,
            exps_output_il,
            otherwise_opt,
        )?);
    }
    let rule_group_al = al::ast::RuleGroupKind {
        id: id_group,
        rule_match: rule_match_al,
        rule_paths: rule_paths_al,
    };
    let rule_group_al = phrase!(node: rule_group_al, span: span);
    Ok(rule_group_al)
}

/// Analyzes the otherwise rule as a one-rule group flagged as fallback.
fn lower_else_group(
    ctx: &mut Context,
    inputs: &InputHint,
    else_group_il: ast::ElseGroup,
) -> Result<al::ast::ElseGroup, AlgoError> {
    let span = else_group_il.span;
    let ast::ElseGroupKind { id: id_group, rule: rule_il, otherwise } = else_group_il.node;
    // Reuse the rule group analysis on the single rule
    let rule_group_il = phrase! {
        node: ast::RuleGroupKind { id: id_group, rules: vec![rule_il] },
        span: span.clone(),
    };
    let rule_group_al = lower_rule_group(ctx, inputs, rule_group_il, Some(&otherwise))?;
    let rule_path_al = rule_group_al
        .node
        .rule_paths
        .into_iter()
        .next()
        .expect("else groups contain one rule");
    let else_group_al = al::ast::ElseGroupKind {
        id: rule_group_al.node.id,
        rule_match: rule_group_al.node.rule_match,
        rule_path: rule_path_al,
    };
    let else_group_al = phrase!(node: else_group_al, span: span);
    Ok(else_group_al)
}

// == Clause binding analysis

/// Analyzes a clause: arguments bind, premises follow, the body must be bound.
fn lower_clause(
    ctx: &mut Context,
    clause_il: ast::Clause,
    is_else: bool,
) -> Result<al::ast::Clause, AlgoError> {
    let mut ctx = ctx.clone();
    ctx.add_frees(&clause_il.free_ids());
    let span = clause_il.span;
    let ast::ClauseKind { args: args_il, exp: exp_il, prems: prems_il, otherwise_opt } =
        clause_il.node;
    // Arguments bind first, then premises in order, then the body must be bound
    let (venv, args_al, prems_generated) = analyze_args_as_bind(&mut ctx, &args_il)?;
    ctx.add_bounds(&venv);
    let prems_analyzed = lower_prems(&mut ctx, prems_il)?;
    analyze_exp_as_bound(&ctx, &exp_il)?;
    let otherwise = is_else.then(|| {
        otherwise_opt.unwrap_or_else(|| phrase!(node: ast::OtherwiseKind, span: span.clone()))
    });
    // An otherwise clause may not contain partial premises
    let mut prems_all_analyzed = prems_generated;
    prems_all_analyzed.extend(prems_analyzed);
    if let Some(otherwise) = &otherwise {
        check_analyzed_prems_in_else(&prems_all_analyzed, otherwise)?;
    }
    let prems_all_al = prems_all_analyzed
        .into_iter()
        .map(AnalyzedPrem::into_prem)
        .collect();
    let clause_al = phrase! {
        node: al::ast::ClauseKind {
            args: args_al,
            exp: exp_il,
            prems: prems_all_al,
        },
        span: span,
    };
    Ok(clause_al)
}

// == Table row binding analysis

/// All case notations of a variant type, as the pattern space of one argument.
fn pattern_set_covered_by_typ(ctx: &Context, typ: &ast::Typ) -> Result<PatternSet, AlgoError> {
    let ast::TypKind::Var(id, _) = &typ.node else {
        return Err(error::table::table_pattern_type_invalid(typ, None));
    };
    let (id_decl, typdef) = ctx
        .tdenv
        .get_key_value(id)
        .ok_or_else(|| error::typ::type_undefined(id))?;
    let TypeDef::Defined(_, def_typ) = typdef else {
        return Err(error::table::table_pattern_type_invalid(typ, Some(&id_decl.span)));
    };
    let ast::DefTypKind::Variant(cases) = &def_typ.node else {
        return Err(error::table::table_pattern_type_invalid(typ, Some(&id_decl.span)));
    };
    let pattern_set = cases
        .iter()
        .map(|ast::TypCase { not_typ, .. }| not_typ.clone())
        .collect();
    Ok(pattern_set)
}

/// The cases one table pattern argument matches.
fn pattern_set_covered_by_exp(ctx: &Context, exp_al: &ast::Exp) -> Result<PatternSet, AlgoError> {
    match &exp_al.node {
        // A variable covers every case of its type
        ast::ExpKind::Id(_) => {
            let typ = phrase!(node: exp_al.note.as_ref().clone(), span: exp_al.span.clone());
            pattern_set_covered_by_typ(ctx, &typ)
        }
        // An upcast retains the source variable or case pattern
        ast::ExpKind::UpCast(_, exp_inner) => pattern_set_covered_by_exp(ctx, exp_inner),
        // A case covers exactly its notation
        ast::ExpKind::Case(not_exp) => {
            let not_typ =
                not_exp.map(|exp| phrase!(node: exp.note.as_ref().clone(), span: exp.span.clone()));
            let not_typ = phrase!(node: not_typ, span: exp_al.span.clone());
            Ok([not_typ].into_iter().collect())
        }
        // Row admission rejects every other top-level pattern shape
        _ => unreachable!("validated table signature must be a variable or case"),
    }
}

/// Checks that rows are exclusive and, without a wildcard closer, exhaustive.
fn check_valid_table_rows(
    ctx: &Context,
    span: &Span,
    typs_match_il: &[ast::Typ],
    rows_al: &[al::ast::TableRow],
) -> Result<(), AlgoError> {
    // Validate declared pattern types even when a wildcard row closes the table
    let pattern_sets_total = typs_match_il
        .iter()
        .map(|typ| pattern_set_covered_by_typ(ctx, typ))
        .collect::<Result<_, _>>()?;
    // A final row of wildcards catches everything left
    let has_closer =
        if let Some(row_al) = rows_al.last() {
            row_al.node.exps_signature.iter().all(
                |exp_al| matches!(&exp_al.node, ast::ExpKind::Id(id) if id.node.starts_with('_')),
            )
        } else {
            false
        };
    let rows_pattern_al = if has_closer { &rows_al[..rows_al.len() - 1] } else { rows_al };
    let mut pattern_sets_by_row = Vec::with_capacity(rows_pattern_al.len());
    for row_al in rows_pattern_al {
        let mut pattern_sets = Vec::with_capacity(row_al.node.exps_signature.len());
        for exp_al in &row_al.node.exps_signature {
            let pattern_set = pattern_set_covered_by_exp(ctx, exp_al)?;
            pattern_sets.push(pattern_set);
        }
        let pattern_sets = pattern_sets.into_iter().collect();
        pattern_sets_by_row.push(pattern_sets);
    }
    // Pass the selected rows to the overlap diagnostic
    if let Some((idx, idx_other)) = pattern::find_overlap(span, &pattern_sets_by_row)? {
        return Err(error::table::table_pattern_overlapping(
            &rows_pattern_al[idx_other],
            &rows_pattern_al[idx],
        ));
    }
    // Without a closer, relate missing products to their case declarations
    let patterns_missing = pattern::find_missing(span, &pattern_sets_total, &pattern_sets_by_row)?;
    if !has_closer && !patterns_missing.is_empty() {
        let span = rows_al
            .last()
            .map(|row| Span::new(row.span.right.clone(), row.span.right.clone()))
            .unwrap_or_else(|| span.clone());
        return Err(error::table::table_pattern_incomplete(&span, &patterns_missing));
    }
    Ok(())
}

/// Analyzes a table row: shallow binder arguments, patterns, bound body.
fn lower_table_row(
    ctx: &mut Context,
    row_il: ast::TableRow,
) -> Result<al::ast::TableRow, AlgoError> {
    let mut ctx = ctx.clone();
    ctx.add_frees(&row_il.free_ids());
    let span = row_il.span;
    let ast::TableRowKind { args: args_il, exp: exp_il } = row_il.node;
    // Arguments bind, shallowly
    let (venv, args_input_al, prems_al) = analyze_args_as_bind_shallow(&mut ctx, &args_il)?;
    ctx.add_bounds(&venv);
    analyze_args_as_bound_shallow(&ctx, &args_il)?;
    // The signature patterns are the argument expressions themselves
    let mut exps_signature_al = Vec::with_capacity(args_il.len());
    for arg_il in args_il {
        let ast::ArgKind::Exp(exp_il) = arg_il.node else {
            unreachable!("shallow table validation rejects function arguments");
        };
        exps_signature_al.push(*exp_il);
    }
    // The body must be bound
    analyze_exp_as_bound(&ctx, &exp_il)?;
    let row_al = phrase! {
        node: al::ast::TableRowKind {
            exps_signature: exps_signature_al,
            args: args_input_al,
            exp: exp_il,
            prems: prems_al,
        },
        span: span,
    };
    Ok(row_al)
}

/// Analyzes all rows, then checks them against the parameter types.
fn lower_table_rows(
    ctx: &mut Context,
    span: &Span,
    params_il: &[ast::Param],
    rows_il: Vec<ast::TableRow>,
) -> Result<Vec<al::ast::TableRow>, AlgoError> {
    let mut rows_al = Vec::with_capacity(rows_il.len());
    for row_il in rows_il {
        // Validate public IL row widths before the wildcard-closer shortcut
        if row_il.node.args.len() != params_il.len() {
            return Err(error::table::table_pattern_arity_mismatch(
                &row_il.span,
                params_il.len(),
                row_il.node.args.len(),
            ));
        }
        rows_al.push(lower_table_row(ctx, row_il)?);
    }
    // Table parameters must be plain expressions
    let mut typs_match_il = Vec::with_capacity(params_il.len());
    for param_il in params_il {
        let ast::ParamKind::Exp(typ_il) = &param_il.node else {
            return Err(error::table::table_parameter_invalid(&param_il.span));
        };
        typs_match_il.push(typ_il.clone());
    }
    check_valid_table_rows(ctx, span, &typs_match_il, &rows_al)?;
    Ok(rows_al)
}

// - Type definitions

fn lower_typ_def(typ_def_il: ast::TypDef) -> al::ast::TypDef {
    match typ_def_il {
        ast::TypDef::Extern(extern_typ_il) => {
            al::ast::TypDef::Extern(lower_extern_typ(extern_typ_il))
        }
        ast::TypDef::Defined(defined_typ_il) => {
            let defined_typ_al = lower_defined_typ(*defined_typ_il);
            al::ast::TypDef::Defined(Box::new(defined_typ_al))
        }
    }
}

fn lower_extern_typ(extern_typ_il: ast::ExternTyp) -> al::ast::ExternTyp {
    al::ast::ExternTyp { id: extern_typ_il.id, hints: extern_typ_il.hints }
}

fn lower_defined_typ(defined_typ_il: ast::DefinedTyp) -> al::ast::DefinedTyp {
    al::ast::DefinedTyp {
        id: defined_typ_il.id,
        tparams: defined_typ_il.tparams,
        def_typ: defined_typ_il.def_typ,
        hints: defined_typ_il.hints,
    }
}

// - Meta-variables

fn lower_var_def(var_def_il: ast::VarDef) -> al::ast::VarDef {
    al::ast::VarDef { id: var_def_il.id, typ: var_def_il.typ, hints: var_def_il.hints }
}

// - Relations

fn lower_rel_def(ctx: &mut Context, rel_def_il: ast::RelDef) -> Result<al::ast::RelDef, AlgoError> {
    match rel_def_il {
        ast::RelDef::Extern(extern_rel_il) => {
            let extern_rel_al = lower_extern_rel(*extern_rel_il);
            Ok(al::ast::RelDef::Extern(Box::new(extern_rel_al)))
        }
        ast::RelDef::Defined(defined_rel_il) => {
            let defined_rel_al = lower_defined_rel(ctx, *defined_rel_il)?;
            Ok(al::ast::RelDef::Defined(Box::new(defined_rel_al)))
        }
    }
}

fn lower_extern_rel(extern_rel_il: ast::ExternRel) -> al::ast::ExternRel {
    al::ast::ExternRel {
        id: extern_rel_il.id,
        not_typ: extern_rel_il.not_typ,
        input_hint: extern_rel_il.input_hint,
        hints: extern_rel_il.hints,
    }
}

/// Analyzes every rule group and the otherwise group of a relation.
fn lower_defined_rel(
    ctx: &mut Context,
    defined_rel_il: ast::DefinedRel,
) -> Result<al::ast::DefinedRel, AlgoError> {
    let ast::DefinedRel { id, not_typ, input_hint, rule_groups, else_group, hints } =
        defined_rel_il;
    let mut rule_groups_al = Vec::with_capacity(rule_groups.len());
    for rule_group_il in rule_groups {
        // The group keeps its source span
        let span = rule_group_il.span.clone();
        let mut rule_group_al = lower_rule_group(ctx, &input_hint, rule_group_il, None)?;
        rule_group_al.span = span;
        rule_groups_al.push(rule_group_al);
    }
    let else_group_al = else_group
        .map(|else_group_il| lower_else_group(ctx, &input_hint, else_group_il))
        .transpose()?;
    Ok(al::ast::DefinedRel {
        id,
        not_typ,
        input_hint,
        rule_groups: rule_groups_al,
        else_group: else_group_al,
        hints,
    })
}

// - Meta-functions

fn lower_meta_func_def(
    ctx: &mut Context,
    meta_func_def_il: ast::MetaFuncDef,
    span: &Span,
) -> Result<al::ast::MetaFuncDef, AlgoError> {
    match meta_func_def_il {
        ast::MetaFuncDef::Extern(extern_func_il) => {
            Ok(al::ast::MetaFuncDef::Extern(lower_extern_func(extern_func_il)))
        }
        ast::MetaFuncDef::Builtin(builtin_func_il) => {
            Ok(al::ast::MetaFuncDef::Builtin(lower_builtin_func(builtin_func_il)))
        }
        ast::MetaFuncDef::Table(table_func_il) => {
            Ok(al::ast::MetaFuncDef::Table(lower_table_func(ctx, table_func_il, span)?))
        }
        ast::MetaFuncDef::Defined(defined_func_il) => {
            let defined_func_al = lower_defined_func(ctx, *defined_func_il)?;
            Ok(al::ast::MetaFuncDef::Defined(Box::new(defined_func_al)))
        }
    }
}

fn lower_extern_func(extern_func_il: ast::ExternFunc) -> al::ast::ExternFunc {
    al::ast::ExternFunc {
        id: extern_func_il.id,
        tparams: extern_func_il.tparams,
        params: extern_func_il.params,
        typ: extern_func_il.typ,
        hints: extern_func_il.hints,
    }
}

fn lower_builtin_func(builtin_func_il: ast::BuiltinFunc) -> al::ast::BuiltinFunc {
    al::ast::BuiltinFunc {
        id: builtin_func_il.id,
        tparams: builtin_func_il.tparams,
        params: builtin_func_il.params,
        typ: builtin_func_il.typ,
        hints: builtin_func_il.hints,
    }
}

fn lower_table_func(
    ctx: &mut Context,
    table_func_il: ast::TableFunc,
    span: &Span,
) -> Result<al::ast::TableFunc, AlgoError> {
    let table_rows_al = lower_table_rows(ctx, span, &table_func_il.params, table_func_il.rows)?;
    Ok(al::ast::TableFunc {
        id: table_func_il.id,
        params: table_func_il.params,
        typ: table_func_il.typ,
        table_rows: table_rows_al,
        hints: table_func_il.hints,
    })
}

/// Analyzes every clause and the otherwise clause of a function.
fn lower_defined_func(
    ctx: &mut Context,
    defined_func_il: ast::DefinedFunc,
) -> Result<al::ast::DefinedFunc, AlgoError> {
    let mut clauses_al = Vec::with_capacity(defined_func_il.clauses.len());
    for clause_il in defined_func_il.clauses {
        clauses_al.push(lower_clause(ctx, clause_il, false)?);
    }
    // The otherwise clause is analyzed as the fallback
    let else_clause_al = defined_func_il
        .else_clause
        .map(|clause_il| lower_clause(ctx, clause_il, true))
        .transpose()?;
    Ok(al::ast::DefinedFunc {
        id: defined_func_il.id,
        tparams: defined_func_il.tparams,
        params: defined_func_il.params,
        typ: defined_func_il.typ,
        clauses: clauses_al,
        else_clause: else_clause_al,
        hints: defined_func_il.hints,
    })
}

// - Definitions

fn lower_def(ctx: &mut Context, def_il: ast::Def) -> Result<al::ast::Def, AlgoError> {
    let span = def_il.span;
    let def_kind_al = match def_il.node {
        ast::DefKind::Typ(typ_def_il) => {
            let typ_def_al = lower_typ_def(typ_def_il);
            al::ast::DefKind::Typ(typ_def_al)
        }
        ast::DefKind::Var(var_def_il) => {
            let var_def_al = lower_var_def(var_def_il);
            al::ast::DefKind::Var(var_def_al)
        }
        ast::DefKind::Rel(rel_def_il) => {
            let rel_def_al = lower_rel_def(ctx, rel_def_il)?;
            al::ast::DefKind::Rel(rel_def_al)
        }
        ast::DefKind::MetaFunc(meta_func_def_il) => {
            let meta_func_def_al = lower_meta_func_def(ctx, meta_func_def_il, &span)?;
            al::ast::DefKind::MetaFunc(meta_func_def_al)
        }
    };
    let def_al = phrase!(node: def_kind_al, span: span);
    Ok(def_al)
}

// - Specification

/// Lowers an IL specification to AL while normalizing its bindings.
pub(in crate::pass::algo) fn lower_spec(spec_il: ast::Spec) -> Result<al::ast::Spec, AlgoError> {
    let mut ctx = Context::new();
    ctx.load(&spec_il);
    let mut defs_al = Vec::with_capacity(spec_il.len());
    for def_il in spec_il {
        defs_al.push(lower_def(&mut ctx, def_il)?);
    }
    Ok(defs_al)
}
