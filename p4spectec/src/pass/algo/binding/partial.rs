//! Rewriting of partially bound patterns
//!
//! Bound values inside a binder become fresh variables plus equality premises.
//!
//! -- let PATTERN (a, 1 + 2) = ...
//!
//! becomes
//!
//! -- let PATTERN (a, int) = ..., -- if int = 1 + 2
//!
//! Variant and subtype injections become guards followed by simpler bindings.
//!
//! -- let PATTERN (a, int) = pat
//!
//! becomes
//!
//! -- if pat matches PATTERN, -- let PATTERN (a, b) = pat
//!
//! -- let ((typ) child) = parent
//!
//! becomes
//!
//! -- if parent <: child, -- let child = parent as child
//!
//! Generated premises retain the iteration context of the source pattern.

use std::rc::Rc;

use crate::lang::{
    common::{ds::set::IdSet, prim, source::Span},
    data::notation::tree as notation,
    traits::free::FreeIds,
};

use crate::lang::il::{ast, fresh, var};

use crate::lang::al;

use crate::runtime::{
    dim::Dim,
    ops::typ::{Theta, optimize_sub_typ, subst_typ},
    typdef::TypeDef,
};

use crate::{note_phrase, phrase};

use super::super::{AlgoError, error};

use super::{
    context::Context,
    dimension,
    iteration::{ICtx, Iteration},
};

// == Helpers

/// Checks whether a type is a variant with exactly one case, through aliases.
fn is_singleton_case(ctx: &Context, typ: &ast::Typ) -> Result<bool, AlgoError> {
    let ast::TypKind::Var(id, targs) = &typ.node else {
        return Ok(false);
    };
    let TypeDef::Defined(tparams, def_typ) = ctx.find_typdef(id)? else {
        return Ok(false);
    };
    match &def_typ.node {
        // Follow a plain alias with its type arguments substituted
        ast::DefTypKind::Plain(typ_inner) => {
            let theta = Theta::from_lists(tparams, targs).map_err(|mismatch| {
                error::typ::type_argument_arity_mismatch(
                    &typ.span,
                    mismatch.expected,
                    mismatch.actual,
                )
            })?;
            let typ_inner = subst_typ(&|id| theta.get(id), typ_inner)
                .map_err(error::typ::type_operation_invalid)?;
            is_singleton_case(ctx, &typ_inner)
        }
        ast::DefTypKind::Struct(_) => Ok(false),
        ast::DefTypKind::Variant(cases) => Ok(cases.len() == 1),
    }
}

/// Checks for an upcast of a nullary case, which still needs its subtype guard.
fn is_upcast_terminal(exp: &ast::Exp) -> bool {
    matches!(
        &exp.node,
        ast::ExpKind::UpCast(_, exp_inner)
            if matches!(&exp_inner.node, ast::ExpKind::Case(not_exp) if not_exp.arity() == 0)
    )
}

// == Rename environment

/// What a fresh destination variable stands for.
#[derive(Debug)]
#[allow(clippy::large_enum_variant)]
enum Source {
    /// A bound sub-expression, checked after binding.
    Bound { exp_from: ast::Exp },
    /// A binder injected into a variant, option, or list case.
    BindMatch { pattern: ast::Pattern, exp_from: ast::Exp },
    /// A binder injected into a supertype.
    BindSub { typ_sub: ast::Typ, exp_sub: ast::Exp, exp_from: ast::Exp },
}

/// A fresh variable standing for a rewritten sub-pattern.
#[derive(Debug)]
pub(crate) struct Rename {
    pub(crate) destination: ast::Var,
    source: Source,
    /// Iterations enclosing the sub-pattern at rewrite time.
    pub(crate) iter_ctx: ICtx,
}

/// Renames in the order their premises must appear.
#[derive(Debug)]
pub struct RenameEnv {
    pub(crate) renames: Vec<Rename>,
}

impl RenameEnv {
    pub fn new() -> Self {
        Self { renames: Vec::new() }
    }

    fn prepend(&mut self, rename: Rename) {
        self.renames.insert(0, rename);
    }

    fn append(&mut self, mut other: Self) {
        self.renames.append(&mut other.renames);
    }
}

// == Premise generation

/// Source pattern and reason for a condition introduced by partialbind rewriting.
pub enum Origin {
    Equality(Span),
    Match(Span, &'static str),
    Subtype(Span),
}

/// A partialbind check or the binding premise following it.
pub enum AnalyzedPrem {
    Condition { prem_al: al::ast::Prem, origin: Origin },
    Binding { prem_al: al::ast::Prem },
}

impl AnalyzedPrem {
    /// Discards the rewrite provenance after otherwise validation.
    pub fn into_prem(self) -> al::ast::Prem {
        match self {
            Self::Condition { prem_al, .. } | Self::Binding { prem_al } => prem_al,
        }
    }
}

/// Builds the premise checking a bound sub-expression against its destination.
///
/// Nullary cases, options, and the empty list become match tests;
/// anything else becomes an equality.
fn gen_prem_bound(
    ctx: &Context,
    iter_ctx: &ICtx,
    destination: &ast::Var,
    exp_from: &ast::Exp,
) -> Result<AnalyzedPrem, AlgoError> {
    let exp_l = var::as_exp(true, destination);
    let typ_from = phrase!(node: exp_from.note.as_ref().clone(), span: exp_from.span.clone());
    let (exp_kind, origin) = match &exp_from.node {
        ast::ExpKind::Case(not_exp)
            if not_exp.arity() == 0 && !is_singleton_case(ctx, &typ_from)? =>
        {
            (
                ast::ExpKind::Match(
                    Box::new(exp_l),
                    ast::Pattern::Case(Rc::clone(not_exp.mixop())),
                ),
                Origin::Match(exp_from.span.clone(), "variant case"),
            )
        }
        ast::ExpKind::Opt(Some(_)) => (
            ast::ExpKind::Match(Box::new(exp_l), ast::Pattern::Opt(ast::OptPattern::Some)),
            Origin::Match(exp_from.span.clone(), "option pattern"),
        ),
        ast::ExpKind::Opt(None) => (
            ast::ExpKind::Match(Box::new(exp_l), ast::Pattern::Opt(ast::OptPattern::None)),
            Origin::Match(exp_from.span.clone(), "option pattern"),
        ),
        ast::ExpKind::List(exps) if exps.is_empty() => (
            ast::ExpKind::Match(Box::new(exp_l), ast::Pattern::List(ast::ListPattern::Nil)),
            Origin::Match(exp_from.span.clone(), "list pattern"),
        ),
        _ => (
            ast::ExpKind::Cmp(
                ast::CmpOp::Bool(prim::bool::CmpOp::Eq),
                ast::OpTyp::Bool,
                Box::new(exp_l),
                Box::new(exp_from.clone()),
            ),
            Origin::Equality(exp_from.span.clone()),
        ),
    };
    let exp_cond = note_phrase! {
        node: exp_kind,
        note: ast::TypKind::Bool,
        span: exp_from.span.clone(),
    };
    let side_condition = phrase! {
        node: al::ast::PremKind::If(al::ast::IfPrem { exp: exp_cond }),
        span: exp_from.span.clone(),
    };
    // Keep only the iteration sources used by the source expression
    // Iterate the check under the destination's dimension
    let mut iter_ctx = iter_ctx.clone();
    let venv = dimension::infer_exp(exp_from);
    iter_ctx.filter_bound(|var| {
        venv.get(&var.id)
            .is_some_and(|dim_source| dim_source.sub(&Dim::new(var.typ.clone(), var.iters.clone())))
    });
    iter_ctx.add_var_bound(
        destination.id.clone(),
        destination.typ.clone(),
        destination.iters.clone(),
    );
    let prem_al = iter_ctx.iterate_prem(side_condition);
    Ok(AnalyzedPrem::Condition { prem_al, origin })
}

/// Builds `if x matches PATTERN` followed by `let PATTERN = x`.
fn gen_prem_bind_match(
    iter_ctx: &ICtx,
    destination: &ast::Var,
    pattern: &ast::Pattern,
    exp_from: &ast::Exp,
) -> Vec<AnalyzedPrem> {
    let exp_to = var::as_exp(true, destination);
    let exp_guard_match = note_phrase! {
        node: ast::ExpKind::Match(Box::new(exp_to.clone()), pattern.clone()),
        note: ast::TypKind::Bool,
        span: exp_from.span.clone(),
    };
    let side_condition_guard_match = phrase! {
        node: al::ast::PremKind::If(al::ast::IfPrem {
            exp: exp_guard_match,
        }),
        span: exp_from.span.clone(),
    };
    // The guard iterates over the destination only
    let mut iter_ctx_match = ICtx::from_iterations(
        iter_ctx
            .as_slice()
            .iter()
            .map(|entry| Iteration { iter: entry.iter, vars_bound: vec![], vars_bind: vec![] })
            .collect(),
    );
    iter_ctx_match.add_var_bound(
        destination.id.clone(),
        destination.typ.clone(),
        destination.iters.clone(),
    );

    let prem_bind = phrase! {
        node: al::ast::PremKind::Let(al::ast::LetPrem {
            exp_l: exp_from.clone(),
            exp_r: exp_to,
        }),
        span: exp_from.span.clone(),
    };
    // The binding also binds the pattern's own variables
    let mut iter_ctx_bind = ICtx::from_iterations(
        iter_ctx
            .as_slice()
            .iter()
            .map(|entry| Iteration { iter: entry.iter, vars_bound: vec![], vars_bind: vec![] })
            .collect(),
    );
    iter_ctx_bind.add_vars_bind(dimension::infer_exp(exp_from));
    iter_ctx_bind.add_var_bound(
        destination.id.clone(),
        destination.typ.clone(),
        destination.iters.clone(),
    );
    let prem_match = iter_ctx_match.iterate_prem(side_condition_guard_match);
    let prem_bind = iter_ctx_bind.iterate_prem(prem_bind);
    let text_construct = match pattern {
        ast::Pattern::Case(_) => "variant case",
        ast::Pattern::List(_) => "list pattern",
        ast::Pattern::Opt(_) => "option pattern",
    };
    vec![
        AnalyzedPrem::Condition {
            prem_al: prem_match,
            origin: Origin::Match(exp_from.span.clone(), text_construct),
        },
        AnalyzedPrem::Binding { prem_al: prem_bind },
    ]
}

/// Builds a subtype guard on the destination, then a let with the downcast.
fn gen_prem_bind_sub(
    ctx: &Context,
    iter_ctx: &ICtx,
    destination: &ast::Var,
    typ_sub: &ast::Typ,
    exp_sub: &ast::Exp,
    exp_from: &ast::Exp,
) -> Result<Vec<AnalyzedPrem>, AlgoError> {
    let exp_to = var::as_exp(true, destination);
    // Compute the subtype check once
    let typ_source = phrase!(node: exp_to.note.as_ref().clone(), span: exp_to.span.clone());
    let subcheck = optimize_sub_typ(&ctx.tdenv, &typ_source, typ_sub)
        .map_err(error::typ::type_operation_invalid)?;
    let exp_guard_sub = note_phrase! {
        node: ast::ExpKind::Sub(
            Box::new(exp_to.clone()),
            Box::new(typ_sub.clone()),
            Box::new(subcheck),
        ),
        note: ast::TypKind::Bool,
        span: exp_from.span.clone(),
    };
    let side_condition_guard_sub = phrase! {
        node: al::ast::PremKind::If(al::ast::IfPrem { exp: exp_guard_sub }),
        span: exp_from.span.clone(),
    };
    // The guard iterates over the destination only
    let mut iter_ctx_sub = ICtx::from_iterations(
        iter_ctx
            .as_slice()
            .iter()
            .map(|entry| Iteration { iter: entry.iter, vars_bound: vec![], vars_bind: vec![] })
            .collect(),
    );
    iter_ctx_sub.add_var_bound(
        destination.id.clone(),
        destination.typ.clone(),
        destination.iters.clone(),
    );

    // Bind the inner pattern to the downcast destination
    let exp_downcast = note_phrase! {
        node: ast::ExpKind::DownCast(Box::new(typ_sub.clone()), Box::new(exp_to)),
        note: typ_sub.node.clone(),
        span: exp_from.span.clone(),
    };
    let prem_bind = phrase! {
        node: al::ast::PremKind::Let(al::ast::LetPrem {
            exp_l: exp_sub.clone(),
            exp_r: exp_downcast,
        }),
        span: exp_from.span.clone(),
    };
    let mut iter_ctx_bind = ICtx::from_iterations(
        iter_ctx
            .as_slice()
            .iter()
            .map(|entry| Iteration { iter: entry.iter, vars_bound: vec![], vars_bind: vec![] })
            .collect(),
    );
    iter_ctx_bind.add_vars_bind(dimension::infer_exp(exp_from));
    iter_ctx_bind.add_var_bound(
        destination.id.clone(),
        destination.typ.clone(),
        destination.iters.clone(),
    );
    let prem_sub = iter_ctx_sub.iterate_prem(side_condition_guard_sub);
    let prem_bind = iter_ctx_bind.iterate_prem(prem_bind);
    Ok(vec![
        AnalyzedPrem::Condition {
            prem_al: prem_sub,
            origin: Origin::Subtype(exp_from.span.clone()),
        },
        AnalyzedPrem::Binding { prem_al: prem_bind },
    ])
}

/// Builds the premises of one rename under the enclosing iterations.
fn gen_prem(
    ctx: &Context,
    iter_ctx_prem: &ICtx,
    rename: &Rename,
) -> Result<Vec<AnalyzedPrem>, AlgoError> {
    // The rename's own iterations sit inside the premise's
    let mut iterations = rename.iter_ctx.as_slice().to_vec();
    iterations.extend(iter_ctx_prem.as_slice().iter().cloned());
    let iter_ctx = ICtx::from_iterations(iterations);
    match &rename.source {
        Source::Bound { exp_from } => {
            let prem_analyzed = gen_prem_bound(ctx, &iter_ctx, &rename.destination, exp_from)?;
            Ok(vec![prem_analyzed])
        }
        Source::BindMatch { pattern, exp_from } => {
            let prems = gen_prem_bind_match(&iter_ctx, &rename.destination, pattern, exp_from);
            Ok(prems)
        }
        Source::BindSub { typ_sub, exp_sub, exp_from } => {
            gen_prem_bind_sub(ctx, &iter_ctx, &rename.destination, typ_sub, exp_sub, exp_from)
        }
    }
}

/// Builds partialbind checks and bindings in rewrite order.
pub fn gen_prems(
    ctx: &Context,
    iter_ctx_prem: &ICtx,
    renv: &RenameEnv,
) -> Result<Vec<AnalyzedPrem>, AlgoError> {
    let mut prems = Vec::new();
    for rename in &renv.renames {
        prems.extend(gen_prem(ctx, iter_ctx_prem, rename)?);
    }
    Ok(prems)
}

// == Expression rewriting

/// Replaces an injected binder by a fresh variable and records a match rename.
fn rename_exp_bind_match(
    ctx: &mut Context,
    renv: &mut RenameEnv,
    iter_ctx: &mut ICtx,
    pattern: ast::Pattern,
    exp_from: ast::Exp,
) -> ast::Exp {
    let typ = phrase!(node: exp_from.note.as_ref().clone(), span: exp_from.span.clone());
    let destination = fresh::var_from_typ(&ctx.menv, &ctx.frees, exp_from.span.clone(), &typ);
    ctx.add_free(destination.id.clone());
    let bounds = exp_from.free_ids();
    renv.prepend(Rename {
        destination: destination.clone(),
        source: Source::BindMatch { pattern, exp_from },
        iter_ctx: iter_ctx.clone(),
    });
    // The fresh variable takes the sub-pattern's place in the iteration
    iter_ctx.filter_bound(|var| !bounds.contains(&var.id));
    iter_ctx.add_var_bound(
        destination.id.clone(),
        destination.typ.clone(),
        destination.iters.clone(),
    );
    var::as_exp(true, &destination)
}

/// Replaces an upcast binder by a fresh variable and records a subtype rename.
fn rename_exp_bind_sub(
    ctx: &mut Context,
    renv: &mut RenameEnv,
    iter_ctx: &mut ICtx,
    typ_sub: ast::Typ,
    exp_sub: ast::Exp,
    exp_from: ast::Exp,
) -> ast::Exp {
    let typ = phrase!(node: exp_from.note.as_ref().clone(), span: exp_from.span.clone());
    let destination = fresh::var_from_typ(&ctx.menv, &ctx.frees, exp_from.span.clone(), &typ);
    ctx.add_free(destination.id.clone());
    let bounds = exp_from.free_ids();
    renv.prepend(Rename {
        destination: destination.clone(),
        source: Source::BindSub { typ_sub, exp_sub, exp_from },
        iter_ctx: iter_ctx.clone(),
    });
    // The fresh variable takes the sub-pattern's place in the iteration
    iter_ctx.filter_bound(|var| !bounds.contains(&var.id));
    iter_ctx.add_var_bound(
        destination.id.clone(),
        destination.typ.clone(),
        destination.iters.clone(),
    );
    var::as_exp(true, &destination)
}

/// Rewrites a pattern; sub-expressions without binders become bound checks.
pub fn rename_exp(
    ctx: &mut Context,
    renv: &mut RenameEnv,
    iter_ctx: &mut ICtx,
    binds: &IdSet,
    exp: ast::Exp,
) -> Result<ast::Exp, AlgoError> {
    let frees = exp.free_ids();
    let has_binding = binds.iter().any(|id| frees.contains(id));
    if !has_binding && !is_upcast_terminal(&exp) {
        let exp = rename_exp_bound(ctx, renv, iter_ctx, exp);
        return Ok(exp);
    }
    rename_exp_bind(ctx, renv, iter_ctx, binds, exp)
}

/// Replaces a bound sub-expression by a fresh variable, recording the rename.
fn rename_exp_bound(
    ctx: &mut Context,
    renv: &mut RenameEnv,
    iter_ctx: &mut ICtx,
    exp: ast::Exp,
) -> ast::Exp {
    let typ = phrase!(node: exp.note.as_ref().clone(), span: exp.span.clone());
    let destination = fresh::var_from_typ(&ctx.menv, &ctx.frees, exp.span.clone(), &typ);
    ctx.add_free(destination.id.clone());
    let bounds = exp.free_ids();
    renv.prepend(Rename {
        destination: destination.clone(),
        source: Source::Bound { exp_from: exp },
        iter_ctx: iter_ctx.clone(),
    });
    // The fresh variable takes the sub-expression's place in the iteration
    iter_ctx.filter_bound(|var| !bounds.contains(&var.id));
    iter_ctx.add_var_bound(
        destination.id.clone(),
        destination.typ.clone(),
        destination.iters.clone(),
    );
    var::as_exp(true, &destination)
}

/// Rewrites a binder pattern node by node, injecting guards where needed.
fn rename_exp_bind(
    ctx: &mut Context,
    renv: &mut RenameEnv,
    iter_ctx: &mut ICtx,
    binds: &IdSet,
    exp: ast::Exp,
) -> Result<ast::Exp, AlgoError> {
    let span = exp.span;
    let note = exp.note;
    match exp.node {
        // Upcast: rewrite the inner pattern, then guard the subtype
        ast::ExpKind::UpCast(typ, exp_inner) => {
            let exp_sub = rename_exp(ctx, renv, iter_ctx, binds, *exp_inner)?;
            let exp_from = note_phrase! {
                node: ast::ExpKind::UpCast(typ, Box::new(exp_sub.clone())),
                note: note,
                span: span.clone(),
            };
            let typ_sub = phrase!(node: exp_sub.note.as_ref().clone(), span: span);
            let exp = rename_exp_bind_sub(ctx, renv, iter_ctx, typ_sub, exp_sub, exp_from);
            Ok(exp)
        }
        ast::ExpKind::Tuple(exps) => {
            let exps = rename_exps(ctx, renv, iter_ctx, binds, exps)?;
            let exp = note_phrase!(node: ast::ExpKind::Tuple(exps), note: note, span: span);
            Ok(exp)
        }
        ast::ExpKind::Case(not_exp) => {
            let (mixop, args) = (*not_exp).into_parts();
            let args = rename_exps(ctx, renv, iter_ctx, binds, args)?;
            let not_exp = notation::Mixfix::new(Rc::clone(&mixop), args)
                .expect("arguments obtained from the same mixfix must match its arity");
            let exp_from = note_phrase! {
                node: ast::ExpKind::Case(Box::new(not_exp)),
                note: note.clone(),
                span: span.clone(),
            };
            let typ = phrase!(node: note.as_ref().clone(), span: span.clone());
            // A singleton case needs no match guard
            if is_singleton_case(ctx, &typ)? {
                Ok(exp_from)
            } else {
                let pattern = ast::Pattern::Case(mixop);
                let exp = rename_exp_bind_match(ctx, renv, iter_ctx, pattern, exp_from);
                Ok(exp)
            }
        }
        ast::ExpKind::Str(fields) => {
            let (atoms, exps): (Vec<_>, Vec<_>) = fields
                .into_iter()
                .map(|field| (field.atom, field.exp))
                .unzip();
            let exps = rename_exps(ctx, renv, iter_ctx, binds, exps)?;
            let fields = atoms
                .into_iter()
                .zip(exps)
                .map(|(atom, exp)| ast::ExpField { atom, exp })
                .collect();
            let exp = note_phrase!(node: ast::ExpKind::Str(fields), note: note, span: span);
            Ok(exp)
        }
        ast::ExpKind::Opt(Some(exp_inner)) => {
            let exp_inner = rename_exp(ctx, renv, iter_ctx, binds, *exp_inner)?;
            let exp_from = note_phrase! {
                node: ast::ExpKind::Opt(Some(Box::new(exp_inner))),
                note: note,
                span: span,
            };
            let pattern = ast::Pattern::Opt(ast::OptPattern::Some);
            let exp = rename_exp_bind_match(ctx, renv, iter_ctx, pattern, exp_from);
            Ok(exp)
        }
        ast::ExpKind::Opt(None) => {
            let pattern = ast::Pattern::Opt(ast::OptPattern::None);
            let exp_from = note_phrase!(node: ast::ExpKind::Opt(None), note: note, span: span);
            let exp = rename_exp_bind_match(ctx, renv, iter_ctx, pattern, exp_from);
            Ok(exp)
        }
        ast::ExpKind::List(exps) => {
            let exps = rename_exps(ctx, renv, iter_ctx, binds, exps)?;
            let exps_len = exps.len();
            let exp_from = note_phrase! {
                node: ast::ExpKind::List(exps),
                note: note,
                span: span.clone(),
            };
            // Lists match on their length
            let pattern = if exps_len == 0 {
                ast::ListPattern::Nil
            } else {
                ast::ListPattern::Fixed(exps_len)
            };
            let pattern = ast::Pattern::List(pattern);
            let exp = rename_exp_bind_match(ctx, renv, iter_ctx, pattern, exp_from);
            Ok(exp)
        }
        ast::ExpKind::Cons(exp_head, exp_tail) => {
            let exp_head = rename_exp(ctx, renv, iter_ctx, binds, *exp_head)?;
            let exp_tail = rename_exp(ctx, renv, iter_ctx, binds, *exp_tail)?;
            let exp_from = note_phrase! {
                node: ast::ExpKind::Cons(Box::new(exp_head), Box::new(exp_tail)),
                note: note,
                span: span,
            };
            let pattern = ast::Pattern::List(ast::ListPattern::Cons);
            let exp = rename_exp_bind_match(ctx, renv, iter_ctx, pattern, exp_from);
            Ok(exp)
        }
        ast::ExpKind::Iter(exp_inner, ast::ExpIter { iter, vars }) => {
            // Rewrite under a new iteration scope, keeping its variables
            let iteration = Iteration { iter, vars_bound: vars, vars_bind: vec![] };
            let mut iter_scope = iter_ctx.scope(iteration);
            let exp_inner = rename_exp(ctx, renv, &mut iter_scope, binds, *exp_inner)?;
            let iteration = iter_scope.finish();
            let exp = note_phrase! {
                node: ast::ExpKind::Iter(
                    Box::new(exp_inner),
                    ast::ExpIter { iter: iteration.iter, vars: iteration.vars_bound },
                ),
                note: note,
                span: span,
            };
            Ok(exp)
        }
        // Remaining nodes are leaves
        kind => Ok(note_phrase!(node: kind, note: note, span: span)),
    }
}

/// Rewrites patterns left to right, appending each pattern's renames in order.
pub fn rename_exps(
    ctx: &mut Context,
    renv: &mut RenameEnv,
    iter_ctx: &mut ICtx,
    binds: &IdSet,
    exps: Vec<ast::Exp>,
) -> Result<Vec<ast::Exp>, AlgoError> {
    let mut exps_renamed = Vec::with_capacity(exps.len());
    for exp in exps {
        let mut renv_post = RenameEnv::new();
        let exp = rename_exp(ctx, &mut renv_post, iter_ctx, binds, exp)?;
        renv.append(renv_post);
        exps_renamed.push(exp);
    }
    Ok(exps_renamed)
}

// == Argument rewriting

/// Rewrites an expression argument pattern; function arguments are unchanged.
fn rename_arg(
    ctx: &mut Context,
    renv: &mut RenameEnv,
    iter_ctx: &mut ICtx,
    binds: &IdSet,
    arg: ast::Arg,
) -> Result<ast::Arg, AlgoError> {
    let ast::ArgKind::Exp(exp) = arg.node else {
        return Ok(arg);
    };
    let mut renv_post = RenameEnv::new();
    let exp = rename_exp(ctx, &mut renv_post, iter_ctx, binds, *exp)?;
    renv.append(renv_post);
    let arg = phrase!(node: ast::ArgKind::Exp(Box::new(exp)), span: arg.span);
    Ok(arg)
}

pub fn rename_args(
    ctx: &mut Context,
    renv: &mut RenameEnv,
    iter_ctx: &mut ICtx,
    binds: &IdSet,
    args: Vec<ast::Arg>,
) -> Result<Vec<ast::Arg>, AlgoError> {
    let mut args_renamed = Vec::with_capacity(args.len());
    for arg in args {
        let arg = rename_arg(ctx, renv, iter_ctx, binds, arg)?;
        args_renamed.push(arg);
    }
    Ok(args_renamed)
}
