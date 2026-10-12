//! Iteration state surrounding a binding operation
//!
//! `ICtx` is the stack of iterations enclosing the expression under analysis,
//! innermost first.
//! Each `Iteration` accumulates the variables that supply its values (`vars_bound`)
//! and the variables it binds (`vars_bind`);
//! `iterate_prem` finally wraps a premise in one `IterPrem` per level.

use std::ops::{Deref, DerefMut};

use crate::lang::common::{Id, source::Span};

use crate::lang::il::ast;

use crate::lang::al;

use crate::runtime::{dim::Dim, envs::algo::VEnv};

use crate::phrase;

use super::super::{AlgoError, error};

/// One enclosing iteration with its source variables and new bindings.
#[derive(Clone, Debug, PartialEq)]
pub struct Iteration {
    pub iter: ast::Iter,
    /// Variables bound outside that supply this iteration's values.
    pub vars_bound: Vec<ast::Var>,
    /// Variables this iteration binds.
    pub vars_bind: Vec<ast::Var>,
}

/// Enclosing iterations, innermost first.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct ICtx(Vec<Iteration>);

/// An iteration scope that rolls back its context changes when dropped.
pub struct IterationScope<'a> {
    iter_ctx: &'a mut ICtx,
    original: Option<ICtx>,
}

impl Deref for IterationScope<'_> {
    type Target = ICtx;

    fn deref(&self) -> &Self::Target {
        self.iter_ctx
    }
}

impl DerefMut for IterationScope<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.iter_ctx
    }
}

impl Drop for IterationScope<'_> {
    fn drop(&mut self) {
        // Restore the context unless `finish` committed the scope
        if let Some(original) = self.original.take() {
            *self.iter_ctx = original;
        }
    }
}

impl IterationScope<'_> {
    /// Commits the scope, removing and returning the innermost iteration.
    pub fn finish(mut self) -> Iteration {
        let iteration = self.iter_ctx.0.remove(0);
        self.original = None;
        iteration
    }
}

impl ICtx {
    // == Constructors

    pub fn new() -> Self {
        Self::default()
    }

    pub fn from_iterations(iterations: Vec<Iteration>) -> Self {
        Self(iterations)
    }

    pub fn as_slice(&self) -> &[Iteration] {
        &self.0
    }

    pub fn iters(&self) -> Vec<ast::Iter> {
        self.0.iter().map(|entry| entry.iter).collect()
    }

    // == Transactions

    /// Pushes an iteration; the returned scope rolls it back unless finished.
    pub fn scope(&mut self, iteration: Iteration) -> IterationScope<'_> {
        let original = self.clone();
        self.0.insert(0, iteration);
        IterationScope { iter_ctx: self, original: Some(original) }
    }

    // == Adders

    /// Adds one iteration to every dimension.
    fn add_iter(venv: VEnv, iter: ast::Iter) -> VEnv {
        venv.iter()
            .map(|(id, dim)| (id.clone(), dim.clone().add_iter(iter)))
            .collect()
    }

    /// Registers source variables at every level, one iteration per level.
    pub fn add_vars_bound(&mut self, mut venv: VEnv) {
        for entry in &mut self.0 {
            entry
                .vars_bound
                .extend(venv.iter().map(|(id, dim)| ast::Var {
                    id: id.clone(),
                    typ: dim.typ.clone(),
                    iters: dim.iters.clone(),
                }));
            // Each outer level sees the variables under one more iteration
            venv = Self::add_iter(venv, entry.iter);
        }
    }

    pub fn add_var_bound(&mut self, id: Id, typ: ast::Typ, iters: Vec<ast::Iter>) {
        let mut venv = VEnv::new();
        venv.insert(id, Dim::new(typ, iters));
        self.add_vars_bound(venv);
    }

    /// Registers binding variables at every level, iterating once per level.
    pub fn add_vars_bind(&mut self, mut venv: VEnv) {
        for entry in &mut self.0 {
            entry
                .vars_bind
                .extend(venv.iter().map(|(id, dim)| ast::Var {
                    id: id.clone(),
                    typ: dim.typ.clone(),
                    iters: dim.iters.clone(),
                }));
            // Each outer level sees the variables under one more iteration
            venv = Self::add_iter(venv, entry.iter);
        }
    }

    // == Filtering

    /// Keeps only source variables that satisfy the predicate.
    pub fn filter_bound(&mut self, mut predicate: impl FnMut(&ast::Var) -> bool) {
        for entry in &mut self.0 {
            entry.vars_bound.retain(&mut predicate);
        }
    }

    // == Validation

    /// Rejects iterations with no previously bound source variable.
    pub fn validate(&self, span: Span) -> Result<(), AlgoError> {
        for entry in &self.0 {
            if entry.vars_bound.is_empty() {
                // Binding without an iteration source has no determinable length
                return Err(error::binding::iteration_loop_variable_missing(
                    &span,
                    &entry.vars_bind,
                ));
            }
        }
        Ok(())
    }

    // == Iteration premises

    /// Wraps a premise in one iteration premise per level, innermost first.
    pub fn iterate_prem(&self, mut prem: al::ast::Prem) -> al::ast::Prem {
        // Wrap innermost first
        for entry in &self.0 {
            let span = prem.span;
            let prem_iter = al::ast::PremIter {
                iter: entry.iter,
                vars_bound: entry.vars_bound.clone(),
                vars_bind: entry.vars_bind.clone(),
            };
            let iter_prem = al::ast::IterPrem { prem: Box::new(prem), prem_iter };
            prem = phrase!(node: al::ast::PremKind::Iter(iter_prem), span: span);
        }
        prem
    }
}
