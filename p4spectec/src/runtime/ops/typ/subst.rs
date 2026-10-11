//! Runtime substitution for internal-language type variables
//!
//! Function-type binders are freshened before applying the outer substitution,
//! so a substituted type cannot capture a bound type parameter.
//! Substituting a type variable that has arguments is rejected as higher-order.

use crate::lang::{
    common::{
        ds::map::{ArityMismatch, IdMap},
        source::Phrase,
    },
    data::notation::Mixfix,
};

use crate::lang::il::ast::{self, TypKind};

use crate::phrase;

use super::{Fresh, TypeError, TypeErrorKind};

/// A substitution from type parameters to types.
#[derive(Default)]
pub struct Theta(IdMap<ast::Typ>);

impl Theta {
    pub fn new() -> Self {
        Self(IdMap::new())
    }

    /// Pairs parameters with arguments; the counts must match.
    pub fn from_lists(tparams: &[ast::TParam], targs: &[ast::Typ]) -> Result<Self, ArityMismatch> {
        let theta = IdMap::from_lists(tparams, targs)?;
        Ok(Self(theta))
    }

    pub fn insert(&mut self, tparam: ast::TParam, targ: ast::Typ) -> Option<ast::Typ> {
        self.0.insert(tparam, targ)
    }

    pub fn get(&self, tparam: &ast::TParam) -> Option<&ast::Typ> {
        self.0.get(tparam)
    }
}

/// Renames binders to fresh variables, returning both the map and the binders.
fn freshen_tparams(fresh: &mut Fresh, tparams: &[ast::TParam]) -> (Theta, Vec<ast::TParam>) {
    let mut theta = Theta::new();
    let mut tparams_fresh = Vec::with_capacity(tparams.len());
    for tparam in tparams {
        let (tparam_fresh, typ_fresh) = fresh.fresh();
        theta.insert(tparam.clone(), typ_fresh);
        tparams_fresh.push(tparam_fresh);
    }
    (theta, tparams_fresh)
}

// == Types

/// Substitutes type variables while freshening nested function binders.
pub fn subst_typ<'env>(
    find_subst: &dyn Fn(&ast::Id) -> Option<&'env ast::Typ>,
    typ: &ast::Typ,
) -> Result<ast::Typ, TypeError> {
    let mut fresh = Fresh::default();
    subst_typ_inner(&mut fresh, find_subst, typ)
}

/// Substitution sharing the fresh counter of an enclosing operation.
pub(crate) fn subst_typ_inner<'env>(
    fresh: &mut Fresh,
    find_subst: &dyn Fn(&ast::Id) -> Option<&'env ast::Typ>,
    typ: &ast::Typ,
) -> Result<ast::Typ, TypeError> {
    let typ_kind = match &typ.node {
        // Primitives have no variables
        TypKind::Bool | TypKind::Num(_) | TypKind::Text => return Ok(typ.clone()),
        // A variable is replaced when substituted, else its arguments recurse
        TypKind::Var(id, targs) => match find_subst(id) {
            // A substituted variable cannot take arguments
            Some(_) if !targs.is_empty() => {
                return Err(TypeError::new(
                    TypeErrorKind::HigherOrderSubstitutionUnsupported,
                    typ.span.clone(),
                ));
            }
            // Replace the whole variable
            Some(typ_subst) => return Ok(typ_subst.clone()),
            // Not substituted: recurse into the arguments
            None => TypKind::Var(id.clone(), subst_typs_inner(fresh, find_subst, targs)?),
        },
        // Componentwise
        TypKind::Tuple(typs) => TypKind::Tuple(subst_typs_inner(fresh, find_subst, typs)?),
        // Into the element type
        TypKind::Iter(typ_inner, iter) => {
            let typ_inner = subst_typ_inner(fresh, find_subst, typ_inner)?;
            TypKind::Iter(Box::new(typ_inner), *iter)
        }
        // Freshen the binders first, then substitute under them
        TypKind::Func(func_typ) => {
            let (theta_fresh, tparams) = freshen_tparams(fresh, &func_typ.tparams);
            let find_fresh = |id: &ast::Id| theta_fresh.get(id);
            // Rename to fresh binders, then apply the outer substitution
            let typs_params = subst_typs_inner(fresh, &find_fresh, &func_typ.typs_params)?;
            let typs_params = subst_typs_inner(fresh, find_subst, &typs_params)?;
            let typ_ret = subst_typ_inner(fresh, &find_fresh, &func_typ.typ_ret)?;
            let typ_ret = subst_typ_inner(fresh, find_subst, &typ_ret)?;
            TypKind::Func(ast::FuncTyp { tparams, typs_params, typ_ret: Box::new(typ_ret) })
        }
    };
    Ok(phrase!(node: typ_kind, span: typ.span.clone()))
}

/// Substitutes type variables in a type list.
pub fn subst_typs<'env>(
    find_subst: &dyn Fn(&ast::Id) -> Option<&'env ast::Typ>,
    typs: &[ast::Typ],
) -> Result<Vec<ast::Typ>, TypeError> {
    let mut fresh = Fresh::default();
    subst_typs_inner(&mut fresh, find_subst, typs)
}

/// List substitution sharing the fresh counter.
pub(crate) fn subst_typs_inner<'env>(
    fresh: &mut Fresh,
    find_subst: &dyn Fn(&ast::Id) -> Option<&'env ast::Typ>,
    typs: &[ast::Typ],
) -> Result<Vec<ast::Typ>, TypeError> {
    typs.iter()
        .map(|typ| subst_typ_inner(fresh, find_subst, typ))
        .collect()
}

// == Notation types

/// Substitutes type variables in a notation type.
pub fn subst_not_typ<'env, M: Clone>(
    find_subst: &dyn Fn(&ast::Id) -> Option<&'env ast::Typ>,
    not_typ: &Phrase<Mixfix<M, ast::Typ>>,
) -> Result<Phrase<Mixfix<M, ast::Typ>>, TypeError> {
    let mut fresh = Fresh::default();
    subst_not_typ_inner(&mut fresh, find_subst, not_typ)
}

/// Substitutes the arguments, keeping the shared mixop.
pub(crate) fn subst_not_typ_inner<'env, M: Clone>(
    fresh: &mut Fresh,
    find_subst: &dyn Fn(&ast::Id) -> Option<&'env ast::Typ>,
    not_typ: &Phrase<Mixfix<M, ast::Typ>>,
) -> Result<Phrase<Mixfix<M, ast::Typ>>, TypeError> {
    let not_typ_kind = not_typ
        .node
        .try_map(|typ| subst_typ_inner(fresh, find_subst, typ))?;
    Ok(phrase!(node: not_typ_kind, span: not_typ.span.clone()))
}

// == Parameters

/// Substitutes a parameter's type, freshening a function parameter's binders.
fn subst_param_inner<'env>(
    fresh: &mut Fresh,
    find_subst: &dyn Fn(&ast::Id) -> Option<&'env ast::Typ>,
    param: &ast::Param,
) -> Result<ast::Param, TypeError> {
    let kind = match &param.node {
        // A value parameter: its type
        ast::ParamKind::Exp(typ) => ast::ParamKind::Exp(subst_typ_inner(fresh, find_subst, typ)?),
        // A function parameter: freshen its binders, then substitute under them
        ast::ParamKind::Def(id, tparams, params, typ) => {
            let (theta_fresh, tparams) = freshen_tparams(fresh, tparams);
            let find_fresh = |id: &ast::Id| theta_fresh.get(id);
            let params = subst_params_inner(fresh, &find_fresh, params)?;
            let params = subst_params_inner(fresh, find_subst, &params)?;
            let typ = subst_typ_inner(fresh, &find_fresh, typ)?;
            let typ = subst_typ_inner(fresh, find_subst, &typ)?;
            ast::ParamKind::Def(id.clone(), tparams, params, typ)
        }
    };
    Ok(crate::phrase! {
        node: kind,
        span: param.span.clone(),
    })
}

/// Substitutes type variables in parameters while sharing fresh state.
pub(crate) fn subst_params<'env>(
    find_subst: &dyn Fn(&ast::Id) -> Option<&'env ast::Typ>,
    params: &[ast::Param],
) -> Result<Vec<ast::Param>, TypeError> {
    let mut fresh = Fresh::default();
    subst_params_inner(&mut fresh, find_subst, params)
}

/// Parameter-list substitution sharing the fresh counter.
fn subst_params_inner<'env>(
    fresh: &mut Fresh,
    find_subst: &dyn Fn(&ast::Id) -> Option<&'env ast::Typ>,
    params: &[ast::Param],
) -> Result<Vec<ast::Param>, TypeError> {
    params
        .iter()
        .map(|param| subst_param_inner(fresh, find_subst, param))
        .collect()
}
