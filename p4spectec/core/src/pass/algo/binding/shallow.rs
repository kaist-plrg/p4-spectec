//! Checks for shallow binding patterns
//!
//! Table rows select variables or cases, optionally through an upcast.
//! Case payloads must be irrefutable:
//! variables, products, singleton constructors, or iterations of these.
//! `check_args` rejects invalid shapes and repeated new binders.
//! Failures retain the responsible syntax for caller-specific diagnostics.

use crate::lang::common::ds::map::IdMap;

use crate::lang::il::ast;

use crate::runtime::{
    envs::algo::{TDEnv, VEnv},
    ops::typ::{TypeError, has_single_constructor},
};

use crate::phrase;

/// A shallow binding failure retaining its original syntax.
pub enum ShallowFailure<'a> {
    /// An argument outside the shallow pattern language.
    ShapeInvalid(&'a ast::Arg),
    /// A new binder repeated after its previous occurrence.
    BindingRepeated { id: &'a ast::Id, id_previous: &'a ast::Id },
    /// A type operation required by payload validation failed.
    TypeInvalid(TypeError),
}

/// Checks whether a payload pattern accepts every value of its annotated type.
fn is_irrefutable_exp(tdenv: &TDEnv, exp: &ast::Exp) -> Result<bool, TypeError> {
    match &exp.node {
        // Fresh variables accept the entire payload domain
        ast::ExpKind::Id(_) => Ok(true),
        // Iterations preserve the element pattern's coverage
        ast::ExpKind::Iter(exp, _) => is_irrefutable_exp(tdenv, exp),
        // Products cover their type when every component does
        ast::ExpKind::Tuple(exps) => is_irrefutable_exps(tdenv, exps),
        ast::ExpKind::Str(fields) => {
            // Fields decompose a record without selecting a variant
            for field in fields {
                if !is_irrefutable_exp(tdenv, &field.exp)? {
                    return Ok(false);
                }
            }
            Ok(true)
        }
        ast::ExpKind::Case(not_exp) => {
            // A nested case must be the only constructor of its type
            let typ = phrase!(node: exp.note.as_ref().clone(), span: exp.span);
            if !has_single_constructor(tdenv, &typ)? {
                return Ok(false);
            }
            is_irrefutable_exps(tdenv, not_exp.args())
        }
        // Literals, list/option cases, and subtype restrictions narrow a payload
        _ => Ok(false),
    }
}

/// Checks every component of a product or constructor payload.
fn is_irrefutable_exps(tdenv: &TDEnv, exps: &[ast::Exp]) -> Result<bool, TypeError> {
    for exp in exps {
        if !is_irrefutable_exp(tdenv, exp)? {
            return Ok(false);
        }
    }
    Ok(true)
}

// Expressions

/// Checks whether an expression is a shallow binder.
pub fn check_exp(tdenv: &TDEnv, exp: &ast::Exp) -> Result<bool, TypeError> {
    match &exp.node {
        // A row variable covers every constructor of its type
        ast::ExpKind::Id(_) => Ok(true),
        ast::ExpKind::UpCast(_, exp) => {
            // Upcasts select a source domain but must still validate its payloads
            if matches!(&exp.node, ast::ExpKind::Id(_) | ast::ExpKind::Case(_)) {
                check_exp(tdenv, exp)
            } else {
                Ok(false)
            }
        }
        // Selecting a case must cover all values carried by that case
        ast::ExpKind::Case(not_exp) => is_irrefutable_exps(tdenv, not_exp.args()),
        // Other top-level forms have no constructor-set interpretation
        _ => Ok(false),
    }
}

// Arguments

/// Checks whether an argument is a shallow binder; function arguments are not.
pub fn check_arg(tdenv: &TDEnv, arg: &ast::Arg) -> Result<bool, TypeError> {
    match &arg.node {
        ast::ArgKind::Exp(exp) => check_exp(tdenv, exp),
        ast::ArgKind::Def(_) => Ok(false),
    }
}

/// Validates shallow arguments and rejects repeated new binders.
pub fn check_args<'a>(
    tdenv: &TDEnv,
    venv: &VEnv,
    args: &'a [ast::Arg],
) -> Result<(), ShallowFailure<'a>> {
    // Reject the first invalid shape before traversing its binders
    for arg in args {
        if !check_arg(tdenv, arg).map_err(ShallowFailure::TypeInvalid)? {
            return Err(ShallowFailure::ShapeInvalid(arg));
        }
    }
    // Reject repeated new binders in occurrence order, before renaming
    let mut ids_seen = IdMap::new();
    for arg in args {
        let ast::ArgKind::Exp(exp) = &arg.node else {
            unreachable!("shallow validation rejects function arguments");
        };
        check_repeated_binding(venv, &mut ids_seen, exp)?;
    }

    Ok(())
}

/// Rejects the first repeated new binder in a validated shallow pattern.
fn check_repeated_binding<'a>(
    venv: &VEnv,
    ids_seen: &mut IdMap<&'a ast::Id>,
    exp: &'a ast::Exp,
) -> Result<(), ShallowFailure<'a>> {
    match &exp.node {
        ast::ExpKind::Id(id) => {
            // Previously bound names do not introduce new binders
            if venv.contains_key(id) {
                return Ok(());
            }
            // Keep the first source occurrence even when names sort differently
            if let Some(id_previous) = ids_seen.get(id) {
                return Err(ShallowFailure::BindingRepeated { id, id_previous });
            }
            ids_seen.insert(id.clone(), id);
        }
        ast::ExpKind::UpCast(_, exp) | ast::ExpKind::Iter(exp, _) => {
            // Casts and dimensions retain the underlying binding positions
            check_repeated_binding(venv, ids_seen, exp)?;
        }
        ast::ExpKind::Case(not_exp) => {
            // Visit case arguments from left to right
            for exp in not_exp.args() {
                check_repeated_binding(venv, ids_seen, exp)?;
            }
        }
        ast::ExpKind::Tuple(exps) | ast::ExpKind::List(exps) => {
            // Upcast cases can contain invertible compound arguments
            for exp in exps {
                check_repeated_binding(venv, ids_seen, exp)?;
            }
        }
        ast::ExpKind::Str(exp_fields) => {
            // Preserve field occurrence order beneath an upcast case
            for ast::ExpField { exp, .. } in exp_fields {
                check_repeated_binding(venv, ids_seen, exp)?;
            }
        }
        ast::ExpKind::Opt(Some(exp)) => {
            // An option contributes the binders of its payload
            check_repeated_binding(venv, ids_seen, exp)?;
        }
        ast::ExpKind::Cons(exp_l, exp_r) => {
            // A cons pattern binds its head before its tail
            check_repeated_binding(venv, ids_seen, exp_l)?;
            check_repeated_binding(venv, ids_seen, exp_r)?;
        }
        _ => {
            // Literals bind nothing; collection rejects non-invertible binders
        }
    }
    Ok(())
}
