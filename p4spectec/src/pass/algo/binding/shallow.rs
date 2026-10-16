//! Checks for shallow binding patterns
//!
//! Shallow binders are variables,
//! upcasts of variables or cases,
//! and cases whose arguments are variables under any number of iterations.
//! `check_args` rejects invalid shapes and repeated new binders.
//! Failures retain the responsible syntax for caller-specific diagnostics.

use crate::lang::common::ds::map::IdMap;

use crate::lang::il::ast;

use crate::runtime::envs::algo::VEnv;

/// A shallow binding failure retaining its original syntax.
pub enum ShallowFailure<'a> {
    /// An argument outside the shallow pattern language.
    ShapeInvalid(&'a ast::Arg),
    /// A new binder repeated after its previous occurrence.
    BindingRepeated { id: &'a ast::Id, id_previous: &'a ast::Id },
}

/// Checks for a variable under any number of iterations.
fn is_iterated_id_exp(exp: &ast::Exp) -> bool {
    match &exp.node {
        ast::ExpKind::Id(_) => true,
        ast::ExpKind::Iter(exp, _) => is_iterated_id_exp(exp),
        _ => false,
    }
}

// Expressions

/// Checks whether an expression is a shallow binder.
pub fn check_exp(exp: &ast::Exp) -> bool {
    match &exp.node {
        ast::ExpKind::Id(_) => true,
        ast::ExpKind::UpCast(_, exp) => {
            matches!(&exp.node, ast::ExpKind::Id(_) | ast::ExpKind::Case(_))
        }
        ast::ExpKind::Case(not_exp) => not_exp.args().iter().all(is_iterated_id_exp),
        _ => false,
    }
}

// Arguments

/// Checks whether an argument is a shallow binder; function arguments are not.
pub fn check_arg(arg: &ast::Arg) -> bool {
    match &arg.node {
        ast::ArgKind::Exp(exp) => check_exp(exp),
        ast::ArgKind::Def(_) => false,
    }
}

/// Validates shallow arguments and rejects repeated new binders.
pub fn check_args<'a>(venv: &VEnv, args: &'a [ast::Arg]) -> Result<(), ShallowFailure<'a>> {
    // Reject the first invalid shape before traversing its binders
    for arg in args {
        if !check_arg(arg) {
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
