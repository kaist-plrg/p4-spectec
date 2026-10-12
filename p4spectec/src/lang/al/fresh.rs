//! Fresh algorithmic-language expressions
//!
//! With a single alias `flag : bool`,
//! `bool` becomes `flag` (or `flag'` if `flag` is already used),
//! and `bool?` adds an optional iteration around it.
//! The name is then primed until it clashes with nothing in scope.

use crate::lang::{
    common::{ds::set::IdSet, source::Span},
    traits::{eq::SyntaxEq, print::Print},
};

use crate::lang::il;

use crate::runtime::envs::algo::MEnv;

use super::{ast::*, var};

// == Expressions

/// Constructs a fresh variable expression for `typ`.
pub fn exp_from_typ(menv: &MEnv, is_dim: bool, ids: &IdSet, typ: &Typ) -> (IdSet, Exp) {
    // Name from an alias or the type, then make it unique
    let mut var = var_from_typ(menv, &typ.span, typ);
    var.id = il::fresh::id(ids, &var.id);
    // The caller continues with the new name taken
    let mut ids_fresh = ids.clone();
    ids_fresh.insert(var.id.clone());
    let exp = var::as_exp(is_dim, &var);
    (ids_fresh, exp)
}

// == Variables

/// Names a variable for `typ`: its unique alias if any, else its own text.
fn var_from_typ(menv: &MEnv, span: &Span, typ: &Typ) -> Var {
    let typ_name = Print::to_string(typ);
    // An alias must have the type and not just be named after it
    let mut vars_alias = menv.iter().filter(|(id_alias, typ_alias)| {
        typ.syntax_eq(typ_alias) && typ_name.as_str() != id_alias.node.as_str()
    });
    // Exactly one alias names the whole type
    if let (Some((id_alias, typ_alias)), None) = (vars_alias.next(), vars_alias.next()) {
        let id = crate::phrase! { node: id_alias.node.clone(), span: span.clone() };
        return Var { id, typ: typ_alias.clone(), iters: vec![] };
    }

    match &typ.node {
        // An iterated type names its element and records the iteration
        TypKind::Iter(typ_inner, iter) => {
            let mut var = var_from_typ(menv, span, typ_inner);
            var.iters.push(*iter);
            var
        }
        // Otherwise the printed type is the name
        _ => Var {
            id: crate::phrase! {
                node: Print::to_string(typ),
                span: span.clone(),
            },
            typ: typ.clone(),
            iters: vec![],
        },
    }
}
