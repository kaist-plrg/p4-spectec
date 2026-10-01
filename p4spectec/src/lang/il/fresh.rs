//! Fresh internal-language variables and expressions
//!
//! A fresh variable is named after its type (`nat`, `expr*`),
//! or after a meta-variable that aliases the type when exactly one does,
//! then primed until it clashes with nothing in scope.

use crate::lang::{
    common::{
        ds::{map::IdMap, set::IdSet},
        source::Span,
    },
    traits::{eq::SyntaxEq, print::Print},
};

use super::ast::*;

/// Meta-variables in scope, by name.
type Metavars = IdMap<Typ>;

/// Primes `id` until it differs from every same-based identifier in `ids`.
pub(crate) fn id(ids: &IdSet, id: &Id) -> Id {
    // Only identifiers sharing the base name can clash
    let base = id.strip_suffix().node;
    let ids = ids
        .iter()
        .filter(|id_other| id_other.strip_suffix().node == base)
        .cloned()
        .collect::<IdSet>();
    let mut fresh = id.clone();
    // Add primes until free
    while ids.contains(&fresh) {
        fresh.node.push('\'');
    }
    fresh
}

/// The unique meta-variable aliasing `typ`, if there is exactly one.
fn find_alias(metavars: &Metavars, span: &Span, typ: &Typ) -> Option<Var> {
    let typ_name = Print::to_string(typ);
    // An alias must have the type and not just be named after it
    let mut matching = metavars.iter().filter(|(id_alias, typ_alias)| {
        typ.syntax_eq(typ_alias) && typ_name.as_str() != id_alias.node.as_str()
    });
    let (id_alias, typ_alias) = matching.next()?;
    // Two aliases would make the name ambiguous
    if matching.next().is_some() {
        return None;
    }
    Some(Var {
        id: crate::phrase! {
            node: id_alias.node.clone(),
            span: span.clone(),
        },
        typ: typ_alias.clone(),
        iters: vec![],
    })
}

/// Names a variable for `typ`: an alias if any, else the type's own text,
/// peeling off iterations.
fn var_from_typ_inner(metavars: &Metavars, span: &Span, typ: &Typ) -> Var {
    // An alias names the whole type, iterations included
    if let Some(alias) = find_alias(metavars, span, typ) {
        return alias;
    }
    match &typ.node {
        // An iterated type names its element and records the iteration
        TypKind::Iter(inner, iter) => {
            let mut var = var_from_typ_inner(metavars, span, inner);
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

/// Constructs a fresh variable for `typ`.
pub fn var_from_typ(metavars: &Metavars, ids: &IdSet, span: Span, typ: &Typ) -> Var {
    let mut var = var_from_typ_inner(metavars, &span, typ);
    var.id = id(ids, &var.id);
    var
}

/// Constructs a fresh wildcard variable for `typ`.
pub fn var_from_typ_wildcard(metavars: &Metavars, ids: &IdSet, span: Span, typ: &Typ) -> Var {
    let mut var = var_from_typ_inner(metavars, &span, typ);
    // Wildcards are marked with a leading underscore
    var.id.node.insert(0, '_');
    var.id = id(ids, &var.id);
    var
}
