//! Registration of parser-visible names from completed P4 parse-tree values
//!
//! P4 token classification depends on declarations
//! reduced earlier in the grammar.
//! These helpers extract names and referenced types from completed values,
//! then update the active parser scope.
//! List-shaped declarations are traversed left to right
//! so every name is available to following tokens.

use crate::lang::data::value::{Value, get};

use super::{
    context::{Context, Namespace, TypeId},
    extract,
};

// == Individual names

/// Declares a type name from a `name` value.
pub(super) fn typ(ctx: &Context, value: &Value, has_params: bool) {
    let id = extract::id_name(&ctx.arena(), value).expect("P4 declaration name");
    ctx.declare_typ(id, has_params).expect("P4 parser scope");
}

/// Declares a variable from a `name` value,
/// recording its type for member lookup.
pub(super) fn var(ctx: &Context, value: &Value, has_params: bool, type_ref: Option<&Value>) {
    let id = extract::id_name(&ctx.arena(), value).expect("P4 declaration name");
    let type_id = match type_ref {
        Some(type_ref) => {
            extract::type_id_type_ref(&ctx.arena(), type_ref).expect("P4 type reference")
        }
        None => TypeId::Empty,
    };
    ctx.declare_var(id, has_params, type_id)
        .expect("P4 parser scope");
}

// == Name lists

/// Declares every name of a `nameList`, left to right.
pub(super) fn vars(ctx: &Context, value: &Value) {
    get::matches! { &ctx.arena(),
        value,
        "nameList ',' name" => |values| {
            vars(ctx, values[0]);
            var(ctx, values[1], false, None);
        },
        _ => var(ctx, value, false, None),
    }
}

/// Declares every type parameter of a `typeParameterList`, left to right.
pub(super) fn typs(ctx: &Context, value: &Value) {
    get::matches! { &ctx.arena(),
        value,
        "typeParameterList ',' typeParameter" => |values| {
            typs(ctx, values[0]);
            typ(ctx, values[1], false);
        },
        _ => typ(ctx, value, false),
    }
}

// == Type namespaces

/// Attaches the members declared inside a type to its type name.
pub(super) fn type_namespace(ctx: &Context, value: &Value, namespace: Namespace) {
    let id = extract::id_name(&ctx.arena(), value).expect("P4 type name");
    ctx.namespace_set_typ(&id, namespace);
}
