//! AL adapters for shared execution contexts
//!
//! `Global::load` prepares AL definitions into the shared environments.
//! `Context` retains the shared scope, binding, and iteration operations;
//! `FuncSignature` extracts types from prepared AL function definitions.

use crate::{
    interp::shared::{
        context::{self as shared, FuncSignature},
        error::Error,
    },
    lang::{
        al::ast as source,
        data::typ::{FuncTyp, make},
    },
    runtime::{
        envs::interp::{al::ast_prepared as ast, shared::callable::Callable},
        typdef::TypeDef,
    },
};

// = Context aliases

pub use shared::Scope;

/// Stores loaded AL definitions and prepared callables.
pub type Global = shared::Global<ast::RelDef, ast::MetaFuncDef>;

/// Holds local AL bindings over borrowed global definitions.
pub type Context<'global> = shared::Context<'global, ast::RelDef, ast::MetaFuncDef>;

// = Loading

impl Global {
    /// Loads a specification and prepares its callables for slot execution.
    ///
    /// Requires executable IR satisfying the preceding passes' invariants,
    /// including unique global type definitions.
    /// Direct IR constructors must uphold the same precondition.
    pub fn load(spec: source::Spec) -> Result<Self, Error> {
        let mut loaded = Self::new();
        // Prepare definitions before inserting them into their namespaces
        for def in spec {
            match def.node {
                source::DefKind::Typ(typdef) => {
                    // Types keep their definition body
                    let (id, typdef) = match typdef {
                        ast::TypDef::Extern(typdef) => (typdef.id, TypeDef::Extern),
                        ast::TypDef::Defined(typdef) => {
                            let ast::DefinedTyp { id, tparams, def_typ, .. } = *typdef;
                            (id, TypeDef::Defined(tparams, Box::new(def_typ)))
                        }
                    };
                    loaded.insert_typdef(id, typdef);
                }
                // Meta-variables carry no runtime state
                source::DefKind::Var(_) => {}
                source::DefKind::Rel(rel) => {
                    // Relations are prepared into callables with a frame layout
                    let rel = Callable::prepare(rel);
                    let id = match &rel.def {
                        ast::RelDef::Extern(rel) => &rel.id,
                        ast::RelDef::Defined(rel) => &rel.id,
                    };
                    loaded.insert_rel(id.clone(), rel)?;
                }
                source::DefKind::MetaFunc(func) => {
                    // Prepare functions before sharing them with local bindings
                    let func = Callable::prepare(func);
                    let id = match &func.def {
                        ast::MetaFuncDef::Extern(func) => &func.id,
                        ast::MetaFuncDef::Builtin(func) => &func.id,
                        ast::MetaFuncDef::Table(func) => &func.id,
                        ast::MetaFuncDef::Defined(func) => &func.id,
                    };
                    loaded.insert_func(id.clone(), func)?;
                }
            }
        }
        Ok(loaded)
    }
}

// = Function signatures

impl FuncSignature for ast::MetaFuncDef {
    fn func_typ(&self) -> FuncTyp {
        fn param_typ(param: &ast::Param) -> ast::Typ {
            match &param.node {
                ast::ParamKind::Exp(typ) => typ.clone(),
                // A function parameter has a function type
                ast::ParamKind::Def(_, tparams, params, typ) => {
                    make::func(tparams.clone(), params.iter().map(param_typ).collect(), typ.clone())
                }
            }
        }
        // Read the signature without inspecting the callable body
        let (tparams, params, typ): (&[ast::TParam], &[ast::Param], &ast::Typ) = match self {
            ast::MetaFuncDef::Extern(func) => (&func.tparams, &func.params, &func.typ),
            ast::MetaFuncDef::Builtin(func) => (&func.tparams, &func.params, &func.typ),
            // Table functions have no type parameters
            ast::MetaFuncDef::Table(func) => (&[], &func.params, &func.typ),
            ast::MetaFuncDef::Defined(func) => (&func.tparams, &func.params, &func.typ),
        };
        // Preserve nested function parameters in the resulting type
        FuncTyp {
            tparams: tparams.to_vec(),
            typs_params: params.iter().map(param_typ).collect(),
            typ_ret: Box::new(typ.clone()),
        }
    }
}
