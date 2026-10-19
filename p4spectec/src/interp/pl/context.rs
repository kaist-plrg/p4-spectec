//! PL adapters for shared execution contexts
//!
//! `SpecEnv::load` prepares PL definitions into the shared environments.
//! `Context` retains the shared scope, binding, and iteration operations;
//! `FuncSignature` extracts types from prepared PL function definitions.

use crate::lang::data::{
    frame::FrameLayout,
    notation::MixopArena,
    typ::{FuncTyp, make},
};

use crate::lang::pl::{ast as source, prepared as ast};

use crate::runtime::envs::{elab::TDEnv, interp::shared::TypeDef};

use crate::interp::shared::{
    context::{self as shared, FuncSignature},
    error::Error,
    prepare::Prepare,
};

use super::prepare::PrepareContext;

// = Context aliases

/// Stores loaded PL definitions, prepared.
pub type SpecEnv = shared::Global<ast::RelDef, ast::MetaFuncDef>;

/// Holds local PL bindings over borrowed global definitions.
pub type Context<'global> = shared::Context<'global, ast::RelDef, ast::MetaFuncDef>;

// = Loading

impl SpecEnv {
    /// Loads type definitions and prepares each callable for slot execution.
    ///
    /// Panics if a global definition is repeated.
    pub fn load(arena_mixop: &mut MixopArena, spec: source::Spec) -> Result<Self, Error> {
        let mut loaded = Self::new();
        let tdenv = loaded.load_typdefs(arena_mixop, &spec);
        loaded.load_callables(arena_mixop, &tdenv, spec);
        Ok(loaded)
    }

    /// Inserts prepared type definitions and returns their source forms.
    fn load_typdefs(&mut self, arena_mixop: &mut MixopArena, spec: &source::Spec) -> TDEnv {
        let mut tdenv = TDEnv::new();
        for def in spec {
            // Only types load in this pass
            let source::DefKind::Typ(typdef) = &def.node.node else {
                continue;
            };
            // Keep the source body for subtype checks
            let (id, typdef_source, typdef) = match typdef {
                source::TypDef::Extern(typdef) => {
                    (typdef.id.clone(), TypeDef::Extern, TypeDef::Extern)
                }
                source::TypDef::Defined(typdef) => {
                    let source::DefinedTyp { id, tparams, def_typ } = typdef.as_ref();
                    let typdef_source =
                        TypeDef::Defined(tparams.clone(), Box::new(def_typ.clone()));
                    let def_typ = def_typ.clone().prepare(arena_mixop);
                    let def_typ = Box::new(def_typ);
                    let typdef = TypeDef::Defined(tparams.clone(), def_typ);
                    (id.clone(), typdef_source, typdef)
                }
            };
            tdenv.insert(id.clone(), typdef_source);
            self.insert_typdef(id, typdef);
        }
        tdenv
    }

    /// Prepares relations and functions against the complete type definitions.
    fn load_callables(&mut self, arena_mixop: &mut MixopArena, tdenv: &TDEnv, spec: source::Spec) {
        let mut layout = FrameLayout::default();
        let mut ctx = PrepareContext::new(arena_mixop, tdenv, &mut layout);
        for def in spec {
            match def.node.node {
                source::DefKind::Rel(rel) => {
                    // A defined relation is prepared into a callable with a frame layout
                    let rel = rel.prepare(&mut ctx);
                    let id = match &rel {
                        ast::RelDef::Extern(rel) => &rel.id,
                        ast::RelDef::Defined(rel) => &rel.def.id,
                    };
                    self.insert_rel(id.clone(), rel);
                }
                source::DefKind::MetaFunc(func) => {
                    // Prepare functions before sharing them with local bindings
                    let func = func.prepare(&mut ctx);
                    let id = match &func {
                        ast::MetaFuncDef::Extern(func) => &func.id,
                        ast::MetaFuncDef::Builtin(func) => &func.id,
                        ast::MetaFuncDef::Table(func) => &func.def.id,
                        ast::MetaFuncDef::Defined(func) => &func.def.id,
                    };
                    self.insert_func(id.clone(), func);
                }
                // Types loaded first; meta-variables carry no runtime state
                source::DefKind::Typ(_) | source::DefKind::Var(_) => {}
            }
        }
    }
}

// = Function signatures

impl FuncSignature for ast::MetaFuncDef {
    fn func_typ(&self) -> FuncTyp {
        fn param_typ(param: &ast::Param) -> ast::Typ {
            match &param.node {
                ast::ParamKind::Exp(typ, _) => typ.clone(),
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
            ast::MetaFuncDef::Table(func) => (&[], &func.def.params, &func.def.typ),
            ast::MetaFuncDef::Defined(func) => (&func.def.tparams, &func.def.params, &func.def.typ),
        };
        // Erase binding patterns while retaining nested function signatures
        FuncTyp {
            tparams: tparams.to_vec(),
            typs_params: params.iter().map(param_typ).collect(),
            typ_ret: Box::new(typ.clone()),
        }
    }
}
