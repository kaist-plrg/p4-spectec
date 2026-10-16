//! State accumulated while analyzing bindings
//!
//! `Context` carries the free identifiers of the construct under analysis,
//! the variables already bound (`venv`) with their dimensions,
//! and the type and meta-variable environments loaded from the specification.
//! Premises bind variables in order,
//! so `venv` grows as a premise list is analyzed.

use crate::lang::{
    common::{Id, ds::set::IdSet, source::Span},
    data::typ,
};

use crate::lang::il::ast;

use crate::runtime::{
    envs::algo::{MEnv, TDEnv, VEnv},
    typdef::TypeDef,
};

use crate::phrase;

use super::super::{AlgoError, error};

/// Bindings and environments threaded through one binding analysis.
#[derive(Clone, Debug)]
pub struct Context {
    /// Free identifiers of the construct under analysis.
    pub(crate) frees: IdSet,
    /// Variables bound so far, with their dimensions.
    pub(crate) venv: VEnv,
    /// Type definitions.
    pub(crate) tdenv: TDEnv,
    /// Meta-variable types.
    pub(crate) menv: MEnv,
}

impl Context {
    // - Constructor

    /// Creates a context with the primitive meta-variables bound.
    pub fn new() -> Self {
        let mut menv = MEnv::new();
        for (name, typ) in [
            ("bool", typ::make::bool()),
            ("nat", typ::make::nat()),
            ("int", typ::make::int()),
            ("text", typ::make::text()),
        ] {
            let id = phrase!(node: name.into(), span: Span::default());
            menv.insert(id, typ);
        }
        Self { frees: IdSet::new(), venv: VEnv::new(), tdenv: TDEnv::new(), menv }
    }

    // - Adders

    pub fn add_free(&mut self, id: Id) {
        self.frees.insert(id);
    }

    pub fn add_frees(&mut self, ids: &IdSet) {
        for id in ids.iter() {
            self.add_free(id.clone());
        }
    }

    /// Records newly bound variables, keeping the first dimension seen.
    pub fn add_bounds(&mut self, venv: &VEnv) {
        for (id, dim) in venv.iter() {
            if !self.venv.contains_key(id) {
                self.venv.insert(id.clone(), dim.clone());
            }
        }
    }

    // - Finders

    pub fn find_typdef_opt(&self, id: &Id) -> Option<&TypeDef> {
        self.tdenv.get(id)
    }

    pub fn find_typdef(&self, id: &Id) -> Result<&TypeDef, AlgoError> {
        self.find_typdef_opt(id)
            .ok_or_else(|| error::typ::type_undefined(id))
    }

    // - Definition loading

    /// Loads a type or meta-variable definition into the environments.
    pub fn load_def(&mut self, def_al: &ast::Def) {
        match &def_al.node {
            ast::DefKind::Typ(typ_def_al) => self.load_typ_def(typ_def_al),
            ast::DefKind::Var(var_def) => {
                self.menv.insert(var_def.id.clone(), var_def.typ.clone());
            }
            _ => {}
        }
    }

    /// Stores an extern or defined type under its id.
    fn load_typ_def(&mut self, typ_def_al: &ast::TypDef) {
        match typ_def_al {
            ast::TypDef::Extern(extern_typ_al) => {
                self.tdenv.insert(extern_typ_al.id.clone(), TypeDef::Extern);
            }
            ast::TypDef::Defined(defined_typ_al) => {
                let typdef = TypeDef::Defined(
                    defined_typ_al.tparams.clone(),
                    Box::new(defined_typ_al.def_typ.clone()),
                );
                self.tdenv.insert(defined_typ_al.id.clone(), typdef);
            }
        }
    }

    /// Loads every type and meta-variable definition of the specification.
    pub fn load(&mut self, spec_al: &ast::Spec) {
        for def_al in spec_al {
            self.load_def(def_al);
        }
    }
}
