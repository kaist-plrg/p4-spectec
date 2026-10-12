//! Type and metavariable environments used during structuring
//!
//! `Context::load` reads every type and meta-variable definition of the AL
//! specification once;
//! structuring then only queries `tdenv` and `menv`,
//! for example to expand a variant type when totalizing a case analysis
//! or to pick fresh input names from meta-variable types.

use crate::lang::{
    common::{Id, source::Span},
    data::typ,
};

use crate::lang::al::ast;

use crate::runtime::{
    envs::algo::{MEnv, TDEnv},
    typdef::TypeDef,
};

use crate::phrase;

/// Type and meta-variable environments of the specification being structured.
#[derive(Clone, Debug)]
pub struct Context {
    /// Type definitions.
    pub(crate) tdenv: TDEnv,
    /// Meta-variable types.
    pub(crate) menv: MEnv,
}

impl Context {
    // - Constructor

    /// Creates a context with the primitive meta-variables bound.
    fn init() -> Self {
        let mut menv = MEnv::new();
        for (text_name, typ) in [
            ("bool", typ::make::bool()),
            ("nat", typ::make::nat()),
            ("int", typ::make::int()),
            ("text", typ::make::text()),
        ] {
            let id = phrase!(node: text_name.into(), span: Span::default());
            menv.insert(id, typ);
        }
        Self { tdenv: TDEnv::new(), menv }
    }

    // - Adders

    fn add_typdef(&mut self, id: Id, typdef: TypeDef) {
        // Elaboration rejects duplicate types before binding preserves definitions
        assert!(!self.tdenv.contains_key(&id), "type already defined");
        self.tdenv.insert(id, typdef);
    }

    fn add_metavar(&mut self, id: Id, typ: ast::Typ) {
        // Elaboration reserves the same primitive and type metavariable names
        assert!(!self.menv.contains_key(&id), "metavariable already defined");
        self.menv.insert(id, typ);
    }

    // - Definition loading

    /// Loads a type or meta-variable definition; other definitions add nothing.
    fn load_def(&mut self, def_al: &ast::Def) {
        let def_kind_al = &def_al.node;
        match def_kind_al {
            ast::DefKind::Typ(typ_def_al) => self.load_typ_def(typ_def_al),
            ast::DefKind::Var(def_var_al) => self.load_var_def(def_var_al),
            _ => (),
        }
    }

    fn load_typ_def(&mut self, typ_def_al: &ast::TypDef) {
        match typ_def_al {
            ast::TypDef::Extern(extern_typ_al) => self.load_extern_typ(extern_typ_al),
            ast::TypDef::Defined(defined_typ_al) => self.load_defined_typ(defined_typ_al),
        }
    }

    /// Registers an extern type and a meta-variable of that type.
    fn load_extern_typ(&mut self, extern_typ_al: &ast::ExternTyp) {
        let id = extern_typ_al.id.clone();
        let typ = typ::make::var(id.clone(), vec![]);
        self.add_metavar(id.clone(), typ);
        self.add_typdef(id, TypeDef::Extern)
    }

    /// Registers a defined type and, if unparameterized, a meta-variable of it.
    fn load_defined_typ(&mut self, defined_typ_al: &ast::DefinedTyp) {
        let id = defined_typ_al.id.clone();
        if defined_typ_al.tparams.is_empty() {
            let typ = typ::make::var(id.clone(), vec![]);
            self.add_metavar(id.clone(), typ);
        }
        let typdef = TypeDef::Defined(
            defined_typ_al.tparams.clone(),
            Box::new(defined_typ_al.def_typ.clone()),
        );
        self.add_typdef(id, typdef)
    }

    fn load_var_def(&mut self, def_var_al: &ast::VarDef) {
        self.add_metavar(def_var_al.id.clone(), def_var_al.typ.clone())
    }

    /// Builds the context from every definition of the specification.
    pub fn load(spec_al: &ast::Spec) -> Self {
        let mut ctx = Self::init();
        for def_al in spec_al {
            ctx.load_def(def_al);
        }
        ctx
    }
}
