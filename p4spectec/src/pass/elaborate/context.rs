//! Elaboration bindings and operation-local fresh state
//!
//! `Context` holds the type, meta-variable, relation, and function environments
//! built while walking definitions,
//! plus the free identifiers of the rule or clause under elaboration.
//!
//! A declaration registers an empty definition (`add_defined_rel`),
//! later definitions attach bodies to it (`add_defined_rule_group`),
//! and population takes the completed body back out (`take_defined_rel`).
//!
//! Lookups come in three forms:
//! `find_*_opt` returns an option,
//! `find_*` a located undefined error,
//! and `bound_*` a boolean.

use crate::lang::{
    common::{Id, ds::set::IdSet, source::Span},
    data::typ,
    hints::input::InputHint,
};

use crate::lang::il::ast;

use crate::runtime::{
    envs::elab::{FEnv, MEnv, REnv, TDEnv},
    typdef::TypeDef,
};

use crate::phrase;

use super::error::{self, ElabError};

/// Bindings and fresh state threaded through one elaboration operation.
#[derive(Clone, Debug)]
pub(super) struct Context {
    /// Free identifiers of the rule or clause under elaboration.
    pub(super) frees: IdSet,
    /// Type definitions.
    pub(super) tdenv: TDEnv,
    /// Meta-variable types.
    pub(super) menv: MEnv,
    /// Relation definitions.
    pub(super) renv: REnv,
    /// Function definitions.
    pub(super) fenv: FEnv,
}

impl Context {
    // == Constructors

    /// Creates a context with the primitive meta-variables
    /// `bool`, `nat`, `int`, and `text` bound to their types.
    pub(super) fn new() -> Self {
        // Primitive types are predeclared meta-variables
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
        Self {
            frees: IdSet::new(),
            tdenv: TDEnv::new(),
            menv,
            renv: REnv::new(),
            fenv: FEnv::new(),
        }
    }

    // == Finders

    // - Type definitions

    pub(super) fn find_typdef_opt(&self, id: &Id) -> Option<&TypeDef> {
        self.tdenv.get(id)
    }

    pub(super) fn find_typdef(&self, id: &Id) -> Result<&TypeDef, ElabError> {
        self.find_typdef_opt(id)
            .ok_or_else(|| error::decl::type_undefined(id))
    }

    // - Meta-variables

    pub(super) fn find_metavar_opt(&self, id: &Id) -> Option<&ast::Typ> {
        self.menv.get(id)
    }

    // - Relations

    /// Finds a relation only when it is defined rather than extern.
    pub(super) fn find_defined_rel_opt(&self, id: &Id) -> Option<&ast::DefinedRel> {
        match self.renv.get(id)? {
            ast::RelDef::Defined(defined_rel_il) => Some(defined_rel_il),
            ast::RelDef::Extern(_) => None,
        }
    }

    pub(super) fn find_defined_rel(&self, id: &Id) -> Result<&ast::DefinedRel, ElabError> {
        match self.renv.get(id) {
            // Rules attach only to defined relations
            Some(ast::RelDef::Defined(rel_il)) => Ok(rel_il),
            // Extern declarations identify the reason rules are forbidden
            Some(ast::RelDef::Extern(rel_il)) => {
                Err(error::decl::relation_extern_rule_unsupported(id, &rel_il.id.span))
            }
            // Missing user names remain located errors
            None => Err(error::decl::relation_rule_undefined(id)),
        }
    }

    /// Finds the notation type and input hint of any relation.
    pub(super) fn find_rel_signature_opt(&self, id: &Id) -> Option<(&ast::NotTyp, &InputHint)> {
        match self.renv.get(id)? {
            ast::RelDef::Extern(extern_rel_il) => {
                Some((&extern_rel_il.not_typ, &extern_rel_il.input_hint))
            }
            ast::RelDef::Defined(defined_rel_il) => {
                Some((&defined_rel_il.not_typ, &defined_rel_il.input_hint))
            }
        }
    }

    pub(super) fn find_rel_signature(
        &self,
        id: &Id,
    ) -> Result<(&ast::NotTyp, &InputHint), ElabError> {
        self.find_rel_signature_opt(id)
            .ok_or_else(|| error::decl::relation_undefined(id))
    }

    /// Finds the original identifier of a regular or otherwise rule group.
    fn find_rule_group_id(&self, relid: &Id, groupid: &Id) -> Option<&Id> {
        let rel_il = self.find_defined_rel_opt(relid)?;
        rel_il
            .rule_groups
            .iter()
            .map(|group| &group.node.id)
            .chain(rel_il.else_group.iter().map(|group| &group.node.id))
            .find(|id| id.node == groupid.node)
    }

    // - Functions

    /// Finds a function only when it is a table function.
    pub(super) fn find_table_func_opt(&self, id: &Id) -> Option<&ast::TableFunc> {
        match self.fenv.get(id)? {
            ast::MetaFuncDef::Table(table_func_il) => Some(table_func_il),
            _ => None,
        }
    }

    pub(super) fn find_table_func(&self, id: &Id) -> Result<&ast::TableFunc, ElabError> {
        match self.fenv.get_key_value(id) {
            // Table declarations admit row definitions
            Some((_, ast::MetaFuncDef::Table(func_il))) => Ok(func_il),
            // A different function kind is present, so relate its declaration
            Some((id_previous, _)) => {
                Err(error::decl::function_table_required(id, &id_previous.span))
            }
            // A missing table has no related declaration
            None => Err(error::decl::function_table_undefined(id)),
        }
    }

    /// Finds a function only when it is defined by clauses.
    pub(super) fn find_defined_func_opt(&self, id: &Id) -> Option<&ast::DefinedFunc> {
        match self.fenv.get(id)? {
            ast::MetaFuncDef::Defined(defined_func_il) => Some(defined_func_il),
            _ => None,
        }
    }

    pub(super) fn find_defined_func(&self, id: &Id) -> Result<&ast::DefinedFunc, ElabError> {
        self.find_defined_func_opt(id)
            .ok_or_else(|| error::decl::function_declaration_required(id))
    }

    /// Finds the declaration identifier and signature of any function.
    pub(super) fn find_func_signature_opt(
        &self,
        id: &Id,
    ) -> Option<(&Id, &[ast::TParam], &[ast::Param], &ast::Typ)> {
        let (id_declaration, func_il) = self.fenv.get_key_value(id)?;
        let (tparams, params, typ_ret) = match func_il {
            ast::MetaFuncDef::Extern(func_il) => {
                (func_il.tparams.as_slice(), func_il.params.as_slice(), &func_il.typ)
            }
            ast::MetaFuncDef::Builtin(func_il) => {
                (func_il.tparams.as_slice(), func_il.params.as_slice(), &func_il.typ)
            }
            // Table functions take no type parameters
            ast::MetaFuncDef::Table(func_il) => (&[][..], func_il.params.as_slice(), &func_il.typ),
            ast::MetaFuncDef::Defined(func_il) => {
                (func_il.tparams.as_slice(), func_il.params.as_slice(), &func_il.typ)
            }
        };
        Some((id_declaration, tparams, params, typ_ret))
    }

    /// Finds a signature or reports the undefined function at its use.
    pub(super) fn find_func_signature(
        &self,
        id: &Id,
    ) -> Result<(&Id, &[ast::TParam], &[ast::Param], &ast::Typ), ElabError> {
        self.find_func_signature_opt(id)
            .ok_or_else(|| error::decl::function_undefined(id))
    }

    // == Adders

    // - Free variables

    pub(super) fn add_free(&mut self, id: Id) {
        self.frees.insert(id);
    }

    pub(super) fn add_frees(&mut self, ids: &IdSet) {
        for id in ids.iter().cloned() {
            self.add_free(id);
        }
    }

    pub(super) fn reset_frees(&mut self) {
        self.frees = IdSet::new();
    }

    // - Meta-variables

    pub(super) fn add_metavar(&mut self, id: Id, typ: ast::Typ) -> Result<(), ElabError> {
        // Keep the original binding for the related label
        if let Some((id_previous, _)) = self.menv.get_key_value(&id) {
            return Err(error::decl::meta_variable_repeated(&id, &id_previous.span));
        }
        self.menv.insert(id, typ);
        Ok(())
    }

    // - Type definitions

    pub(super) fn add_typdef(&mut self, id: Id, typdef: TypeDef) -> Result<(), ElabError> {
        if let Some((id_previous, _)) = self.tdenv.get_key_value(&id) {
            return Err(error::typ::type_declaration_repeated(&id, &id_previous.span));
        }
        self.tdenv.insert(id, typdef);
        Ok(())
    }

    /// Binds a type parameter as a type and as a meta-variable of that type.
    pub(super) fn add_tparam(&mut self, tparam: ast::TParam) -> Result<(), ElabError> {
        if let Some((id_previous, _)) = self.tdenv.get_key_value(&tparam) {
            return Err(error::typ::type_parameter_repeated(&tparam, &id_previous.span));
        }
        if let Some((id_previous, _)) = self.menv.get_key_value(&tparam) {
            return Err(error::typ::type_parameter_repeated(&tparam, &id_previous.span));
        }
        let typ = typ::make::var(tparam.clone(), vec![]);
        self.add_typdef(tparam.clone(), TypeDef::Parameter)?;
        self.add_metavar(tparam, typ)
    }

    pub(super) fn add_tparams(&mut self, tparams: &[ast::TParam]) -> Result<(), ElabError> {
        for tparam in tparams {
            self.add_tparam(tparam.clone())?;
        }
        Ok(())
    }

    // - Relations

    pub(super) fn add_extern_rel(
        &mut self,
        extern_rel_il: ast::ExternRel,
    ) -> Result<(), ElabError> {
        let id = extern_rel_il.id.clone();
        if let Some((id_previous, _)) = self.renv.get_key_value(&id) {
            return Err(error::decl::relation_extern_repeated(&id, &id_previous.span));
        }
        self.renv
            .insert(id, ast::RelDef::Extern(Box::new(extern_rel_il)));
        Ok(())
    }

    pub(super) fn add_defined_rel(
        &mut self,
        defined_rel_il: ast::DefinedRel,
    ) -> Result<(), ElabError> {
        let id = defined_rel_il.id.clone();
        if let Some((id_previous, _)) = self.renv.get_key_value(&id) {
            return Err(error::decl::relation_repeated(&id, &id_previous.span));
        }
        self.renv
            .insert(id, ast::RelDef::Defined(Box::new(defined_rel_il)));
        Ok(())
    }

    /// Attaches an elaborated rule group to its defined relation.
    pub(super) fn add_defined_rule_group(
        &mut self,
        relid: &Id,
        rule_group: ast::RuleGroup,
    ) -> Result<(), ElabError> {
        // [elab_rule_group] has already admitted a defined relation
        assert!(self.find_defined_rel_opt(relid).is_some());
        let groupid = &rule_group.node.id;
        if let Some(id_previous) = self.find_rule_group_id(relid, groupid) {
            return Err(error::decl::relation_rule_group_repeated(groupid, &id_previous.span));
        }
        let ast::RelDef::Defined(defined_rel_il) =
            self.renv.get_mut(relid).expect("defined relation")
        else {
            unreachable!("checked defined relation")
        };
        defined_rel_il.rule_groups.push(rule_group);
        Ok(())
    }

    /// Attaches the single otherwise group to its defined relation.
    pub(super) fn add_defined_else_group(
        &mut self,
        relid: &Id,
        else_group: ast::ElseGroup,
    ) -> Result<(), ElabError> {
        // [elab_rule_group] has already admitted a defined relation
        assert!(self.find_defined_rel_opt(relid).is_some());
        let groupid = &else_group.node.id;
        if let Some(id_previous) = self.find_rule_group_id(relid, groupid) {
            return Err(error::decl::relation_rule_group_repeated(groupid, &id_previous.span));
        }
        let ast::RelDef::Defined(defined_rel_il) =
            self.renv.get_mut(relid).expect("defined relation")
        else {
            unreachable!("checked defined relation")
        };
        // A relation has at most one otherwise group
        if let Some(group_previous) = &defined_rel_il.else_group {
            return Err(error::decl::relation_otherwise_repeated(
                relid,
                &else_group.span,
                &group_previous.span,
            ));
        }
        defined_rel_il.else_group = Some(else_group);
        Ok(())
    }

    // - Functions

    pub(super) fn add_extern_func(
        &mut self,
        extern_func_il: ast::ExternFunc,
    ) -> Result<(), ElabError> {
        let id = extern_func_il.id.clone();
        // Check the shared namespace before storing this declaration kind
        if let Some((id_previous, _)) = self.fenv.get_key_value(&id) {
            return Err(error::decl::function_extern_repeated(&id, &id_previous.span));
        }
        self.fenv
            .insert(id, ast::MetaFuncDef::Extern(extern_func_il));
        Ok(())
    }

    pub(super) fn add_builtin_func(
        &mut self,
        builtin_func_il: ast::BuiltinFunc,
    ) -> Result<(), ElabError> {
        let id = builtin_func_il.id.clone();
        // Check the shared namespace before storing this declaration kind
        if let Some((id_previous, _)) = self.fenv.get_key_value(&id) {
            return Err(error::decl::function_builtin_repeated(&id, &id_previous.span));
        }
        self.fenv
            .insert(id, ast::MetaFuncDef::Builtin(builtin_func_il));
        Ok(())
    }

    pub(super) fn add_table_func(
        &mut self,
        table_func_il: ast::TableFunc,
    ) -> Result<(), ElabError> {
        let id = table_func_il.id.clone();
        // Check the shared namespace before storing this declaration kind
        if let Some((id_previous, _)) = self.fenv.get_key_value(&id) {
            return Err(error::decl::function_table_repeated(&id, &id_previous.span));
        }
        self.fenv.insert(id, ast::MetaFuncDef::Table(table_func_il));
        Ok(())
    }

    pub(super) fn add_defined_func(
        &mut self,
        defined_func_il: ast::DefinedFunc,
    ) -> Result<(), ElabError> {
        let id = defined_func_il.id.clone();
        // Check the shared namespace before storing this declaration kind
        if let Some((id_previous, _)) = self.fenv.get_key_value(&id) {
            return Err(error::decl::function_repeated(&id, &id_previous.span));
        }
        self.fenv
            .insert(id, ast::MetaFuncDef::Defined(Box::new(defined_func_il)));
        Ok(())
    }

    /// Stores the rows of a declared table function, which must still be empty.
    pub(super) fn add_table_func_rows(
        &mut self,
        id: &Id,
        table_rows: Vec<ast::TableRow>,
    ) -> Result<(), ElabError> {
        // [elab_table_def] has already admitted the table before elaborating rows
        let table_func_il = self
            .find_table_func_opt(id)
            .expect("elaborated table declaration");
        // The later table identifier owns the failure, the earlier row is related
        if let Some(row_previous) = table_func_il.rows.first() {
            return Err(error::decl::table_row_repeated(id, &row_previous.span));
        }
        let ast::MetaFuncDef::Table(table_func_il) = self.fenv.get_mut(id).expect("table function")
        else {
            unreachable!("checked table function")
        };
        table_func_il.rows = table_rows;
        Ok(())
    }

    /// Appends a clause to a declared function.
    pub(super) fn add_defined_func_clause(&mut self, id: &Id, clause: ast::Clause) {
        // [elab_clause] has already admitted the matching function declaration
        assert!(self.find_defined_func_opt(id).is_some());
        let ast::MetaFuncDef::Defined(defined_func_il) =
            self.fenv.get_mut(id).expect("defined function")
        else {
            unreachable!("checked defined function")
        };
        defined_func_il.clauses.push(clause);
    }

    /// Sets the single otherwise clause of a declared function.
    pub(super) fn add_defined_func_else_clause(
        &mut self,
        id: &Id,
        else_clause: ast::ElseClause,
    ) -> Result<(), ElabError> {
        // [elab_clause] has already admitted the matching function declaration
        assert!(self.find_defined_func_opt(id).is_some());
        let ast::MetaFuncDef::Defined(defined_func_il) =
            self.fenv.get_mut(id).expect("defined function")
        else {
            unreachable!("checked defined function")
        };
        // A function has at most one otherwise clause
        if let Some(clause_previous) = &defined_func_il.else_clause {
            return Err(error::decl::function_otherwise_repeated(
                id,
                &else_clause.span,
                &clause_previous.span,
            ));
        }
        defined_func_il.else_clause = Some(else_clause);
        Ok(())
    }

    // == Definition extraction

    // - Relations

    /// Removes a defined relation and returns it with its rule groups.
    pub(super) fn take_defined_rel(&mut self, id: &Id) -> ast::DefinedRel {
        // [elab_def] registers declarations before [populate_defs] consumes them
        let Some(ast::RelDef::Defined(defined_rel_il)) = self.renv.remove(id) else {
            unreachable!("registered declaration")
        };
        *defined_rel_il
    }

    // - Functions

    /// Removes a table function and returns it with its collected rows.
    pub(super) fn take_table_func(&mut self, id: &Id) -> ast::TableFunc {
        // [elab_def] registers declarations before [populate_defs] consumes them
        let Some(ast::MetaFuncDef::Table(table_func_il)) = self.fenv.remove(id) else {
            unreachable!("registered declaration")
        };
        table_func_il
    }

    /// Removes a defined function and returns it with its collected clauses.
    pub(super) fn take_defined_func(&mut self, id: &Id) -> ast::DefinedFunc {
        // [elab_def] registers declarations before [populate_defs] consumes them
        let Some(ast::MetaFuncDef::Defined(defined_func_il)) = self.fenv.remove(id) else {
            unreachable!("registered declaration")
        };
        *defined_func_il
    }

    // == Updaters

    // - Type definitions

    /// Replaces the definition of an already-declared type.
    pub(super) fn update_typdef(&mut self, id: &Id, typdef: TypeDef) {
        // [elab_typ_def] first admits an existing type binding
        let typdef_stored = self.tdenv.get_mut(id).expect("admitted type definition");
        *typdef_stored = typdef;
    }
}
