//! Loaded definitions, local bindings, and shared context access
//!
//! Stage-specific loaders prepare callables into `Global<R, F>`.
//! A `Context` borrows those globals and owns persistent local bindings;
//! cloning preserves its scope, while `localize` starts a fresh one.
//! `ReadContext`, `WriteContext`, and `IterContext` serve shared evaluation;
//! `FuncSignature` reads function types from each stage's prepared syntax.

use std::rc::Rc;

use crate::lang::{
    common::{ds::map::IdMap, source::Span},
    data::{
        arena::Arena,
        typ,
        value::flat::{self as value, Value},
        var::{SlotIdx, VarSlot},
    },
};

use crate::lang::il::prepared as ast;

use crate::diagnostic::{Label, Report};

use crate::runtime::envs::interp::shared::{
    TDEnv, TypeDef,
    callable::Callable,
    frame::{Frame, FrameLayout},
};

use crate::interp::shared::{
    backtrack::{Backtrack, ok, unwrap_from_result},
    error::{self, EntityKind, Error},
};

// = Function signatures

/// Reads a function type from a stage-specific prepared definition.
pub trait FuncSignature {
    /// Extracts the type parameters, parameter types, and return type.
    fn func_typ(&self) -> ast::FuncTyp;
}

// = Context access

/// Read access to value, type, and function bindings.
pub trait ReadContext {
    // == Values

    /// Finds the value bound at `slot`, if any.
    fn find_value_at_slot(&self, slot: SlotIdx) -> Option<&Value>;
    /// Finds the slot of `var` under one more iteration `iter`.
    fn find_var_iterated(&self, var: &VarSlot, iter: ast::Iter) -> VarSlot;

    /// Finds the prepared slot under one more iteration.
    fn find_slot_iterated(&self, var: &VarSlot, iter: ast::Iter) -> SlotIdx {
        self.find_var_iterated(var, iter).slot
    }

    // == Types

    /// Finds a type definition by id, if any.
    fn find_typdef_opt(&self, id: &ast::Id) -> Option<&TypeDef>;
    /// Finds a type bound by the current call, excluding global definitions.
    fn find_typdef_local_opt(&self, id: &ast::Id) -> Option<&TypeDef>;
    /// Finds the parameters and body of a defined type or reports an error.
    fn find_defined_typdef(&self, id: &ast::Id) -> Result<(&[ast::TParam], &ast::DefTyp), Error>;

    // == Functions

    /// The function definition type this context stores.
    type Func;

    /// Finds a function definition by id.
    fn find_func(&self, id: &ast::Id) -> Result<&Rc<Self::Func>, Error>;
    /// Finds the type of a function by id.
    fn find_func_typ(&self, id: &ast::Id) -> Result<ast::FuncTyp, Error>;
}

/// Write access to type, value, and function bindings.
pub trait WriteContext: ReadContext + Clone {
    // == Types

    /// Binds a type definition, rejecting duplicates only in the local scope.
    fn add_typdef_local(&mut self, id: ast::Id, typdef: TypeDef) -> Result<(), Error>;

    // == Values

    /// Binds a value to a slot.
    fn add_value_at_slot(&mut self, slot: SlotIdx, value: Value);
    /// Drops every value binding.
    fn clear_value_bindings(&mut self);

    // == Functions

    /// Binds a function definition to an id.
    fn add_func(&mut self, id: ast::Id, func: Rc<Self::Func>) -> Result<(), Error>;
}

/// Context operations used by shared iteration evaluation.
pub trait IterContext: WriteContext {
    // == Input values

    /// Finds the list values bound to `vars`, requiring equal lengths.
    fn find_list_values_by_var<'arena>(
        &self,
        arena: &'arena Arena,
        vars: &[ast::Var],
    ) -> Result<Vec<&'arena [Value]>, Error>;

    /// Finds the option values bound to `vars`, all present or all absent.
    fn find_opt_values_by_var(
        &self,
        arena: &Arena,
        vars: &[ast::Var],
    ) -> Result<Option<Vec<Value>>, Error>;

    // == Output bindings

    /// Appends the values bound to `vars` to their columns.
    fn collect_values_by_var(
        &self,
        vars: &[ast::Var],
        values_by_var: &mut [Vec<Value>],
    ) -> Backtrack<()>;

    /// Binds each variable to the list of its column.
    fn bind_list_values_by_var(
        &mut self,
        arena: &mut Arena,
        vars: &[ast::Var],
        values_by_var: Vec<Vec<Value>>,
    ) -> Backtrack<()>;

    /// Binds each variable to the option built from its column.
    fn bind_opt_values_by_var(
        &mut self,
        arena: &mut Arena,
        vars: &[ast::Var],
        values_by_var: Vec<Vec<Value>>,
    ) -> Backtrack<()>;
}

// = Definition scopes

/// Identifies the scope supplying a function definition.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Scope {
    /// Loaded from the specification.
    Global,
    /// Bound by a function argument in the current call.
    Local,
}

// = Global definitions

/// Stores type definitions and prepared relation and function callables.
#[derive(Debug)]
pub struct Global<R, F> {
    tdenv: TDEnv,
    renv: IdMap<Callable<R>>,
    fenv: IdMap<Rc<Callable<F>>>,
}

impl<R, F> Global<R, F> {
    // == Constructors

    /// Creates empty environments for a stage-specific loader.
    pub(crate) fn new() -> Self {
        Self { tdenv: TDEnv::new(), renv: IdMap::new(), fenv: IdMap::new() }
    }

    // == Inserters

    // - Types

    /// Inserts a global type, panicking if its name is already defined.
    pub(crate) fn insert_typdef(&mut self, id: ast::Id, typdef: TypeDef) {
        // Elaboration already rejects duplicate type names
        assert!(
            !self.tdenv.contains_key(&id),
            "global type definitions must be unique: {}",
            id.node
        );
        self.tdenv.insert(id, typdef);
    }

    // - Relations

    /// Inserts a prepared relation, panicking if its name is already defined.
    pub(crate) fn insert_rel(&mut self, id: ast::Id, rel: Callable<R>)
    where
        R: Clone,
    {
        // Elaboration already rejects duplicate relation names
        assert!(
            !self.renv.contains_key(&id),
            "global relation definitions must be unique: {}",
            id.node
        );
        self.renv.insert(id, rel);
    }

    // - Functions

    /// Inserts a prepared function, panicking if its name is already defined.
    pub(crate) fn insert_func(&mut self, id: ast::Id, func: Callable<F>) {
        // Elaboration already rejects duplicate function names
        assert!(
            !self.fenv.contains_key(&id),
            "global function definitions must be unique: {}",
            id.node
        );
        self.fenv.insert(id, Rc::new(func));
    }
}

// = Local bindings

/// Holds type parameters, function arguments, and values of one call.
#[derive(Debug)]
struct Local<F> {
    /// Type parameters bound to their type arguments.
    tdenv: TDEnv,
    /// Function arguments bound to their prepared definitions.
    fenv: IdMap<Rc<Callable<F>>>,
    /// Value slots of the current callable.
    frame: Frame,
}

impl<F> Default for Local<F> {
    fn default() -> Self {
        Self { tdenv: TDEnv::new(), fenv: IdMap::new(), frame: Frame::default() }
    }
}

impl<F> Clone for Local<F> {
    fn clone(&self) -> Self {
        Self { tdenv: self.tdenv.clone(), fenv: self.fenv.clone(), frame: self.frame.clone() }
    }
}

// = Execution context

/// Combines borrowed global definitions with local bindings of one call.
#[derive(Debug)]
pub struct Context<'global, R, F> {
    global: &'global Global<R, F>,
    local: Local<F>,
}

impl<R, F> Clone for Context<'_, R, F> {
    fn clone(&self) -> Self {
        Self { global: self.global, local: self.local.clone() }
    }
}

impl<'global, R, F: FuncSignature> Context<'global, R, F> {
    // == Constructors

    /// Creates a context with no local bindings.
    pub fn new(global: &'global Global<R, F>) -> Self {
        Self { global, local: Local::default() }
    }

    /// Starts a fresh local scope over the same globals.
    pub fn localize(&self) -> Self {
        Self::new(self.global)
    }

    /// Starts a fresh local scope with the callee's frame layout.
    pub fn localize_with_layout(&self, layout: &Rc<FrameLayout>) -> Self {
        Self {
            global: self.global,
            local: Local {
                tdenv: TDEnv::new(),
                fenv: IdMap::new(),
                frame: Frame::new(Rc::clone(layout)),
            },
        }
    }

    // == Finders

    // - Types

    /// Finds a type in either scope or reports the lookup location.
    pub fn find_typdef<'a>(&'a self, id: &ast::Id) -> Result<&'a TypeDef, Error> {
        self.find_typdef_opt(id).ok_or_else(|| {
            let diagnostic = error::context::binding_undefined(EntityKind::Type, id.node.clone())
                .with_label(Label::primary(&id.span, ""));
            Box::new(Report::from(diagnostic))
        })
    }

    // - Relations

    /// Finds a relation in the global definitions.
    pub fn find_rel_opt(&self, id: &ast::Id) -> Option<&'global Callable<R>> {
        self.global.renv.get(id)
    }

    /// Finds a global relation or reports the lookup location.
    pub fn find_rel(&self, id: &ast::Id) -> Result<&'global Callable<R>, Error> {
        self.find_rel_opt(id).ok_or_else(|| {
            let diagnostic =
                error::context::binding_undefined(EntityKind::Relation, id.node.clone())
                    .with_label(Label::primary(&id.span, ""));
            Box::new(Report::from(diagnostic))
        })
    }

    // - Functions

    /// Finds a function, preferring local arguments to global definitions.
    pub fn find_func_opt<'a>(&'a self, id: &ast::Id) -> Option<(Scope, &'a Rc<Callable<F>>)> {
        if let Some(func) = self.local.fenv.get(id) {
            Some((Scope::Local, func))
        } else {
            self.global.fenv.get(id).map(|func| (Scope::Global, func))
        }
    }

    /// Finds a function and its scope or reports the lookup location.
    pub fn find_func_with_scope<'a>(
        &'a self,
        id: &ast::Id,
    ) -> Result<(Scope, &'a Rc<Callable<F>>), Error> {
        self.find_func_opt(id).ok_or_else(|| {
            let diagnostic =
                error::context::binding_undefined(EntityKind::Function, id.node.clone())
                    .with_label(Label::primary(&id.span, ""));
            Box::new(Report::from(diagnostic))
        })
    }

    // == Adders

    // - Types

    /// Binds a type locally; the id must be new in both scopes.
    pub fn add_typdef(&mut self, id: ast::Id, typdef: TypeDef) -> Result<(), Error> {
        if self.find_typdef_opt(&id).is_some() {
            let diagnostic = error::context::binding_repeated(EntityKind::Type, id.node)
                .with_label(Label::primary(&id.span, ""));
            return Err(Box::new(Report::from(diagnostic)));
        }
        self.local.tdenv.insert(id, typdef);
        Ok(())
    }
}

// = Read access

impl<R, F: FuncSignature> ReadContext for Context<'_, R, F> {
    type Func = Callable<F>;

    // == Finders

    // - Values

    fn find_value_at_slot(&self, slot: SlotIdx) -> Option<&Value> {
        self.local.frame.get(slot)
    }

    fn find_slot_iterated(&self, var: &VarSlot, iter: ast::Iter) -> SlotIdx {
        self.local.frame.layout().find_slot_iterated(var.slot, iter)
    }

    fn find_var_iterated(&self, var: &VarSlot, iter: ast::Iter) -> VarSlot {
        self.local.frame.layout().find_var_iterated(var, iter)
    }

    // - Types

    fn find_typdef_local_opt(&self, id: &ast::Id) -> Option<&TypeDef> {
        self.local.tdenv.get(id)
    }

    fn find_typdef_opt<'a>(&'a self, id: &ast::Id) -> Option<&'a TypeDef> {
        // Local type parameters shadow global types
        self.find_typdef_local_opt(id)
            .or_else(|| self.global.tdenv.get(id))
    }

    fn find_defined_typdef<'a>(
        &'a self,
        id: &ast::Id,
    ) -> Result<(&'a [ast::TParam], &'a ast::DefTyp), Error> {
        match self.find_typdef(id)? {
            TypeDef::Defined(tparams, def_typ) => Ok((tparams, def_typ)),
            _ => {
                let diagnostic =
                    error::context::binding_undefined(EntityKind::DefinedType, id.node.clone())
                        .with_label(Label::primary(&id.span, ""));
                Err(Box::new(Report::from(diagnostic)))
            }
        }
    }

    // - Functions

    fn find_func(&self, id: &ast::Id) -> Result<&Rc<Self::Func>, Error> {
        self.find_func_with_scope(id).map(|(_, func)| func)
    }

    fn find_func_typ(&self, id: &ast::Id) -> Result<ast::FuncTyp, Error> {
        Ok(self.find_func(id)?.def.func_typ())
    }
}

// = Write access

impl<R, F: FuncSignature> WriteContext for Context<'_, R, F> {
    // == Adders

    // - Types

    fn add_typdef_local(&mut self, id: ast::Id, typdef: TypeDef) -> Result<(), Error> {
        // A type parameter may shadow a global definition
        if self.local.tdenv.contains_key(&id) {
            let diagnostic = error::context::binding_repeated(EntityKind::Type, id.node)
                .with_label(Label::primary(&id.span, ""));
            return Err(Box::new(Report::from(diagnostic)));
        }
        self.local.tdenv.insert(id, typdef);
        Ok(())
    }

    // - Values

    fn add_value_at_slot(&mut self, slot: SlotIdx, value: Value) {
        self.local.frame.set(slot, value);
    }

    // - Functions

    fn add_func(&mut self, id: ast::Id, func: Rc<Callable<F>>) -> Result<(), Error> {
        if self.find_func_opt(&id).is_some() {
            let diagnostic = error::context::binding_repeated(EntityKind::Function, id.node)
                .with_label(Label::primary(&id.span, ""));
            return Err(Box::new(Report::from(diagnostic)));
        }
        self.local.fenv.insert(id, func);
        Ok(())
    }

    // == Clearing

    fn clear_value_bindings(&mut self) {
        // Keep the layout, drop the values
        self.local.frame = self.local.frame.wipe();
    }
}

// = Iteration access

impl<R, F: FuncSignature> IterContext for Context<'_, R, F> {
    // == Finders

    // - Values

    fn find_list_values_by_var<'a>(
        &self,
        arena: &'a Arena,
        vars: &[ast::Var],
    ) -> Result<Vec<&'a [Value]>, Error> {
        let mut values_by_var = Vec::with_capacity(vars.len());
        for var in vars {
            // Every variable must be bound
            let value = self
                .find_value_at_slot(var.slot)
                .expect("value must be bound");
            // Each variable must hold a list
            let values = value::get::list(arena, value).expect("iteration input must be a list");
            values_by_var.push(values);
        }
        // No variables: nothing to iterate
        let Some(values) = values_by_var.first() else {
            return Ok(Vec::new());
        };
        // All lists must have the same length
        let len = values.len();
        for values in &values_by_var {
            if values.len() != len {
                return Err(Box::new(
                    error::context::iteration_length_mismatch(len, values.len()).into(),
                ));
            }
        }
        Ok(values_by_var)
    }

    fn find_opt_values_by_var(
        &self,
        arena: &Arena,
        vars: &[ast::Var],
    ) -> Result<Option<Vec<Value>>, Error> {
        let mut values = Vec::with_capacity(vars.len());
        for var in vars {
            // Every variable must be bound
            let value = self
                .find_value_at_slot(var.slot)
                .expect("value must be bound");
            // Each variable must hold an option
            let value = value::get::opt(arena, value).expect("iteration input must be an option");
            values.push(value);
        }
        // All present, all absent, or a mismatch
        if values.iter().all(|value| value.is_some()) {
            Ok(Some(values.into_iter().flatten().collect()))
        } else if values.iter().all(|value| value.is_none()) {
            Ok(None)
        } else {
            Err(Box::new(error::context::iteration_optionality_mismatch().into()))
        }
    }

    // == Collectors

    // - Values

    fn collect_values_by_var(
        &self,
        vars: &[ast::Var],
        values_by_var: &mut [Vec<Value>],
    ) -> Backtrack<()> {
        // Append this row's value of each variable
        for (var, values) in vars.iter().zip(values_by_var) {
            values.push(
                *self
                    .find_value_at_slot(var.slot)
                    .expect("value must be bound"),
            );
        }
        ok!(())
    }

    // == Adders

    // - Values

    fn bind_list_values_by_var(
        &mut self,
        arena: &mut Arena,
        vars: &[ast::Var],
        values_by_var: Vec<Vec<Value>>,
    ) -> Backtrack<()> {
        for (var, values) in vars.iter().zip(values_by_var) {
            let typ = typ::make::iterate(var.var.typ.clone(), &var.var.iters);
            // Each variable becomes a list one iteration outward
            let value = value::make::list(arena, typ.node.into(), values, Span::default());
            let value = unwrap_from_result!(value, &Span::default());
            self.add_value_at_slot(var.slot, value);
        }
        ok!(())
    }

    fn bind_opt_values_by_var(
        &mut self,
        arena: &mut Arena,
        vars: &[ast::Var],
        values_by_var: Vec<Vec<Value>>,
    ) -> Backtrack<()> {
        for (var, values) in vars.iter().zip(values_by_var) {
            let typ = typ::make::iterate(var.var.typ.clone(), &var.var.iters);
            // Each variable becomes an option one iteration outward
            let value = value::make::opt(
                arena,
                typ.node.into(),
                values.into_iter().next(),
                Span::default(),
            );
            let value = unwrap_from_result!(value, &Span::default());
            self.add_value_at_slot(var.slot, value);
        }
        ok!(())
    }
}
