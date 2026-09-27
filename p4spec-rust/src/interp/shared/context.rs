//! Loaded definitions, local bindings, and shared context access
//!
//! Stage-specific loaders prepare callables into `Global<R, F>`.
//! A `Context` borrows those globals and owns persistent local bindings;
//! cloning preserves its scope, while `localize` starts a fresh one.
//! `ReadContext`, `WriteContext`, and `IterContext` serve shared evaluation;
//! `FuncSignature` reads function types from each stage's prepared syntax.

use std::rc::Rc;

use crate::{
    interp::shared::{
        backtrack::{Backtrack, ok, unwrap_from_result},
        error::{ContextErrorKind, EntityKind, Error, ErrorKind},
        prepare::ast,
    },
    lang::{
        common::{ds::map::IdMap, source::Span},
        data::{
            typ,
            value::{Value, ValueArena, get, make},
            var::{SlotIdx, VarSlot},
        },
        traits::print::Print,
    },
    runtime::{
        envs::interp::shared::{
            TDEnv,
            callable::Callable,
            frame::{Frame, FrameLayout},
        },
        typdef::TypeDef,
    },
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
    fn find_value(&self, slot: SlotIdx) -> Option<&Value>;
    /// Finds the slot of `var` under one more iteration `iter`.
    fn find_iter_var(&self, var: &VarSlot, iter: ast::Iter) -> VarSlot;

    /// Finds the prepared slot under one more iteration.
    fn find_iter_slot(&self, var: &VarSlot, iter: ast::Iter) -> SlotIdx {
        self.find_iter_var(var, iter).slot
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
    fn add_value(&mut self, slot: SlotIdx, value: Value);
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
        arena: &'arena ValueArena,
        vars: &[ast::Var],
    ) -> Result<Vec<&'arena [Value]>, Error>;

    /// Finds the option values bound to `vars`, all present or all absent.
    fn find_opt_values_by_var(
        &self,
        arena: &ValueArena,
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
        arena: &mut ValueArena,
        vars: &[ast::Var],
        values_by_var: Vec<Vec<Value>>,
    ) -> Backtrack<()>;

    /// Binds each variable to the option built from its column.
    fn bind_opt_values_by_var(
        &mut self,
        arena: &mut ValueArena,
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

    /// Inserts a type, rejecting duplicates without replacing the definition.
    pub(crate) fn insert_typdef(&mut self, id: ast::Id, typdef: TypeDef) -> Result<(), Error> {
        if self.tdenv.contains_key(&id) {
            return Err(Error::duplicate(EntityKind::Type, id.node, id.span));
        }
        self.tdenv.insert(id, typdef);
        Ok(())
    }

    // - Relations

    /// Inserts a prepared relation, rejecting duplicates in its namespace.
    pub(crate) fn insert_rel(&mut self, id: ast::Id, rel: Callable<R>) -> Result<(), Error>
    where
        R: Clone,
    {
        // Check before the persistent map can replace the existing callable
        if self.renv.contains_key(&id) {
            return Err(Error::duplicate(EntityKind::Relation, id.node, id.span));
        }
        self.renv.insert(id, rel);
        Ok(())
    }

    // - Functions

    /// Inserts a prepared function, sharing it with future function arguments.
    pub(crate) fn insert_func(&mut self, id: ast::Id, func: Callable<F>) -> Result<(), Error> {
        if self.fenv.contains_key(&id) {
            return Err(Error::duplicate(EntityKind::Function, id.node, id.span));
        }
        self.fenv.insert(id, Rc::new(func));
        Ok(())
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
        self.find_typdef_opt(id)
            .ok_or_else(|| Error::undefined(EntityKind::Type, id.node.clone(), id.span.clone()))
    }

    // - Relations

    /// Finds a relation in the global definitions.
    pub fn find_rel_opt(&self, id: &ast::Id) -> Option<&'global Callable<R>> {
        self.global.renv.get(id)
    }

    /// Finds a global relation or reports the lookup location.
    pub fn find_rel(&self, id: &ast::Id) -> Result<&'global Callable<R>, Error> {
        self.find_rel_opt(id)
            .ok_or_else(|| Error::undefined(EntityKind::Relation, id.node.clone(), id.span.clone()))
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
        self.find_func_opt(id)
            .ok_or_else(|| Error::undefined(EntityKind::Function, id.node.clone(), id.span.clone()))
    }

    // == Adders

    // - Types

    /// Binds a type locally; the id must be new in both scopes.
    pub fn add_typdef(&mut self, id: ast::Id, typdef: TypeDef) -> Result<(), Error> {
        if self.find_typdef_opt(&id).is_some() {
            return Err(Error::duplicate(EntityKind::Type, id.node, id.span));
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

    fn find_value(&self, slot: SlotIdx) -> Option<&Value> {
        self.local.frame.get(slot)
    }

    fn find_iter_slot(&self, var: &VarSlot, iter: ast::Iter) -> SlotIdx {
        self.local.frame.layout().find_iter_slot(var.slot, iter)
    }

    fn find_iter_var(&self, var: &VarSlot, iter: ast::Iter) -> VarSlot {
        self.local.frame.layout().find_iter_var(var, iter)
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
            _ => Err(Error::undefined(EntityKind::DefinedType, id.node.clone(), id.span.clone())),
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
            return Err(Error::duplicate(EntityKind::Type, id.node, id.span));
        }
        self.local.tdenv.insert(id, typdef);
        Ok(())
    }

    // - Values

    fn add_value(&mut self, slot: SlotIdx, value: Value) {
        self.local.frame.set(slot, value);
    }

    // - Functions

    fn add_func(&mut self, id: ast::Id, func: Rc<Callable<F>>) -> Result<(), Error> {
        if self.find_func_opt(&id).is_some() {
            return Err(Error::duplicate(EntityKind::Function, id.node, id.span));
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
        arena: &'a ValueArena,
        vars: &[ast::Var],
    ) -> Result<Vec<&'a [Value]>, Error> {
        let mut values_by_var = Vec::with_capacity(vars.len());
        for var in vars {
            // Every variable must be bound
            let value = self.find_value(var.slot).ok_or_else(|| {
                Error::undefined(
                    EntityKind::Value,
                    Print::to_string(&var.var),
                    var.var.id.span.clone(),
                )
            })?;
            // Each variable must hold a list
            let values = get::list(arena, value)
                .map_err(|error| Error::from(error).at_if_missing(&var.var.id.span))?;
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
                return Err(Error::new(
                    ErrorKind::Context(ContextErrorKind::IterationLengthMismatch {
                        expected: len,
                        actual: values.len(),
                    }),
                    Span::default(),
                ));
            }
        }
        Ok(values_by_var)
    }

    fn find_opt_values_by_var(
        &self,
        arena: &ValueArena,
        vars: &[ast::Var],
    ) -> Result<Option<Vec<Value>>, Error> {
        let mut values = Vec::with_capacity(vars.len());
        for var in vars {
            // Every variable must be bound
            let value = self.find_value(var.slot).ok_or_else(|| {
                Error::undefined(
                    EntityKind::Value,
                    Print::to_string(&var.var),
                    var.var.id.span.clone(),
                )
            })?;
            // Each variable must hold an option
            let value = get::opt(arena, value)
                .map_err(|error| Error::from(error).at_if_missing(&var.var.id.span))?;
            values.push(value);
        }
        // All present, all absent, or a mismatch
        if values.iter().all(|value| value.is_some()) {
            Ok(Some(values.into_iter().flatten().collect()))
        } else if values.iter().all(|value| value.is_none()) {
            Ok(None)
        } else {
            Err(Error::new(
                ErrorKind::Context(ContextErrorKind::OptionalityMismatch),
                Span::default(),
            ))
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
            values.push(*unwrap_from_result!(
                self.find_value(var.slot).ok_or_else(|| {
                    Error::undefined(
                        EntityKind::Value,
                        Print::to_string(&var.var),
                        var.var.id.span.clone(),
                    )
                }),
                &var.var.id.span
            ));
        }
        ok!(())
    }

    // == Adders

    // - Values

    fn bind_list_values_by_var(
        &mut self,
        arena: &mut ValueArena,
        vars: &[ast::Var],
        values_by_var: Vec<Vec<Value>>,
    ) -> Backtrack<()> {
        for (var, values) in vars.iter().zip(values_by_var) {
            let typ = typ::make::iterate(var.var.typ.clone(), &var.var.iters);
            // Each variable becomes a list one iteration outward
            let value = make::list(arena, typ.node.into(), values, Span::default());
            let value = unwrap_from_result!(value, &Span::default());
            self.add_value(var.slot, value);
        }
        ok!(())
    }

    fn bind_opt_values_by_var(
        &mut self,
        arena: &mut ValueArena,
        vars: &[ast::Var],
        values_by_var: Vec<Vec<Value>>,
    ) -> Backtrack<()> {
        for (var, values) in vars.iter().zip(values_by_var) {
            let typ = typ::make::iterate(var.var.typ.clone(), &var.var.iters);
            // Each variable becomes an option one iteration outward
            let value =
                make::opt(arena, typ.node.into(), values.into_iter().next(), Span::default());
            let value = unwrap_from_result!(value, &Span::default());
            self.add_value(var.slot, value);
        }
        ok!(())
    }
}
