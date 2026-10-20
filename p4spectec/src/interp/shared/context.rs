//! Loaded definitions, local bindings, and shared context access
//!
//! Stage-specific loaders prepare definitions into `Global<R, F>`.
//! A `Context` borrows those globals and owns persistent local bindings;
//! cloning preserves its scope, while `localize` starts a fresh one.
//! `ReadContext`, `WriteContext`, and `IterContext` serve shared evaluation;
//! `FuncSignature` reads function types from each stage's prepared syntax.

use std::rc::Rc;

use crate::lang::{
    common::{ds::map::FrozenIdMap, source::Span},
    data::{
        arena::Arena,
        frame::{Frame, FrameLayout},
        typ,
        value::flat::{self as value, Value, ValueKind},
        var::{SlotIdx, VarSlot},
    },
};

use crate::lang::il::prepared as ast;

use crate::diagnostic::{Label, Report};

use crate::runtime::envs::interp::shared::{FrozenTDEnv, TDEnv, TypeDef};

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

    /// Finds the prepared slot of `var` under one more iteration.
    fn find_var_slot_iterated(&self, var: &VarSlot, iter: ast::Iter) -> SlotIdx {
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

    /// Finds a global function definition by id.
    fn find_func(&self, id: &ast::Id) -> Result<&Rc<Self::Func>, Error>;
    /// Finds the function value bound to `id`, if `id` is a function parameter of this call.
    fn find_func_param_opt(&self, id: &ast::Id) -> Option<Value>;
    /// Finds the type of a function by id.
    fn find_func_typ(&self, id: &ast::Id) -> Result<ast::FuncTyp, Error>;

    /// Names the global definition a call refers to: a parameter's function value, or `id` itself.
    fn resolve_func_id(&self, arena: &Arena, id: &ast::Id) -> ast::Id {
        match self
            .find_func_param_opt(id)
            .map(|value| arena.value.kind(value.node))
        {
            Some(ValueKind::Func(id_global)) => {
                crate::phrase!(node: Rc::clone(&id_global.node), span: id.span)
            }
            Some(_) => unreachable!("function parameter must hold a function value"),
            None => id.clone(),
        }
    }
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

    /// Binds a function parameter to the function value it receives.
    fn add_func_param(&mut self, id: &ast::Id, value: Value);
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

// = Global definitions

/// Stores type definitions and prepared relation and function definitions.
#[derive(Debug)]
pub struct Global<R, F> {
    tdenv: FrozenTDEnv,
    renv: FrozenIdMap<R>,
    fenv: FrozenIdMap<Rc<F>>,
}

impl<R, F> Global<R, F> {
    // == Constructors

    /// Creates empty environments for a stage-specific loader.
    pub(crate) fn new() -> Self {
        Self { tdenv: FrozenTDEnv::new(), renv: FrozenIdMap::new(), fenv: FrozenIdMap::new() }
    }

    // == Inserters

    // - Types

    /// Inserts a global type, panicking if its name is already defined.
    pub(crate) fn insert_typdef(&mut self, id: ast::Id, typdef: TypeDef) {
        // Elaboration already rejects duplicate type names
        let name = Rc::clone(&id.node);
        let typdef_prev = self.tdenv.insert(id, typdef);
        assert!(typdef_prev.is_none(), "global type definitions must be unique: {name}");
    }

    // - Relations

    /// Inserts a prepared relation, panicking if its name is already defined.
    pub(crate) fn insert_rel(&mut self, id: ast::Id, rel: R)
    where
        R: Clone,
    {
        // Elaboration already rejects duplicate relation names
        let name = Rc::clone(&id.node);
        let rel_prev = self.renv.insert(id, rel);
        assert!(rel_prev.is_none(), "global relation definitions must be unique: {name}");
    }

    // - Functions

    /// Inserts a prepared function, panicking if its name is already defined.
    pub(crate) fn insert_func(&mut self, id: ast::Id, func: F) {
        // Elaboration already rejects duplicate function names
        let name = Rc::clone(&id.node);
        let func_prev = self.fenv.insert(id, Rc::new(func));
        assert!(func_prev.is_none(), "global function definitions must be unique: {name}");
    }

    // == Freezing

    /// Ends loading; later insertions panic.
    pub(crate) fn freeze(&mut self) {
        self.tdenv.freeze();
        self.renv.freeze();
        self.fenv.freeze();
    }
}

// = Local bindings

/// Holds type parameters and values of one call; function arguments are values too.
#[derive(Clone, Debug, Default)]
struct Local {
    /// Type parameters bound to their type arguments.
    tdenv: TDEnv,
    /// Value slots of the current callable.
    frame: Frame,
}

// = Execution context

/// Combines borrowed global definitions with local bindings of one call.
#[derive(Debug)]
pub struct Context<'global, R, F> {
    global: &'global Global<R, F>,
    local: Local,
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
            local: Local { tdenv: TDEnv::new(), frame: Frame::new(Rc::clone(layout)) },
        }
    }

    /// Keeps this call's type parameters and starts a fresh frame with the layout.
    pub fn localize_frame(&self, layout: &Rc<FrameLayout>) -> Self {
        Self {
            global: self.global,
            local: Local { frame: Frame::new(Rc::clone(layout)), ..self.local.clone() },
        }
    }

    // == Finders

    // - Types

    /// Finds a type in either scope or reports the lookup location.
    pub fn find_typdef<'a>(&'a self, id: &ast::Id) -> Result<&'a TypeDef, Error> {
        self.find_typdef_opt(id).ok_or_else(|| {
            let diagnostic =
                error::context::binding_undefined(EntityKind::Type, id.node.to_string())
                    .with_label(Label::primary(&id.span, ""));
            Box::new(Report::from(diagnostic))
        })
    }

    // - Relations

    /// Finds a relation in the global definitions.
    pub fn find_rel_opt(&self, id: &ast::Id) -> Option<&'global R> {
        self.global.renv.get(id)
    }

    /// Finds a global relation or reports the lookup location.
    pub fn find_rel(&self, id: &ast::Id) -> Result<&'global R, Error> {
        self.find_rel_opt(id).ok_or_else(|| {
            let diagnostic =
                error::context::binding_undefined(EntityKind::Relation, id.node.to_string())
                    .with_label(Label::primary(&id.span, ""));
            Box::new(Report::from(diagnostic))
        })
    }

    // == Adders

    // - Types

    /// Binds a type locally; the id must be new in both scopes.
    pub fn add_typdef(&mut self, id: ast::Id, typdef: TypeDef) -> Result<(), Error> {
        if self.find_typdef_opt(&id).is_some() {
            let diagnostic =
                error::context::binding_repeated(EntityKind::Type, id.node.to_string())
                    .with_label(Label::primary(&id.span, ""));
            return Err(Box::new(Report::from(diagnostic)));
        }
        self.local.tdenv.insert(id, typdef);
        Ok(())
    }
}

// = Read access

impl<R, F: FuncSignature> ReadContext for Context<'_, R, F> {
    type Func = F;

    // == Finders

    // - Values

    fn find_value_at_slot(&self, slot: SlotIdx) -> Option<&Value> {
        self.local.frame.get(slot)
    }

    fn find_var_slot_iterated(&self, var: &VarSlot, iter: ast::Iter) -> SlotIdx {
        self.local
            .frame
            .layout()
            .find_var_slot_iterated(var.slot, iter)
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
                    error::context::binding_undefined(EntityKind::DefinedType, id.node.to_string())
                        .with_label(Label::primary(&id.span, ""));
                Err(Box::new(Report::from(diagnostic)))
            }
        }
    }

    // - Functions

    fn find_func(&self, id: &ast::Id) -> Result<&Rc<Self::Func>, Error> {
        self.global.fenv.get(id).ok_or_else(|| {
            let diagnostic =
                error::context::binding_undefined(EntityKind::Function, id.node.to_string())
                    .with_label(Label::primary(&id.span, ""));
            Box::new(Report::from(diagnostic))
        })
    }

    fn find_func_param_opt(&self, id: &ast::Id) -> Option<Value> {
        let slot = self.local.frame.layout().find_func_slot(id)?;
        // A callable binds its function parameters before running its body
        let value = self
            .local
            .frame
            .get(slot)
            .expect("function parameter must be bound");
        Some(*value)
    }

    fn find_func_typ(&self, id: &ast::Id) -> Result<ast::FuncTyp, Error> {
        Ok(self.find_func(id)?.func_typ())
    }
}

// = Write access

impl<R, F: FuncSignature> WriteContext for Context<'_, R, F> {
    // == Adders

    // - Types

    fn add_typdef_local(&mut self, id: ast::Id, typdef: TypeDef) -> Result<(), Error> {
        // A type parameter may shadow a global definition
        if self.local.tdenv.contains_key(&id) {
            let diagnostic =
                error::context::binding_repeated(EntityKind::Type, id.node.to_string())
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

    fn add_func_param(&mut self, id: &ast::Id, value: Value) {
        let slot = self
            .local
            .frame
            .layout()
            .find_func_slot(id)
            .expect("function parameter must have a prepared slot");
        self.local.frame.set(slot, value);
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
