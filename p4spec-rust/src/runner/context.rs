//! Operation-local access to the components assembled by a runner
//!
//! The context splits the runner into independent borrows for one evaluation.
//! An extern receives the same context and can reenter the interpreter
//! after its own shared borrow has been copied into a local reference.

use crate::{
    lang::{
        data::value::{Value, ValueArena},
        il::ast::{Id, Typ},
    },
    runner::{Extern, Interface, Interpreter},
};

// == Runner context

/// Split borrows of a runner's components for one evaluation.
pub struct RunnerContext<'runner, Interp, Iface, Ext>
where
    Interp: Interpreter<Iface, Ext>,
    Iface: Interface,
    Ext: Extern,
{
    arena: &'runner mut ValueArena,
    spec: &'runner Interp::Spec,
    interp: &'runner mut Interp,
    interface: &'runner mut Iface,
    external: &'runner Ext,
}

impl<'runner, Interp, Iface, Ext> RunnerContext<'runner, Interp, Iface, Ext>
where
    Interp: Interpreter<Iface, Ext>,
    Iface: Interface,
    Ext: Extern,
{
    pub(super) fn new(
        arena: &'runner mut ValueArena,
        spec: &'runner Interp::Spec,
        interp: &'runner mut Interp,
        interface: &'runner mut Iface,
        external: &'runner Ext,
    ) -> Self {
        Self { arena, spec, interp, interface, external }
    }

    // - Semantic components

    pub fn spec(&self) -> &'runner Interp::Spec {
        self.spec
    }

    pub fn interp(&self) -> &Interp {
        self.interp
    }

    pub fn interp_mut(&mut self) -> &mut Interp {
        self.interp
    }

    pub fn arena(&self) -> &ValueArena {
        self.arena
    }

    pub fn arena_mut(&mut self) -> &mut ValueArena {
        self.arena
    }

    pub fn external(&self) -> &'runner Ext {
        self.external
    }

    // - Evaluation dispatch

    /// Runs the interpreter's program entry.
    ///
    /// With type guards disabled, the program must match the entry's input type.
    pub fn call_program(
        &mut self,
        name: &str,
        program: Value,
    ) -> Result<Vec<Value>, Interp::Error> {
        Interp::eval_program(self, name, program)
    }

    /// Calls a relation by name through the interpreter.
    ///
    /// With type guards disabled, values must match the declared input types.
    pub fn call_rel(&mut self, name: &str, values: &[Value]) -> Result<Vec<Value>, Interp::Error> {
        Interp::eval_rel(self, name, values)
    }

    /// Calls a function by name through the interpreter.
    ///
    /// With type guards disabled, values and type arguments must match
    /// the declared signature.
    pub fn call_func(
        &mut self,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Result<Value, Interp::Error> {
        Interp::eval_func(self, name, targs, values)
    }

    // - Host dispatch

    /// Calls a builtin; the flag reports an interface state change.
    pub fn call_builtin(
        &mut self,
        id: &Id,
        targs: &[Typ],
        values: &[Value],
    ) -> Result<(Value, bool), Interp::Error> {
        let result = self.interface.call_builtin(self.arena, id, targs, values)?;
        Ok(result)
    }

    /// Calls a host relation; the extern receives this context to reenter.
    pub fn call_extern_rel(
        &mut self,
        name: &str,
        values: &[Value],
    ) -> Result<(Vec<Value>, bool), Interp::Error> {
        // Copy the shared borrow so the extern can take `self` mutably
        let external = self.external;
        external.eval_rel(self, name, values)
    }

    /// Calls a host function; the extern receives this context to reenter.
    pub fn call_extern_func(
        &mut self,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Result<(Value, bool), Interp::Error> {
        // Copy the shared borrow so the extern can take `self` mutably
        let external = self.external;
        external.eval_func(self, name, targs, values)
    }
}
