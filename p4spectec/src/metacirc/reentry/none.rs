//! Reentry policy for host externs above the base
//!
//! A host extern call runs in a context around an interpreter the tower
//! lends, which answers if the extern calls back into a relation, function,
//! or program entry.

use crate::metacirc::error;
use crate::{
    lang::{data::value::flat::Value, il::ast::Typ},
    runner::{Extern, Interface, Interpreter, InterpreterError, RunnerContext},
};

/// This tower lends none: a host extern that reenters fails with
/// metacirc/reentry-unsupported. The meta relay and the Dummy P4 extern
/// never reenter.
pub(in crate::metacirc) struct NoReentry;

impl<Iface: Interface, Ext: Extern> Interpreter<Iface, Ext> for NoReentry {
    type SpecEnv = ();

    fn clear(&mut self) {}

    fn reset(&mut self) {}

    fn eval_program(
        _ctx: &mut RunnerContext<'_, Self, Iface, Ext>,
        _name: &str,
        _program: Value,
    ) -> Result<Vec<Value>, InterpreterError> {
        Err(error::reentry_unsupported().into())
    }

    fn eval_rel(
        _ctx: &mut RunnerContext<'_, Self, Iface, Ext>,
        _name: &str,
        _values: &[Value],
    ) -> Result<Vec<Value>, InterpreterError> {
        Err(error::reentry_unsupported().into())
    }

    fn eval_func(
        _ctx: &mut RunnerContext<'_, Self, Iface, Ext>,
        _name: &str,
        _targs: &[Typ],
        _values: &[Value],
    ) -> Result<Value, InterpreterError> {
        Err(error::reentry_unsupported().into())
    }
}
