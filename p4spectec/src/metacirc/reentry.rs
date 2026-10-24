//! Reentry policy for host externs above the base
//!
//! A host extern call runs in a context around an interpreter the tower
//! lends, which answers if the extern calls back into a relation, function,
//! or program entry.

use super::error;
use crate::{
    lang::{data::value::flat::Value, il::ast::Typ},
    runner::{Extern, Interface, Interpreter, InterpreterError, RunnerContext},
};

/// This tower lends none: a host extern that reenters fails with
/// metacirc/reentry-unsupported. Every extern above the base today
/// (Dummy, the meta relay) never reenters.
pub(super) struct NoReentry;

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

#[cfg(test)]
mod tests {
    use super::NoReentry;
    use crate::{
        diagnostic::ReportKind,
        lang::{
            data::{arena::Arena, value::flat::Value},
            il::ast::Typ,
        },
        runner::{
            Eval, Extern, ExternError, Host, Interface, Interpreter, InterpreterError,
            NullInterface, RunnerContext,
        },
    };

    /// An extern that calls back into the interpreter it is given.
    struct Reenter;

    impl Extern for Reenter {
        fn eval_rel<Interp, Iface>(
            ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
            name: &str,
            values: &[Value],
        ) -> Eval<Vec<Value>, ExternError>
        where
            Iface: Interface,
            Interp: Interpreter<Iface, Self>,
        {
            ctx.call_rel(name, values).map_err(ExternError::from)
        }

        fn eval_func<Interp, Iface>(
            ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
            name: &str,
            targs: &[Typ],
            values: &[Value],
        ) -> Eval<Value, ExternError>
        where
            Iface: Interface,
            Interp: Interpreter<Iface, Self>,
        {
            ctx.call_func(name, targs, values)
                .map_err(ExternError::from)
        }

        fn clear(&mut self) {}
    }

    #[test]
    fn reentering_host_extern_is_unsupported() {
        let mut arena = Arena::new();
        let mut host = Host::new(NullInterface, Reenter);
        let eval = host.eval_extern_rel(&mut arena, &mut NoReentry, "Rel", &[]);
        let Err(InterpreterError::Fatal(report)) = eval.result else {
            panic!("a reentering extern must fail");
        };
        let ReportKind::Cause(diagnostic) = &report.kind else {
            panic!("the failure must be a coded cause");
        };
        assert_eq!(diagnostic.code.as_deref(), Some("metacirc/reentry-unsupported"));
    }
}
