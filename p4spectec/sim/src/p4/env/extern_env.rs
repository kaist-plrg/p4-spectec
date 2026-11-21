//! The environment an extern call runs in
//!
//! A function call's `Signature` or a `MethodCall` (see `call`)
//! starts the `ExternEnv` the extern runs in, through `Sim::run_extern`.
//! A compile-time call runs in a `CompileTimeEnv` instead,
//! since it has no architecture state.
//! The environment reads and writes variables through `P4Vars`
//! (`get_local("index")`, `set_local("result", ..)`),
//! answers questions about types and values through `P4Ops`
//! (`max_size_in_bits`, `from_bits`),
//! and reads the call's type arguments (`get_type`, in `spec::ops`).
//! An extern returns a `CallResult`: a return, or a parser rejection.

use p4spectec::runner::{ExternError, SideEffect};

use crate::error;

use crate::spec::{SpecValue, host::SpecHost};

use crate::p4::{P4Ops, P4Value, P4Vars};

use super::call::ExternCall;

// == Call results

/// The result of an extern call.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CallResult {
    /// Returns, with the value of a non-void call.
    Return(Option<P4Value>),
    /// Rejects the parser with the named `error` member.
    Reject(String),
}

// == Environment

/// An extern call in progress.
pub struct ExternEnv<'a> {
    pub(crate) host: &'a mut dyn SpecHost,
    /// The caller's evaluation context; `P4Vars` writes update it in place.
    pub(crate) value_ctx: &'a mut SpecValue,
    /// The architecture state.
    value_arch: SpecValue,
    /// The call, for failure messages.
    call: ExternCall<'a>,
}

impl<'a> ExternEnv<'a> {
    /// Starts the call `call` in the context `value_ctx`
    /// and the architecture state `value_arch`.
    pub(in crate::p4) fn new(
        host: &'a mut dyn SpecHost,
        value_ctx: &'a mut SpecValue,
        value_arch: SpecValue,
        call: ExternCall<'a>,
    ) -> Self {
        Self { host, value_ctx, value_arch, call }
    }

    /// Fails because the callee has no implementation.
    pub fn unsupported<T>(&self) -> Result<T, ExternError> {
        let callee = self.call.callee();
        let error = match self.call {
            ExternCall::Function(_) => error::extern_function_unsupported(format!(
                "unsupported extern function call: {callee}"
            )),
            ExternCall::Method(_) => error::extern_method_unsupported(format!(
                "unsupported extern method call: {callee}"
            )),
        };
        Err(error.into())
    }

    /// Marks the call as impure, so the interpreter does not reuse its result:
    /// it printed a log line or drew a random number.
    pub fn mark_impure(&mut self) {
        self.host.record_effect(SideEffect::Impure);
    }
}

impl P4Ops for ExternEnv<'_> {
    fn host(&mut self) -> &mut dyn SpecHost {
        self.host
    }
}

impl P4Vars for ExternEnv<'_> {
    fn host_ctx(&mut self) -> (&mut dyn SpecHost, &mut SpecValue) {
        (self.host, self.value_ctx)
    }

    fn value_arch(&self) -> SpecValue {
        self.value_arch
    }
}
