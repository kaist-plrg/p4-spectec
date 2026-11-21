//! The environment an extern constructor call runs in
//!
//! `register<bit<32>>(512) my_reg;` reaches the simulator
//! with the type arguments `[bit<32>]` and the argument `size = 512`.
//! `InitEnv` gives them out by position (`get_types`) and by name (`get`, `get_opt`);
//! `spec::ops` reads them when the call starts (`InitEnv::new`),
//! and answers questions about types and values through `P4Ops`,
//! such as the `default_of` the register's elements start as.
//! The arguments are a list the specification hands over, not variables,
//! so `InitEnv` does not implement `P4Vars`.

use p4spectec::runner::ExternError;

use crate::error;

use crate::spec::{SpecValue, host::SpecHost};

use crate::p4::{P4Ops, P4Type, P4Value};

/// An extern constructor call in progress.
pub struct InitEnv<'a> {
    pub(crate) host: &'a mut dyn SpecHost,
    /// The type arguments, in order.
    pub(crate) types: Vec<P4Type>,
    /// The arguments with their parameter names.
    pub(crate) args: Vec<(String, SpecValue)>,
}

impl InitEnv<'_> {
    /// The type arguments, in order.
    pub fn get_types(&self) -> &[P4Type] {
        &self.types
    }

    /// The value of the argument `name`.
    pub fn get(&mut self, name: &str) -> Result<P4Value, ExternError> {
        self.get_opt(name)?.ok_or_else(|| {
            error::function_argument_undefined(format!("argument not found: {name}")).into()
        })
    }
}

impl P4Ops for InitEnv<'_> {
    fn host(&mut self) -> &mut dyn SpecHost {
        self.host
    }
}
