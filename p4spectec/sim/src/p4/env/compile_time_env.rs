//! The environment a compile-time extern call runs in
//!
//! `const bool ok = static_assert(V1MODEL_VERSION > 20180000);`
//! reaches the simulator before any packet,
//! so it has an evaluation context but no architecture state.
//! `CompileTimeEnv` reads the call's arguments (`get_local("check")`, in `spec::ops`);
//! it does not implement `P4Vars`, which needs the architecture state.

use crate::spec::{SpecValue, host::SpecHost};

/// A compile-time extern call in progress.
pub struct CompileTimeEnv<'a> {
    pub(crate) host: &'a mut dyn SpecHost,
    /// The caller's evaluation context.
    pub(crate) value_ctx: SpecValue,
}

impl<'a> CompileTimeEnv<'a> {
    /// Starts a call in the caller's context `value_ctx`.
    pub(crate) fn new(host: &'a mut dyn SpecHost, value_ctx: SpecValue) -> Self {
        Self { host, value_ctx }
    }
}
