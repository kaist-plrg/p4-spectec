//! Host diagnostics at interpreter boundaries
//!
//! Failure class is chosen from InterfaceError before conversion.
//! These adapters retain existing host messages;
//! native host-specific diagnostic constructors belong to the host layer.

use super::{Error, diagnostic};
use crate::runner::{ExternError, InterfaceError};

const BUILTIN_FAILED: &str = "runtime/builtin-failed";
const INTERFACE_UNCONFIGURED: &str = "runtime/interface-unconfigured";
const EXTERN_FAILED: &str = "runtime/extern-failed";

impl From<InterfaceError> for Error {
    fn from(error: InterfaceError) -> Self {
        let code = match &error {
            InterfaceError::Builtin(_) => BUILTIN_FAILED,
            InterfaceError::NotConfigured => INTERFACE_UNCONFIGURED,
        };
        Box::new(diagnostic(code, error.to_string(), Vec::new()).into())
    }
}

impl From<ExternError> for Error {
    fn from(error: ExternError) -> Self {
        Box::new(diagnostic(EXTERN_FAILED, error.to_string(), Vec::new()).into())
    }
}
