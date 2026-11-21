//! The environments an extern runs in
//!
//! `call` has the `Signature` or `MethodCall`
//! the specification hands over.
//! `extern_env` has `ExternEnv`, where one such call runs,
//! and the `CallResult` it returns.
//! `init_env` has `InitEnv`, where one extern constructor call runs.

mod call;
mod compile_time_env;
mod extern_env;
mod init_env;

pub(crate) use call::{ExternCall, MethodCall, Signature};
pub(crate) use compile_time_env::CompileTimeEnv;
pub(crate) use extern_env::{CallResult, ExternEnv};
pub(crate) use init_env::InitEnv;
