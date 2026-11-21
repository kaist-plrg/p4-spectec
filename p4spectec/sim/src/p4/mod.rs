//! The P4-level vocabulary the architectures are written in
//!
//! `data` has `P4Value`, a P4 runtime value as a plain Rust enum,
//! and the opaque `P4Type`.
//! `P4Ops` for types and values and `P4Vars` for variables
//! are the operations on them, defined in `spec::ops`.
//! `env` has the environments an extern runs in:
//! `ExternEnv` for a function or method call, with the `CallResult` it returns,
//! `CompileTimeEnv` for a compile-time call, and `InitEnv` for a constructor call.
//! `pipe` has `Sim`, the pipeline a scheduler drives,
//! and the control-plane operations, `RuntimeInterface`.
//! `spec` does the work that reads or builds a specification value.

mod data;
mod env;
mod pipe;

pub(crate) use crate::spec::ops::{P4Ops, P4Vars};
pub(crate) use data::{P4Fields, P4Type, P4Value, SpecValue};
pub(crate) use env::{
    CallResult, CompileTimeEnv, ExternCall, ExternEnv, InitEnv, MethodCall, Signature,
};
pub(crate) use pipe::{
    ActionCall, Architecture, BlockResult, EntryStatus, HostTypDef, MatchKey, MatchKeyParam,
    MirroringSessionConfig, Radix, RelArg, RuntimeInterface, Sim, TableEntry, TableKey,
    init_program, runtime_unsupported,
};
