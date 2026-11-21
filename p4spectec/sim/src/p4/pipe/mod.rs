//! The pipeline an architecture drives
//!
//! `arch` has `Architecture`: an architecture's state types, externs, and pipeline.
//! `sim` has `Sim`, the pipeline a scheduler drives over one input packet,
//! and `init_program`, which gives the state before the first packet.
//! `runtime` has the control-plane operations, `RuntimeInterface`,
//! and the table entries and sessions they take.

mod arch;
mod runtime;
mod sim;

pub(crate) use arch::{Architecture, HostTypDef};
pub(crate) use runtime::{
    ActionCall, EntryStatus, MatchKey, MatchKeyParam, MirroringSessionConfig, Radix,
    RuntimeInterface, TableEntry, TableKey, runtime_unsupported,
};
pub(crate) use sim::{BlockResult, RelArg, Sim};

pub(crate) use crate::spec::pipe::init_program;
