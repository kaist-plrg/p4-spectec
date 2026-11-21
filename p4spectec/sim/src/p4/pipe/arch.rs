//! What an architecture is to the simulator
//!
//! `Architecture` is everything an architecture provides, in one trait:
//! the Rust types it keeps in the specification's state
//! (`archState` and `objectState`, each an extern syntax value of a `HostTypDef`),
//! how it constructs extern objects and answers extern calls,
//! and how it sets up its pipeline and drives a packet through it.
//! `spec::entry` calls the extern hooks; `stf` calls the pipeline hooks.
//! `arch_state_to_spec` and `object_to_spec` make the specification's values
//! of an architecture's state and objects.

use std::{fmt, hash::Hash};

use p4spectec::runner::{Extern, ExternError};

use crate::{
    arch::{io::Rx, state::SimState},
    p4::{CallResult, InitEnv, MethodCall, Signature, Sim, SpecValue},
    spec::host::ArchHost,
};

pub use crate::spec::pipe::HostTypDef;

// == Architectures

/// An architecture: its state types, its externs, and its pipeline.
pub trait Architecture: Extern + Sized + 'static {
    /// The name used on the command line and in failure messages.
    const NAME: &'static str;

    /// The architecture state, `archState` in the specification.
    type State: Clone + Default + Ord + Hash + fmt::Debug + 'static;
    /// The state of one extern object, `objectState` in the specification.
    type Object: Clone + Ord + Hash + fmt::Debug + 'static;

    /// The definition of `archState`.
    fn typdef_state(&self) -> HostTypDef<Self::State>;

    /// The definition of `objectState`; a stateless object holds `None`.
    fn typdef_object(&self) -> HostTypDef<Option<Self::Object>>;

    // - Extern calls

    /// The initial state of an extern object of type `name`,
    /// from its constructor call; `None` for an object without state.
    fn init_object(name: &str, env: &mut InitEnv) -> Result<Option<Self::Object>, ExternError>;

    /// Evaluates an extern function call.
    fn call_function(sim: &mut Sim<Self>, signature: &Signature)
    -> Result<CallResult, ExternError>;

    /// Evaluates a method call on an extern object.
    fn call_method(sim: &mut Sim<Self>, call: &MethodCall) -> Result<CallResult, ExternError>;

    // - Pipeline

    /// Initializes the pipeline for a parsed program.
    fn init_pipe(
        host: &mut dyn ArchHost<Self>,
        program: SpecValue,
    ) -> Result<SimState, ExternError>;

    /// Pushes one packet through the pipeline, collecting outputs in the state.
    fn drive_pipe(sim: &mut Sim<Self>, rx: &Rx) -> Result<(), ExternError>;
}
