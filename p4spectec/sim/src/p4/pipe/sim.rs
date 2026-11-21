//! The pipeline as an architecture's scheduler sees it
//!
//! `Sim` pairs the runner with the `SimState` of one input packet.
//! A scheduler reads and writes P4 globals by path through `P4Vars`, as bmv2 does
//! (`sim.get_global("standard_metadata.egress_spec")`),
//! runs a parser or control block by its specification relation
//! (`run_block`, which ends with a `BlockResult`), and transmits packets.
//! The architecture state and the extern objects are Rust values
//! of the types an architecture names in `Architecture`;
//! `Sim` reads and writes them inside the specification's state
//! (`get_arch_state`, `get_object("packet_in")`).
//!
//! An extern dispatcher gets a `Sim` over the state its call starts in,
//! so it reaches that state the same way,
//! and runs the extern with `run_extern`.
//! The methods that read or build specification values are in `spec`:
//! relations and state in `spec::pipe`, tables in `spec::table`.

use p4spectec::runner::ExternError;

use crate::{
    arch::{io::Tx, state::SimState},
    p4::{CallResult, ExternCall, ExternEnv, MethodCall, P4Ops, P4Value, P4Vars, SpecValue},
    spec::host::{ArchHost, SpecHost},
};

use super::arch::Architecture;

// == Pipeline

/// The runner and the state of the packet being processed.
pub struct Sim<'a, Arch> {
    pub(crate) host: &'a mut dyn ArchHost<Arch>,
    pub(crate) state: &'a mut SimState,
}

impl<'a, Arch: Architecture> Sim<'a, Arch> {
    /// Pairs the runner with `state`.
    pub fn new(host: &'a mut dyn ArchHost<Arch>, state: &'a mut SimState) -> Self {
        Self { host, state }
    }

    // - Context

    /// The evaluation context of the packet being processed.
    pub fn get_ctx(&self) -> SpecValue {
        self.state.value_ctx
    }

    /// Makes `value_ctx` the context of the packet being processed.
    pub fn set_ctx(&mut self, value_ctx: SpecValue) {
        self.state.value_ctx = value_ctx;
    }

    // - Extern calls

    /// Runs `f` as the extern function or method call `call`,
    /// in the current context and architecture state.
    pub fn run_extern<'r>(
        &mut self,
        call: impl Into<ExternCall<'r>>,
        f: impl FnOnce(&mut ExternEnv) -> Result<CallResult, ExternError>,
    ) -> Result<CallResult, ExternError> {
        let value_arch = self.state.value_arch;
        let mut env = ExternEnv::new(self.host, &mut self.state.value_ctx, value_arch, call.into());
        f(&mut env)
    }

    /// Runs `f` as `run_extern` does,
    /// with the architecture state for `f` to update,
    /// such as a clone request that the scheduler acts on later.
    pub fn run_extern_with_arch<'r>(
        &mut self,
        call: impl Into<ExternCall<'r>>,
        f: impl FnOnce(&mut ExternEnv, &mut Arch::State) -> Result<CallResult, ExternError>,
    ) -> Result<CallResult, ExternError> {
        let mut state = self.get_arch_state()?;
        let call_result = self.run_extern(call, |env| f(env, &mut state))?;
        self.set_arch_state(&state)?;
        Ok(call_result)
    }

    // - Extern objects

    /// The state of the extern object at the dotted path `name`, such as `packet_in`
    /// or `ip.ig.reg`.
    pub fn get_object(&mut self, name: &str) -> Result<Arch::Object, ExternError> {
        let value_id = self.object_id(name)?;
        self.get_object_by_id(value_id)
    }

    /// Replaces the state of the extern object at the dotted path `name`.
    pub fn set_object(&mut self, name: &str, object: Arch::Object) -> Result<(), ExternError> {
        let value_id = self.object_id(name)?;
        self.set_object_by_id(value_id, object)
    }

    /// The state of the extern object whose method `call` calls.
    pub fn get_object_of(&mut self, call: &MethodCall) -> Result<Arch::Object, ExternError> {
        self.get_object_by_id(call.value_id)
    }

    /// Replaces the state of the extern object whose method `call` calls.
    pub fn set_object_of(
        &mut self,
        call: &MethodCall,
        object: Arch::Object,
    ) -> Result<(), ExternError> {
        self.set_object_by_id(call.value_id, object)
    }

    /// The extern objects' states, to put back with `restore_objects`.
    pub fn save_objects(&self) -> SavedObjects {
        SavedObjects(self.state.value_arch)
    }

    /// Puts back the extern objects' states saved by `save_objects`,
    /// keeping the current architecture state, such as the packet queue.
    pub fn restore_objects(&mut self, saved: SavedObjects) -> Result<(), ExternError> {
        let state = self.get_arch_state()?;
        self.state.value_arch = saved.0;
        self.set_arch_state(&state)
    }

    // - Output

    /// Sends `packet`, as hex text, out of `port`.
    pub fn transmit(&mut self, port: usize, packet: String) {
        self.state.txs.push(Tx { port, packet });
    }
}

impl<Arch: Architecture> P4Ops for Sim<'_, Arch> {
    fn host(&mut self) -> &mut dyn SpecHost {
        self.host
    }
}

impl<Arch: Architecture> P4Vars for Sim<'_, Arch> {
    fn host_ctx(&mut self) -> (&mut dyn SpecHost, &mut SpecValue) {
        (self.host, &mut self.state.value_ctx)
    }

    fn value_arch(&self) -> SpecValue {
        self.state.value_arch
    }
}

// == Block results

/// How a parser or control block ended.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BlockResult {
    /// The parser accepted, or the control returned.
    Done,
    /// The control executed `exit`.
    Exit,
    /// The parser rejected with the named `error` member.
    Reject(String),
}

// == Relation arguments

/// An argument of an architecture's initialization relation,
/// after the context and the state.
pub enum RelArg {
    /// A specification `int`, such as a port number.
    Int(usize),
    /// A specification `text`, such as a packet path name.
    Text(String),
    /// A P4 value.
    Value(P4Value),
}

// == Saved objects

/// Extern object states saved by `Sim::save_objects`.
pub struct SavedObjects(SpecValue);
