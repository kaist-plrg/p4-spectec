//! How a specification call reaches the externs of `ebpf_model.p4`
//!
//! `impl Architecture for Ebpf` is here, with each extern hook:
//! `init_object` constructs a `CounterArray`,
//! `call_function` runs `verify`, the only function supported,
//! and `call_method` runs a method on an object.

use p4spectec::runner::ExternError;

use crate::{
    arch::{io::Rx, state::SimState},
    p4::{Architecture, CallResult, HostTypDef, InitEnv, MethodCall, Signature, Sim, SpecValue},
    spec::host::ArchHost,
};

use crate::arch::core::func as core_func;

use super::{Ebpf, ObjectState, target};

use self::object::CounterArray;

pub(super) mod object;

// == Architecture

impl Architecture for Ebpf {
    const NAME: &'static str = "ebpf";

    type State = ();
    type Object = ObjectState;

    fn typdef_state(&self) -> HostTypDef<()> {
        self.typdef_state
    }

    fn typdef_object(&self) -> HostTypDef<Option<ObjectState>> {
        self.typdef_object
    }

    // - Initialization

    /// Constructs a `CounterArray`; any other object gets an empty state.
    fn init_object(name: &str, env: &mut InitEnv) -> Result<Option<ObjectState>, ExternError> {
        // Only `CounterArray` carries state
        let object = if name == "CounterArray" {
            Some(ObjectState::CounterArray(CounterArray::init(env)?))
        } else {
            None
        };
        Ok(object)
    }

    // - Function calls

    /// Dispatches an extern function call; only `verify` is supported.
    fn call_function(
        sim: &mut Sim<Self>,
        signature: &Signature,
    ) -> Result<CallResult, ExternError> {
        let (name, params) = signature.as_strs();
        // Only `verify` is supported
        let call_result = match (name, &params[..]) {
            ("verify", ["check", "toSignal"]) => sim.run_extern(signature, core_func::verify)?,
            _ => sim.run_extern(signature, |env| env.unsupported())?,
        };
        Ok(call_result)
    }

    // - Method calls

    /// Dispatches an extern method call on the object the call names.
    ///
    /// The object is read, updated by its method, and written back.
    fn call_method(sim: &mut Sim<Self>, call: &MethodCall) -> Result<CallResult, ExternError> {
        let mut object = sim.get_object_of(call)?;
        let (name, params) = call.signature.as_strs();
        let call_result = match &mut object {
            ObjectState::PacketIn(object) => {
                sim.run_extern(call, |env| object.call(env, name, &params))?
            }
            ObjectState::CounterArray(object) => {
                sim.run_extern(call, |env| object.call(env, name, &params))?
            }
        };
        // Write the updated object back
        sim.set_object_of(call, object)?;
        Ok(call_result)
    }

    // - Pipeline

    fn init_pipe(
        host: &mut dyn ArchHost<Self>,
        program: SpecValue,
    ) -> Result<SimState, ExternError> {
        target::pipe::init_pipe(host, program)
    }

    fn drive_pipe(sim: &mut Sim<Self>, rx: &Rx) -> Result<(), ExternError> {
        target::pipe::drive_pipe(sim, rx)
    }
}
