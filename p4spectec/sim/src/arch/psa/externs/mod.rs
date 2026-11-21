//! How a specification call reaches the externs of `psa.p4`
//!
//! `impl Architecture for Psa` is here, with each extern hook:
//! `init_object` constructs an object,
//! `call_function` runs `verify`, the only function supported,
//! and `call_method` runs a method on an object from `object`.

use p4spectec::runner::ExternError;

use crate::{
    arch::{io::Rx, state::SimState},
    p4::{Architecture, CallResult, HostTypDef, InitEnv, MethodCall, Signature, Sim, SpecValue},
    spec::host::ArchHost,
};

use crate::arch::core::func as core_func;

use super::{ObjectState, Psa, target, target::state::ArchState};

use self::object::{Counter, HashExtern, InternetChecksum, Meter, Register};

pub(super) mod object;

// == Architecture

impl Architecture for Psa {
    const NAME: &'static str = "psa";

    type State = ArchState;
    type Object = ObjectState;

    fn typdef_state(&self) -> HostTypDef<ArchState> {
        self.typdef_state
    }

    fn typdef_object(&self) -> HostTypDef<Option<ObjectState>> {
        self.typdef_object
    }

    // - Initialization

    /// Constructs a PSA object from its constructor call.
    ///
    /// Core objects and unknown names get an empty state.
    fn init_object(name: &str, env: &mut InitEnv) -> Result<Option<ObjectState>, ExternError> {
        let object = match name {
            "Counter" => Some(ObjectState::Counter(Counter::init(env)?)),
            "Register" => Some(ObjectState::Register(Register::init(env)?)),
            "Hash" => Some(ObjectState::Hash(HashExtern::init(env)?)),
            "InternetChecksum" => Some(ObjectState::InternetChecksum(InternetChecksum::init())),
            "Meter" => Some(ObjectState::Meter(Meter::init(env)?)),
            // Other externs carry no state of their own
            _ => None,
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
        // Anything but `verify` is unsupported
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
            ObjectState::PacketOut(object) => {
                sim.run_extern(call, |env| object.call(env, name, &params))?
            }
            ObjectState::Counter(object) => {
                sim.run_extern(call, |env| object.call(env, name, &params))?
            }
            ObjectState::Register(object) => {
                sim.run_extern(call, |env| object.call(env, name, &params))?
            }
            ObjectState::Hash(object) => {
                sim.run_extern(call, |env| object.call(env, name, &params))?
            }
            ObjectState::InternetChecksum(object) => {
                sim.run_extern(call, |env| object.call(env, name, &params))?
            }
            ObjectState::Meter(object) => {
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
