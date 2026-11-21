//! How a specification call reaches the externs of `v1model.p4`
//!
//! `impl Architecture for V1Model` is here, with each extern hook:
//! `init_object` constructs an object,
//! `call_function` runs a function from `func` by name and parameter names,
//! and `call_method` runs a method on an object from `object`.

use p4spectec::runner::ExternError;

use crate::{
    arch::{io::Rx, state::SimState},
    p4::{Architecture, CallResult, HostTypDef, InitEnv, MethodCall, Signature, Sim, SpecValue},
    spec::host::ArchHost,
};

use crate::arch::core::func as core_func;

use super::{ObjectState, V1Model, target, target::state::ArchState};

use self::{
    func as v1model_func,
    object::{Counter, DirectCounter, DirectMeter, Register},
};

mod func;
pub(super) mod object;

// == Architecture

impl Architecture for V1Model {
    const NAME: &'static str = "v1model";

    type State = ArchState;
    type Object = ObjectState;

    fn typdef_state(&self) -> HostTypDef<ArchState> {
        self.typdef_state
    }

    fn typdef_object(&self) -> HostTypDef<Option<ObjectState>> {
        self.typdef_object
    }

    // - Initialization

    /// Constructs a v1model object from its constructor call.
    ///
    /// Core objects and unknown names get an empty state.
    fn init_object(name: &str, env: &mut InitEnv) -> Result<Option<ObjectState>, ExternError> {
        let object = match name {
            "counter" => Some(ObjectState::Counter(Counter::init(env)?)),
            "register" => Some(ObjectState::Register(Register::init(env)?)),
            "direct_counter" => Some(ObjectState::DirectCounter(DirectCounter::init(env)?)),
            "direct_meter" => Some(ObjectState::DirectMeter(DirectMeter::init(env)?)),
            // Other externs carry no state of their own
            _ => None,
        };
        Ok(object)
    }

    // - Function calls

    /// Dispatches an extern function call by name and parameter names.
    ///
    /// Clone, resubmit, and recirculate update the architecture state,
    /// read before the call and written back after it.
    fn call_function(
        sim: &mut Sim<Self>,
        signature: &Signature,
    ) -> Result<CallResult, ExternError> {
        let (name, params) = signature.as_strs();
        let call_result = match (name, &params[..]) {
            ("verify", ["check", "toSignal"]) => sim.run_extern(signature, core_func::verify)?,
            ("digest", ["receiver", "data"]) => sim.run_extern(signature, v1model_func::digest)?,
            ("mark_to_drop", ["standard_metadata"]) => {
                sim.run_extern(signature, v1model_func::mark_to_drop)?
            }
            ("verify_checksum", ["condition", "data", "checksum", "algo"]) => {
                sim.run_extern(signature, |env| v1model_func::verify_checksum(env, None))?
            }
            ("update_checksum", ["condition", "data", "checksum", "algo"]) => {
                sim.run_extern(signature, |env| v1model_func::update_checksum(env, None))?
            }
            // Clone, resubmit, and recirculate record a request in the architecture state
            ("clone_preserving_field_list", ["type", "session", "index"]) => {
                sim.run_extern_with_arch(signature, v1model_func::clone_preserving_field_list)?
            }
            ("resubmit_preserving_field_list", ["index"]) => {
                sim.run_extern_with_arch(signature, v1model_func::resubmit_preserving_field_list)?
            }
            ("recirculate_preserving_field_list", ["index"]) => sim
                .run_extern_with_arch(signature, v1model_func::recirculate_preserving_field_list)?,
            ("random", ["result", "lo", "hi"]) => {
                sim.run_extern(signature, v1model_func::random)?
            }
            ("hash", ["result", "algo", "base", "data", "max"]) => {
                sim.run_extern(signature, v1model_func::hash)?
            }
            ("log_msg", ["msg"]) => sim.run_extern(signature, v1model_func::log_msg)?,
            ("log_msg", ["msg", "data"]) => {
                sim.run_extern(signature, v1model_func::log_msg_format)?
            }
            // The payload variants need the packet's unparsed bytes
            ("verify_checksum_with_payload", ["condition", "data", "checksum", "algo"]) => {
                let packet = sim.get_packet_in()?;
                sim.run_extern(signature, |env| v1model_func::verify_checksum(env, Some(&packet)))?
            }
            ("update_checksum_with_payload", ["condition", "data", "checksum", "algo"]) => {
                let packet = sim.get_packet_in()?;
                sim.run_extern(signature, |env| v1model_func::update_checksum(env, Some(&packet)))?
            }
            // Anything else, such as truncate, is unsupported
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
            // Counters see the current packet
            ObjectState::Counter(object) => {
                let packet = sim.get_packet_in()?;
                sim.run_extern(call, |env| object.call(env, name, &params, &packet))?
            }
            ObjectState::Register(object) => {
                sim.run_extern(call, |env| object.call(env, name, &params))?
            }
            ObjectState::DirectCounter(object) => {
                let packet = sim.get_packet_in()?;
                sim.run_extern(call, |env| object.call(env, name, &params, &packet))?
            }
            ObjectState::DirectMeter(object) => {
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
