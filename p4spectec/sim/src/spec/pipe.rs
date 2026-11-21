//! An architecture's relations and state, against the specification
//!
//! The `Sim` methods here are the ones that read or build specification values:
//! `sim.run_block("V1Model_ingress")` passes the evaluation context
//! and the architecture state to the relation and unpacks the ones it returns.
//! The Rust state of an architecture or an extern object is
//! an extern syntax value of a `HostTypDef`;
//! `get_arch_state` and `get_object_by_id` read one, and `arch_state_to_spec`
//! and `object_to_spec` build one.
//! `init_program` gives the state before the first packet.

use std::{fmt, hash::Hash};

use p4spectec::lang::{
    common::source::Span,
    data::{typ, value::flat as value},
};

use p4spectec::runner::ExternError;

/// The definition of an extern syntax type, such as `archState`,
/// whose values carry a Rust payload.
pub use p4spectec::lang::data::value::host::HostTypDef;

use crate::{
    arch::state::SimState,
    error,
    p4::{Architecture, BlockResult, RelArg, Sim},
};

use super::{
    SpecValue,
    bridge::{FromSpec, ToSpec},
    call::func,
    host::{ArchHost, SpecHost},
};

// == Programs

/// Instantiates `program` with the relation `rel`, such as `V1Model_init`,
/// giving the state before the first packet.
pub fn init_program<Arch: Architecture>(
    host: &mut dyn ArchHost<Arch>,
    rel: &str,
    program: SpecValue,
) -> Result<SimState, ExternError> {
    let values = host.call_program(rel, program.0).result?;
    let (value_ctx, value_arch) = value::get::two(&values)?;
    Ok(SimState::new(SpecValue(*value_ctx), SpecValue(*value_arch)))
}

impl<Arch: Architecture> Sim<'_, Arch> {
    // - Relations

    /// Runs a parser or control block by its relation, such as `V1Model_ingress`.
    pub fn run_block(&mut self, rel: &str) -> Result<BlockResult, ExternError> {
        let values = self
            .host
            .call_rel(rel, &[self.state.value_ctx.0, self.state.value_arch.0])
            .result?;
        let (value_ctx, value_arch, value_result) = value::get::three(&values)?;
        self.state.value_ctx = SpecValue(*value_ctx);
        self.state.value_arch = SpecValue(*value_arch);
        BlockResult::from_spec(self.host.arena_mut(), value_result)
    }

    /// Runs an initialization relation that sets up variables,
    /// such as `V1Model_init_globals`, with `args` after the context and the state;
    /// the context it returns becomes the packet's.
    pub fn run_init(&mut self, rel: &str, args: Vec<RelArg>) -> Result<(), ExternError> {
        let mut values = vec![self.state.value_ctx.0, self.state.value_arch.0];
        for arg in args {
            values.push(arg.to_spec(self.host.arena_mut())?);
        }
        let values = self.host.call_rel(rel, &values).result?;
        self.state.value_ctx = SpecValue(*value::get::one(&values)?);
        Ok(())
    }

    /// Installs a packet extern object, `packet_in` or `packet_out`, with its relation,
    /// such as `V1Model_init_packet_in`; the program then reaches it by that name.
    pub fn run_init_object(&mut self, rel: &str, object: Arch::Object) -> Result<(), ExternError> {
        let value_object = object_to_spec(self.host, Some(object))?;
        let values = self
            .host
            .call_rel(rel, &[self.state.value_ctx.0, self.state.value_arch.0, value_object.0])
            .result?;
        let (value_ctx, value_arch) = value::get::two(&values)?;
        self.state.value_ctx = SpecValue(*value_ctx);
        self.state.value_arch = SpecValue(*value_arch);
        Ok(())
    }

    // - Architecture state

    /// The architecture state.
    pub fn get_arch_state(&mut self) -> Result<Arch::State, ExternError> {
        let typdef = self.host.external().typdef_state();
        let value_state = func::find_arch_state_e(self.host, self.state.value_arch.0)?;
        Ok(value::get::external(self.host.arena(), typdef, &value_state)?.clone())
    }

    /// Replaces the architecture state.
    pub fn set_arch_state(&mut self, state: &Arch::State) -> Result<(), ExternError> {
        let value_state = arch_state_to_spec(self.host, state)?;
        let value_arch =
            func::update_arch_state_e(self.host, self.state.value_arch.0, value_state.0)?;
        self.state.value_arch = SpecValue(value_arch);
        Ok(())
    }

    // - Extern objects

    /// The object id of the dotted path `name`: `[packet_in]`, or `[ip, ig, reg]`.
    pub(crate) fn object_id(&mut self, name: &str) -> Result<SpecValue, ExternError> {
        let arena = self.host.arena_mut();
        let values_name = name
            .split('.')
            .map(|id| id.to_spec(arena))
            .collect::<Result<Vec<_>, _>>()?;
        let typ_id = typ::make::list(typ::make::var(
            p4spectec::phrase!(node: "id".into(), span: Span::default()),
            Vec::new(),
        ));
        let value = value::make::list(arena, typ_id.node.into(), values_name, Span::default())?;
        Ok(SpecValue(value))
    }

    /// The state of the extern object with the specification id `value_id`;
    /// a stateless object is an error.
    pub(crate) fn get_object_by_id(
        &mut self,
        value_id: SpecValue,
    ) -> Result<Arch::Object, ExternError> {
        let typdef = self.host.external().typdef_object();
        let value_object =
            func::find_object_state_e(self.host, self.state.value_arch.0, value_id.0)?;
        value::get::external(self.host.arena(), typdef, &value_object)?
            .clone()
            .ok_or_else(|| {
                error::extern_object_undefined("extern object has no state".to_owned()).into()
            })
    }

    /// Replaces the state of the extern object with the specification id `value_id`.
    pub(crate) fn set_object_by_id(
        &mut self,
        value_id: SpecValue,
        object: Arch::Object,
    ) -> Result<(), ExternError> {
        let value_object = object_to_spec(self.host, Some(object))?;
        let value_arch = func::update_object_state_e(
            self.host,
            self.state.value_arch.0,
            value_id.0,
            value_object.0,
        )?;
        self.state.value_arch = SpecValue(value_arch);
        Ok(())
    }
}

// == Host values

/// The `archState` value holding `state`.
pub fn arch_state_to_spec<Arch: Architecture>(
    host: &mut dyn ArchHost<Arch>,
    state: &Arch::State,
) -> Result<SpecValue, ExternError> {
    let typdef = host.external().typdef_state();
    host_value_to_spec(host, typdef, state.clone())
}

/// The `objectState` value holding `object`; a stateless object holds `None`.
pub fn object_to_spec<Arch: Architecture>(
    host: &mut dyn ArchHost<Arch>,
    object: Option<Arch::Object>,
) -> Result<SpecValue, ExternError> {
    let typdef = host.external().typdef_object();
    host_value_to_spec(host, typdef, object)
}

/// The extern syntax value of `typdef` holding `data`.
pub fn host_value_to_spec<T: Ord + Hash + fmt::Debug + 'static>(
    host: &mut dyn SpecHost,
    typdef: HostTypDef<T>,
    data: T,
) -> Result<SpecValue, ExternError> {
    let value = value::make::external(host.arena_mut(), typdef.payload(data), Span::default())?;
    Ok(SpecValue(value))
}
