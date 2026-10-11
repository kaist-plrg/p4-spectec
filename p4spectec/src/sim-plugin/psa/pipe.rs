//! PSA has a parser, control, and deparser for both ingress and egress
//!
//! ```text
//! Rx -> Ingress parser -> Ingress control -> Ingress deparser -> PRE
//!                                                               |
//!                                                               v
//!     Egress parser <- queued packet <--------------------------+
//!           |
//!           v
//!     Egress control -> Egress deparser -> BQE -> Tx
//! ```
//!
//! The packet replication engine (PRE) sends unicast, multicast, and ingress
//! clones to egress, or resubmits to ingress
//! The buffering queueing engine (BQE) sends egress clones to egress, or
//! recirculates to ingress
//!
//! Both engines can drop the current packet after scheduling its clones
//! The scheduler runs queued packets until none remain

use num_bigint::BigInt;
use serde_derive_state::{DeserializeState, SerializeState};

use crate::lang::{
    common::source::Span,
    data::{
        arena::Arena,
        encoding::Encoding,
        typ,
        value::{
            ValueError,
            flat::{
                self as value, Value,
                external::{DecodeContext, EncodeContext, decode_with, encode_with},
            },
        },
    },
};

use crate::runner::{ExternError, Interface, Interpreter, RunnerContext};

use crate::stf::ast::Statement;

use crate::sim_plugin::error;

use super::super::{
    core::{
        func as core_func,
        object::{PacketIn, PacketOut, packet},
    },
    io::{Rx, Tx},
    spec::{func, pack, pgm, rel, unpack},
    state::SimState,
};

use super::{
    arch::Arch,
    object::{Counter, HashExtern, InternetChecksum, Meter, Register},
    packet::{Entrypoint, Packet},
};

// == Configuration

#[derive(Default)]
/// The PSA architecture, parameterized by its state encoding.
pub struct Psa {
    /// Encoding of architecture and object states as external values.
    encoding: Encoding,
}

impl Psa {
    /// Creates the architecture with the given state encoding.
    pub fn new(encoding: Encoding) -> Self {
        Self { encoding }
    }
}

// == Extern objects

/// Core and PSA-specific extern objects.
#[derive(Clone, Debug, PartialEq, Eq, SerializeState, DeserializeState)]
#[serde(serialize_state = "EncodeContext<'arena>", ser_parameters = "'arena")]
#[serde(deserialize_state = "DecodeContext<'de>")]
pub enum ObjectState {
    /// A `packet_in` of the current pipeline.
    PacketIn(PacketIn),
    /// A `packet_out` being emitted.
    PacketOut(PacketOut),
    /// A `Counter` array.
    Counter(Counter),
    /// A `Register` array.
    Register(#[serde(state)] Register),
    /// A `Hash`.
    Hash(HashExtern),
    /// An `InternetChecksum`.
    InternetChecksum(InternetChecksum),
    /// A `Meter`.
    Meter(Meter),
}

impl ObjectState {
    // - Encoding

    /// Encodes the object as the specification's `objectState` external value.
    pub fn to_value(&self, arena: &mut Arena, encoding: Encoding) -> Result<Value, ExternError> {
        let payload = encode_with(arena, encoding, self)?;
        let typ = typ::make::var(
            crate::phrase!(node: "objectState".to_owned(), span: Span::default()),
            Vec::new(),
        );
        Ok(value::make::external(arena, typ.node.into(), payload.into(), Span::default())?)
    }

    // - Decoding

    /// Decodes an object from an `objectState` external value.
    pub fn from_value(
        arena: &mut Arena,
        encoding: Encoding,
        value: &Value,
    ) -> Result<Self, ExternError> {
        let json = value::get::external(arena, value)?.clone();
        decode_with(arena, encoding, json.as_ref()).map_err(ExternError::from)
    }
}

// == STF transformation

/// Rewrites p4c register-STF block names to the specification's `ip.ig`.
pub fn transform_stf_stmt(mut stmt: Statement) -> Statement {
    match &mut stmt {
        Statement::RegisterRead { name, .. }
        | Statement::RegisterWrite { name, .. }
        | Statement::RegisterReset { name } => {
            *name = name.clone().rewrite_substring(&["ingress"], "ip.ig");
        }
        _ => {}
    }
    stmt
}

// == Architectural state

/// The initial architecture state: empty queue, tables, and groups.
pub(super) fn init_arch_state<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let encoding = ctx.external().encoding;
    Arch::default().to_value(ctx.arena_mut(), encoding)
}

/// Decodes the architecture state stored in `value_arch`.
pub fn find_arch_state<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    value_arch: Value,
) -> Result<Arch, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let encoding = ctx.external().encoding;
    let value_state = func::find_arch_state_e(ctx, value_arch)?;
    Arch::from_value(ctx.arena_mut(), encoding, &value_state)
}

/// Encodes `arch` back into `value_arch`.
pub fn update_arch_state<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    value_arch: Value,
    arch: &Arch,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let encoding = ctx.external().encoding;
    let value_state = arch.to_value(ctx.arena_mut(), encoding)?;
    func::update_arch_state_e(ctx, value_arch, value_state).map_err(ExternError::from)
}

// == Object state

/// Decodes the object named `value_id` from `value_arch`.
pub fn find_object_state<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    value_arch: Value,
    value_id: Value,
) -> Result<ObjectState, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let encoding = ctx.external().encoding;
    let value_object = func::find_object_state_e(ctx, value_arch, value_id)?;
    ObjectState::from_value(ctx.arena_mut(), encoding, &value_object)
}

/// The ingress `packet_in` object.
fn find_ingress_packet_in<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    value_arch: Value,
) -> Result<PacketIn, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let value_name =
        value::make::text(ctx.arena_mut(), "ingress_packet_in".to_owned(), Span::default())?;
    let values_name = vec![value_name];
    let typ_id = typ::make::list(typ::make::var(
        crate::phrase!(node: "id".to_owned(), span: Span::default()),
        vec![],
    ));
    let value_id =
        value::make::list(ctx.arena_mut(), typ_id.node.into(), values_name, Span::default())?;
    match find_object_state(ctx, value_arch, value_id)? {
        ObjectState::PacketIn(pkt) => Ok(pkt),
        _ => {
            Err(error::extern_object_undefined("ingress_packet_in extern not found".to_owned())
                .into())
        }
    }
}

/// The ingress `packet_out` object.
fn find_ingress_packet_out<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    value_arch: Value,
) -> Result<PacketOut, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let value_name =
        value::make::text(ctx.arena_mut(), "ingress_packet_out".to_owned(), Span::default())?;
    let values_name = vec![value_name];
    let typ_id = typ::make::list(typ::make::var(
        crate::phrase!(node: "id".to_owned(), span: Span::default()),
        vec![],
    ));
    let value_id =
        value::make::list(ctx.arena_mut(), typ_id.node.into(), values_name, Span::default())?;
    match find_object_state(ctx, value_arch, value_id)? {
        ObjectState::PacketOut(pkt) => Ok(pkt),
        _ => {
            Err(error::extern_object_undefined("ingress_packet_out extern not found".to_owned())
                .into())
        }
    }
}

/// The egress `packet_in` object.
fn find_egress_packet_in<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    value_arch: Value,
) -> Result<PacketIn, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let value_name =
        value::make::text(ctx.arena_mut(), "egress_packet_in".to_owned(), Span::default())?;
    let values_name = vec![value_name];
    let typ_id = typ::make::list(typ::make::var(
        crate::phrase!(node: "id".to_owned(), span: Span::default()),
        vec![],
    ));
    let value_id =
        value::make::list(ctx.arena_mut(), typ_id.node.into(), values_name, Span::default())?;
    match find_object_state(ctx, value_arch, value_id)? {
        ObjectState::PacketIn(pkt) => Ok(pkt),
        _ => {
            Err(error::extern_object_undefined("egress_packet_in extern not found".to_owned())
                .into())
        }
    }
}

/// The egress `packet_out` object.
fn find_egress_packet_out<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    value_arch: Value,
) -> Result<PacketOut, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let value_name =
        value::make::text(ctx.arena_mut(), "egress_packet_out".to_owned(), Span::default())?;
    let values_name = vec![value_name];
    let typ_id = typ::make::list(typ::make::var(
        crate::phrase!(node: "id".to_owned(), span: Span::default()),
        vec![],
    ));
    let value_id =
        value::make::list(ctx.arena_mut(), typ_id.node.into(), values_name, Span::default())?;
    match find_object_state(ctx, value_arch, value_id)? {
        ObjectState::PacketOut(pkt) => Ok(pkt),
        _ => {
            Err(error::extern_object_undefined("egress_packet_out extern not found".to_owned())
                .into())
        }
    }
}

/// The `Register` object named `name`, whose id is the dotted path.
fn find_register<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    value_arch: Value,
    name: &str,
) -> Result<Register, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let values_name = name
        .split('.')
        .map(|name| value::make::text(ctx.arena_mut(), name.to_owned(), Span::default()))
        .collect::<Result<Vec<_>, _>>()?;
    let typ_id = typ::make::list(typ::make::var(
        crate::phrase!(node: "id".to_owned(), span: Span::default()),
        vec![],
    ));
    let value_id =
        value::make::list(ctx.arena_mut(), typ_id.node.into(), values_name, Span::default())?;
    match find_object_state(ctx, value_arch, value_id)? {
        ObjectState::Register(reg) => Ok(reg),
        _ => {
            Err(error::extern_object_undefined(format!("Register extern {name} not found")).into())
        }
    }
}

/// Writes the `Register` object named `name` back.
fn update_register<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    value_arch: Value,
    name: &str,
    reg: Register,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let values_name = name
        .split('.')
        .map(|name| value::make::text(ctx.arena_mut(), name.to_owned(), Span::default()))
        .collect::<Result<Vec<_>, _>>()?;
    let typ_id = typ::make::list(typ::make::var(
        crate::phrase!(node: "id".to_owned(), span: Span::default()),
        vec![],
    ));
    let value_id =
        value::make::list(ctx.arena_mut(), typ_id.node.into(), values_name, Span::default())?;
    let encoding = ctx.external().encoding;
    let value_reg = ObjectState::Register(reg).to_value(ctx.arena_mut(), encoding)?;
    func::update_object_state_e(ctx, value_arch, value_id, value_reg).map_err(ExternError::from)
}

// == Extern calls

// - Initialization

/// Constructs a PSA object from its constructor call.
///
/// Core objects and unknown names get an empty state.
pub(super) fn eval_extern_init<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    values: &[Value],
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let encoding = ctx.external().encoding;
    let (value_name, value_targs, value_ids, value_args) = value::get::four(values)?;
    let name = value::get::text(ctx.arena(), value_name)?.to_owned();
    let object = match name.as_str() {
        "Counter" => Some(ObjectState::Counter(Counter::init(
            ctx.arena(),
            *value_targs,
            *value_ids,
            *value_args,
        )?)),
        "Register" => {
            Some(ObjectState::Register(Register::init(ctx, *value_targs, *value_ids, *value_args)?))
        }
        "Hash" => Some(ObjectState::Hash(HashExtern::init(
            ctx.arena(),
            *value_targs,
            *value_ids,
            *value_args,
        )?)),
        "InternetChecksum" => Some(ObjectState::InternetChecksum(InternetChecksum::init())),
        "Meter" => Some(ObjectState::Meter(Meter::init(
            ctx.arena(),
            *value_targs,
            *value_ids,
            *value_args,
        )?)),
        // Other externs carry no state of their own
        _ => None,
    };
    Ok(match object {
        Some(object) => object.to_value(ctx.arena_mut(), encoding)?,
        // No state: encode the unit value
        None => {
            let payload = encode_with(ctx.arena(), encoding, &())?;
            let typ = typ::make::var(
                crate::phrase!(node: "objectState".to_owned(), span: Span::default()),
                Vec::new(),
            );
            value::make::external(
                ctx.arena_mut(),
                typ.node.into(),
                payload.into(),
                Span::default(),
            )?
        }
    })
}

// - Function calls

/// Dispatches an extern function call; only `verify` is supported.
pub(super) fn eval_extern_func_call<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    values: &[Value],
) -> Result<Vec<Value>, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let (value_ctx, value_arch, value_name, value_names) = value::get::four(values)?;
    let name = value::get::text(ctx.arena(), value_name)?.to_owned();
    let names = value::get::list(ctx.arena(), value_names)?
        .iter()
        .map(|value| value::get::text(ctx.arena(), value).map(str::to_owned))
        .collect::<Result<Vec<_>, _>>()?;
    // Anything but `verify` is unsupported
    if name != "verify" || names != ["check", "toSignal"] {
        return Err(error::extern_function_unsupported(format!(
            "unsupported extern function call: {name}({})",
            names.join(", ")
        ))
        .into());
    }
    let (value_ctx, value_arch, value_call_result) =
        core_func::verify(ctx, *value_ctx, *value_arch)?;
    Ok(vec![value_ctx, value_arch, value_call_result])
}

// - Method calls

/// Dispatches an extern method call on the object named `value_id`.
///
/// The object is decoded, updated by its method, and written back.
pub(super) fn eval_extern_method_call<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    values: &[Value],
) -> Result<Vec<Value>, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let encoding = ctx.external().encoding;
    // Context, state, object id, method name, parameter names
    let [value_ctx, value_arch, value_id, value_name, value_names] = values else {
        return Err(error::extern_argument_arity_mismatch(
            "unexpected number of arguments to extern method call".to_owned(),
        )
        .into());
    };
    let object = find_object_state(ctx, *value_arch, *value_id)?;
    let name = value::get::text(ctx.arena(), value_name)?.to_owned();
    let names = value::get::list(ctx.arena(), value_names)?
        .iter()
        .map(|value| value::get::text(ctx.arena(), value).map(str::to_owned))
        .collect::<Result<Vec<_>, _>>()?;
    let names_ref: Vec<_> = names.iter().map(String::as_str).collect();
    // Each arm hands the object to its method and wraps it again
    let (object, value_ctx, value_arch, value_call_result) =
        match (object, name.as_str(), names_ref.as_slice()) {
            (ObjectState::PacketIn(object), "extract", ["hdr"]) => {
                let (object, value_ctx, value_arch, value_call_result) =
                    object.extract(ctx, *value_ctx, *value_arch)?;
                (ObjectState::PacketIn(object), value_ctx, value_arch, value_call_result)
            }
            (
                ObjectState::PacketIn(object),
                "extract",
                ["variableSizeHeader", "variableFieldSizeInBits"],
            ) => {
                let (object, value_ctx, value_arch, value_call_result) =
                    object.extract_varsize(ctx, *value_ctx, *value_arch)?;
                (ObjectState::PacketIn(object), value_ctx, value_arch, value_call_result)
            }
            (ObjectState::PacketIn(object), "lookahead", []) => {
                let (object, value_ctx, value_arch, value_call_result) =
                    object.lookahead(ctx, *value_ctx, *value_arch)?;
                (ObjectState::PacketIn(object), value_ctx, value_arch, value_call_result)
            }
            (ObjectState::PacketIn(object), "advance", ["sizeInBits"]) => {
                let (object, value_ctx, value_arch, value_call_result) =
                    object.advance(ctx, *value_ctx, *value_arch)?;
                (ObjectState::PacketIn(object), value_ctx, value_arch, value_call_result)
            }
            (ObjectState::PacketIn(object), "length", []) => {
                let (object, value_ctx, value_arch, value_call_result) =
                    object.length(ctx, *value_ctx, *value_arch)?;
                (ObjectState::PacketIn(object), value_ctx, value_arch, value_call_result)
            }
            (ObjectState::PacketOut(object), "emit", ["hdr"]) => {
                let (object, value_ctx, value_arch, value_call_result) =
                    object.emit(ctx, *value_ctx, *value_arch)?;
                (ObjectState::PacketOut(object), value_ctx, value_arch, value_call_result)
            }
            (ObjectState::Counter(object), "count", ["index"]) => {
                let (object, value_ctx, value_arch, value_call_result) =
                    object.count(ctx, *value_ctx, *value_arch)?;
                (ObjectState::Counter(object), value_ctx, value_arch, value_call_result)
            }
            (ObjectState::Register(object), "read", ["index"]) => {
                let (object, value_ctx, value_arch, value_call_result) =
                    object.read(ctx, *value_ctx, *value_arch)?;
                (ObjectState::Register(object), value_ctx, value_arch, value_call_result)
            }
            (ObjectState::Register(object), "write", ["index", "value"]) => {
                let (object, value_ctx, value_arch, value_call_result) =
                    object.write(ctx, *value_ctx, *value_arch)?;
                (ObjectState::Register(object), value_ctx, value_arch, value_call_result)
            }
            (ObjectState::Hash(object), "get_hash", ["data"]) => {
                let (object, value_ctx, value_arch, value_call_result) =
                    object.get_hash(ctx, *value_ctx, *value_arch)?;
                (ObjectState::Hash(object), value_ctx, value_arch, value_call_result)
            }
            (ObjectState::Hash(object), "get_hash", ["base", "data", "max"]) => {
                let (object, value_ctx, value_arch, value_call_result) =
                    object.get_hash_adjust(ctx, *value_ctx, *value_arch)?;
                (ObjectState::Hash(object), value_ctx, value_arch, value_call_result)
            }
            (ObjectState::InternetChecksum(object), "clear", []) => {
                let (object, value_ctx, value_arch, value_call_result) =
                    object.clear(ctx, *value_ctx, *value_arch)?;
                (ObjectState::InternetChecksum(object), value_ctx, value_arch, value_call_result)
            }
            (ObjectState::InternetChecksum(object), "add", ["data"]) => {
                let (object, value_ctx, value_arch, value_call_result) =
                    object.add(ctx, *value_ctx, *value_arch)?;
                (ObjectState::InternetChecksum(object), value_ctx, value_arch, value_call_result)
            }
            (ObjectState::InternetChecksum(object), "subtract", ["data"]) => {
                let (object, value_ctx, value_arch, value_call_result) =
                    object.subtract(ctx, *value_ctx, *value_arch)?;
                (ObjectState::InternetChecksum(object), value_ctx, value_arch, value_call_result)
            }
            (ObjectState::InternetChecksum(object), "get", []) => {
                let (object, value_ctx, value_arch, value_call_result) =
                    object.get(ctx, *value_ctx, *value_arch)?;
                (ObjectState::InternetChecksum(object), value_ctx, value_arch, value_call_result)
            }
            (ObjectState::InternetChecksum(object), "get_state", []) => {
                let (object, value_ctx, value_arch, value_call_result) =
                    object.get_state(ctx, *value_ctx, *value_arch)?;
                (ObjectState::InternetChecksum(object), value_ctx, value_arch, value_call_result)
            }
            (ObjectState::InternetChecksum(object), "set_state", ["checksum_state"]) => {
                let (object, value_ctx, value_arch, value_call_result) =
                    object.set_state(ctx, *value_ctx, *value_arch)?;
                (ObjectState::InternetChecksum(object), value_ctx, value_arch, value_call_result)
            }
            (ObjectState::Meter(object), "execute", ["index", "color"]) => {
                let (object, value_ctx, value_arch, value_call_result) =
                    object.execute_color_aware(ctx, *value_ctx, *value_arch)?;
                (ObjectState::Meter(object), value_ctx, value_arch, value_call_result)
            }
            (ObjectState::Meter(object), "execute", ["index"]) => {
                let (object, value_ctx, value_arch, value_call_result) =
                    object.execute_color_blind(ctx, *value_ctx, *value_arch)?;
                (ObjectState::Meter(object), value_ctx, value_arch, value_call_result)
            }
            // Unknown method: name the object in the error
            _ => {
                let ids = value::get::list(ctx.arena(), value_id)?
                    .iter()
                    .map(|value| value::get::text(ctx.arena(), value).map(str::to_owned))
                    .collect::<Result<Vec<_>, _>>()?;
                return Err(error::extern_method_unsupported(format!(
                    "unsupported extern method call: {}.{name}({})",
                    ids.join("."),
                    names.join(", ")
                ))
                .into());
            }
        };
    // Write the updated object back
    let value_object = object.to_value(ctx.arena_mut(), encoding)?;
    let value_arch = func::update_object_state_e(ctx, value_arch, *value_id, value_object)?;
    Ok(vec![value_ctx, value_arch, value_call_result])
}

// == Mirror session interface

/// Maps clone session `session` to multicast group `group`.
pub fn add_mirror_session_mc<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    value_arch: Value,
    session: usize,
    group: usize,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let mut arch = find_arch_state(ctx, value_arch)?;
    arch.mirrortable.insert(session, group);
    update_arch_state(ctx, value_arch, &arch)
}

// == Multicast interface

/// Creates multicast group `group`.
pub fn mc_mgrp_create<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    value_arch: Value,
    group: usize,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let mut arch = find_arch_state(ctx, value_arch)?;
    arch.multicast.group_create(group);
    update_arch_state(ctx, value_arch, &arch)
}

/// Creates a multicast node with instance id `instance` on `ports`.
pub fn mc_node_create<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    value_arch: Value,
    instance: usize,
    ports: &[usize],
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let mut arch = find_arch_state(ctx, value_arch)?;
    arch.multicast.node_create(instance, ports);
    update_arch_state(ctx, value_arch, &arch)
}

/// Adds node `handle` to multicast group `group`.
pub fn mc_node_associate<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    value_arch: Value,
    group: usize,
    handle: usize,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let mut arch = find_arch_state(ctx, value_arch)?;
    arch.multicast.node_associate(group, handle);
    update_arch_state(ctx, value_arch, &arch)
}

// == Register interface

/// Reads register `name` at `idx`; the source simulator prints nothing.
pub fn register_read<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    value_arch: Value,
    name: &str,
    idx: usize,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let reg = find_register(ctx, value_arch, name)?;
    // Evaluate the register read; printing is disabled in the source
    // Out of range: evaluate the default and discard it
    if idx >= reg.values.len() {
        func::default(ctx, reg.value_typ)?;
    }
    Ok(value_arch)
}

/// Writes `int` to register `name` at `idx`; out of range is ignored.
pub fn register_write<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    value_arch: Value,
    name: &str,
    idx: usize,
    int: BigInt,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let mut reg = find_register(ctx, value_arch, name)?;
    let value = pack::p4_arbitrary_int(ctx.arena_mut(), int)?;
    let value = func::cast_op(ctx, reg.value_typ, value)?;
    // Out of range: ignored
    if let Some(value_reg) = reg.values.get_mut(idx) {
        *value_reg = value;
    }
    update_register(ctx, value_arch, name, reg)
}

/// Resets every element of register `name` to the default.
pub fn register_reset<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    value_arch: Value,
    name: &str,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let mut reg = find_register(ctx, value_arch, name)?;
    let value = func::default(ctx, reg.value_typ)?;
    reg.values.fill(value);
    update_register(ctx, value_arch, name, reg)
}

// == Packet state

/// Makes `packet` current under the `packet_in` name of its entrypoint.
fn insert_packet<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
    packet: Packet,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    // Ingress and egress keep separate `packet_in` objects
    let name = match packet.entrypoint {
        Entrypoint::Ingress => "ingress_packet_in",
        Entrypoint::Egress => "egress_packet_in",
    };
    state.value_arch = {
        let value_name = value::make::text(ctx.arena_mut(), name.to_owned(), Span::default())?;
        let values_name = vec![value_name];
        let typ_id = typ::make::list(typ::make::var(
            crate::phrase!(node: "id".to_owned(), span: Span::default()),
            vec![],
        ));
        let value_id =
            value::make::list(ctx.arena_mut(), typ_id.node.into(), values_name, Span::default())?;
        let encoding = ctx.external().encoding;
        let value_object =
            ObjectState::PacketIn(packet.packet_in).to_value(ctx.arena_mut(), encoding)?;
        func::update_object_state_e(ctx, state.value_arch, value_id, value_object)
    }?;
    state.value_ctx = packet.value_ctx;
    Ok(())
}

/// Rewinds the ingress `packet_in` cursor over the same bytes.
fn remove_ingress_packet_in<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let mut pkt = find_ingress_packet_in(ctx, state.value_arch)?;
    pkt.reset();
    state.value_arch = {
        let value_name =
            value::make::text(ctx.arena_mut(), "ingress_packet_in".to_owned(), Span::default())?;
        let values_name = vec![value_name];
        let typ_id = typ::make::list(typ::make::var(
            crate::phrase!(node: "id".to_owned(), span: Span::default()),
            vec![],
        ));
        let value_id =
            value::make::list(ctx.arena_mut(), typ_id.node.into(), values_name, Span::default())?;
        let encoding = ctx.external().encoding;
        let value_object = ObjectState::PacketIn(pkt).to_value(ctx.arena_mut(), encoding)?;
        func::update_object_state_e(ctx, state.value_arch, value_id, value_object)
    }?;
    Ok(())
}

/// Replaces the ingress `packet_out` with an empty one.
fn remove_ingress_packet_out<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    state.value_arch = {
        let value_name =
            value::make::text(ctx.arena_mut(), "ingress_packet_out".to_owned(), Span::default())?;
        let values_name = vec![value_name];
        let typ_id = typ::make::list(typ::make::var(
            crate::phrase!(node: "id".to_owned(), span: Span::default()),
            vec![],
        ));
        let value_id =
            value::make::list(ctx.arena_mut(), typ_id.node.into(), values_name, Span::default())?;
        let encoding = ctx.external().encoding;
        let value_object =
            ObjectState::PacketOut(PacketOut::default()).to_value(ctx.arena_mut(), encoding)?;
        func::update_object_state_e(ctx, state.value_arch, value_id, value_object)
    }?;
    Ok(())
}

/// Replaces the egress `packet_out` with an empty one.
fn remove_egress_packet_out<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    state.value_arch = {
        let value_name =
            value::make::text(ctx.arena_mut(), "egress_packet_out".to_owned(), Span::default())?;
        let values_name = vec![value_name];
        let typ_id = typ::make::list(typ::make::var(
            crate::phrase!(node: "id".to_owned(), span: Span::default()),
            vec![],
        ));
        let value_id =
            value::make::list(ctx.arena_mut(), typ_id.node.into(), values_name, Span::default())?;
        let encoding = ctx.external().encoding;
        let value_object =
            ObjectState::PacketOut(PacketOut::default()).to_value(ctx.arena_mut(), encoding)?;
        func::update_object_state_e(ctx, state.value_arch, value_id, value_object)
    }?;
    Ok(())
}

/// Whether ingress requested a clone.
fn is_ingress_clone<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
) -> Result<bool, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let value = rel::lvalue_read_dot_global(
        ctx,
        state.value_ctx,
        state.value_arch,
        "ingress_output_metadata",
        "clone",
    )?;
    unpack::p4_bool(ctx.arena(), &value)
}

/// Whether ingress requested a drop.
fn is_ingress_drop<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
) -> Result<bool, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let value = rel::lvalue_read_dot_global(
        ctx,
        state.value_ctx,
        state.value_arch,
        "ingress_output_metadata",
        "drop",
    )?;
    unpack::p4_bool(ctx.arena(), &value)
}

/// Whether ingress requested a resubmit.
fn is_ingress_resubmit<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
) -> Result<bool, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let value = rel::lvalue_read_dot_global(
        ctx,
        state.value_ctx,
        state.value_arch,
        "ingress_output_metadata",
        "resubmit",
    )?;
    unpack::p4_bool(ctx.arena(), &value)
}

/// The clone session id requested by ingress.
fn get_ingress_clone_session_id<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
) -> Result<usize, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let value = rel::lvalue_read_dot_global(
        ctx,
        state.value_ctx,
        state.value_arch,
        "ingress_output_metadata",
        "clone_session_id",
    )?;
    Ok(usize::try_from(&unpack::p4_fixed_bit(ctx.arena(), &value)?.1)?)
}

/// The multicast group requested by ingress.
fn get_multicast_group<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
) -> Result<usize, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let value = rel::lvalue_read_dot_global(
        ctx,
        state.value_ctx,
        state.value_arch,
        "ingress_output_metadata",
        "multicast_group",
    )?;
    Ok(usize::try_from(&unpack::p4_fixed_bit(ctx.arena(), &value)?.1)?)
}

/// Whether egress requested a clone.
fn is_egress_clone<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
) -> Result<bool, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let value = rel::lvalue_read_dot_global(
        ctx,
        state.value_ctx,
        state.value_arch,
        "egress_output_metadata",
        "clone",
    )?;
    unpack::p4_bool(ctx.arena(), &value)
}

/// Whether egress requested a drop.
fn is_egress_drop<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
) -> Result<bool, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let value = rel::lvalue_read_dot_global(
        ctx,
        state.value_ctx,
        state.value_arch,
        "egress_output_metadata",
        "drop",
    )?;
    unpack::p4_bool(ctx.arena(), &value)
}

/// Whether the egress port is the recirculation port.
fn is_egress_recirculate<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
) -> Result<bool, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let value_port = rel::lvalue_read_dot_global(
        ctx,
        state.value_ctx,
        state.value_arch,
        "egress_input_metadata",
        "egress_port",
    )?;
    let (width_port, int_port) = unpack::p4_fixed_bit(ctx.arena(), &value_port)?;
    // The recirculation port is the 32-bit value 0xfffffffa
    Ok(width_port == 32.into() && int_port == 0xffff_fffa_u32.into())
}

/// The clone session id requested by egress.
fn get_egress_clone_session_id<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
) -> Result<usize, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let value = rel::lvalue_read_dot_global(
        ctx,
        state.value_ctx,
        state.value_arch,
        "egress_output_metadata",
        "clone_session_id",
    )?;
    Ok(usize::try_from(&unpack::p4_fixed_bit(ctx.arena(), &value)?.1)?)
}

// == Pipeline initializer

/// Instantiates the program and returns the initial simulator state.
pub fn init_pipe<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    program: Value,
) -> Result<SimState, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let (value_ctx, value_arch) = pgm::psa_init(ctx, program)?;
    Ok(SimState { value_ctx, value_arch, txs: vec![] })
}

// == Prepare context

/// Sets up egress metadata for a normal unicast to the chosen port.
fn prepare_unicast_ctx<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let port = {
        let value = rel::lvalue_read_dot_global(
            ctx,
            state.value_ctx,
            state.value_arch,
            "ingress_output_metadata",
            "egress_port",
        )?;
        usize::try_from(&unpack::p4_fixed_bit(ctx.arena(), &value)?.1).map_err(ExternError::from)
    }?;
    let cos = {
        let value = rel::lvalue_read_dot_global(
            ctx,
            state.value_ctx,
            state.value_arch,
            "ingress_output_metadata",
            "class_of_service",
        )?;
        usize::try_from(&unpack::p4_fixed_bit(ctx.arena(), &value)?.1).map_err(ExternError::from)
    }?;
    // Fill egress input metadata for this replica
    state.value_ctx = rel::psa_egress_init_metadata(
        ctx,
        state.value_ctx,
        state.value_arch,
        port,
        "NORMAL_UNICAST",
        cos,
        0,
    )?;
    Ok(())
}

/// Sets up egress metadata for a multicast replica.
fn prepare_multicast_ctx<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
    instance: usize,
    port: usize,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let cos = {
        let value = rel::lvalue_read_dot_global(
            ctx,
            state.value_ctx,
            state.value_arch,
            "ingress_output_metadata",
            "class_of_service",
        )?;
        usize::try_from(&unpack::p4_fixed_bit(ctx.arena(), &value)?.1).map_err(ExternError::from)
    }?;
    // Fill egress input metadata for this replica
    state.value_ctx = rel::psa_egress_init_metadata(
        ctx,
        state.value_ctx,
        state.value_arch,
        port,
        "NORMAL_MULTICAST",
        cos,
        instance,
    )?;
    Ok(())
}

/// Sets up egress metadata for an ingress-to-egress clone.
fn prepare_clone_i2e_ctx<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
    instance: usize,
    port: usize,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let cos = {
        let value = rel::lvalue_read_dot_global(
            ctx,
            state.value_ctx,
            state.value_arch,
            "ingress_output_metadata",
            "class_of_service",
        )?;
        usize::try_from(&unpack::p4_fixed_bit(ctx.arena(), &value)?.1).map_err(ExternError::from)
    }?;
    // Fill egress input metadata for this replica
    state.value_ctx = rel::psa_egress_init_metadata(
        ctx,
        state.value_ctx,
        state.value_arch,
        port,
        "CLONE_I2E",
        cos,
        instance,
    )?;
    Ok(())
}

/// Sets up egress metadata for an egress-to-egress clone.
fn prepare_clone_e2e_ctx<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
    instance: usize,
    port: usize,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let cos = {
        let value = rel::lvalue_read_dot_global(
            ctx,
            state.value_ctx,
            state.value_arch,
            "egress_input_metadata",
            "class_of_service",
        )?;
        usize::try_from(&unpack::p4_fixed_bit(ctx.arena(), &value)?.1).map_err(ExternError::from)
    }?;
    // Fill egress input metadata for this replica
    state.value_ctx = rel::psa_egress_init_metadata(
        ctx,
        state.value_ctx,
        state.value_arch,
        port,
        "CLONE_E2E",
        cos,
        instance,
    )?;
    Ok(())
}

/// Sets up ingress metadata for a resubmit.
fn prepare_resubmit_ctx<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let port = {
        let value = rel::lvalue_read_dot_global(
            ctx,
            state.value_ctx,
            state.value_arch,
            "ingress_input_metadata",
            "ingress_port",
        )?;
        usize::try_from(&unpack::p4_fixed_bit(ctx.arena(), &value)?.1).map_err(ExternError::from)
    }?;
    state.value_ctx =
        rel::psa_ingress_init_metadata(ctx, state.value_ctx, state.value_arch, port, "RESUBMIT")?;
    Ok(())
}

/// Sets up ingress metadata for a recirculation.
fn prepare_recirculate_ctx<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    state.value_ctx = rel::psa_ingress_init_metadata(
        ctx,
        state.value_ctx,
        state.value_arch,
        0xfffffffa,
        "RECIRCULATE",
    )?;
    Ok(())
}

// == Schedule packet

/// Queues the current packet to resume at `entrypoint`.
pub fn schedule_packet<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
    entrypoint: Entrypoint,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    // Ingress and egress read their own `packet_in`
    let packet_in = match entrypoint {
        Entrypoint::Ingress => find_ingress_packet_in(ctx, state.value_arch)?,
        Entrypoint::Egress => find_egress_packet_in(ctx, state.value_arch)?,
    };
    let packet = Packet { value_ctx: state.value_ctx, packet_in, entrypoint };
    let mut arch = find_arch_state(ctx, state.value_arch)?;
    arch.queue.push_back(packet);
    state.value_arch = update_arch_state(ctx, state.value_arch, &arch)?;
    Ok(())
}

/// Deparses the ingress packet and queues one egress copy.
fn schedule_unicast<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let packet = {
        let pkt_in = find_ingress_packet_in(ctx, state.value_arch)?;
        let pkt_out = find_ingress_packet_out(ctx, state.value_arch)?;
        // Serialize the current packet to bytes
        packet::to_string(&pkt_in, &pkt_out)
    }?;
    let pkt = ObjectState::PacketIn(PacketIn::init(&packet)?);
    state.value_arch = {
        let value_name =
            value::make::text(ctx.arena_mut(), "egress_packet_in".to_owned(), Span::default())?;
        let values_name = vec![value_name];
        let typ_id = typ::make::list(typ::make::var(
            crate::phrase!(node: "id".to_owned(), span: Span::default()),
            vec![],
        ));
        let value_id =
            value::make::list(ctx.arena_mut(), typ_id.node.into(), values_name, Span::default())?;
        let encoding = ctx.external().encoding;
        let value_object = pkt.to_value(ctx.arena_mut(), encoding)?;
        func::update_object_state_e(ctx, state.value_arch, value_id, value_object)
    }?;
    prepare_unicast_ctx(ctx, state)?;
    schedule_packet(ctx, state, Entrypoint::Egress)
}

/// Queues one egress copy per node of multicast `group`.
pub fn schedule_multicast<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
    group: usize,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let arch = find_arch_state(ctx, state.value_arch)?;
    // Unknown group: nothing is replicated
    let Some(handles) = arch.multicast.groups.get(&group).cloned() else {
        return Ok(());
    };
    let packet = {
        let pkt_in = find_ingress_packet_in(ctx, state.value_arch)?;
        let pkt_out = find_ingress_packet_out(ctx, state.value_arch)?;
        // Serialize the current packet to bytes
        packet::to_string(&pkt_in, &pkt_out)
    }?;
    let pkt = ObjectState::PacketIn(PacketIn::init(&packet)?);
    state.value_arch = {
        let value_name =
            value::make::text(ctx.arena_mut(), "egress_packet_in".to_owned(), Span::default())?;
        let values_name = vec![value_name];
        let typ_id = typ::make::list(typ::make::var(
            crate::phrase!(node: "id".to_owned(), span: Span::default()),
            vec![],
        ));
        let value_id =
            value::make::list(ctx.arena_mut(), typ_id.node.into(), values_name, Span::default())?;
        let encoding = ctx.external().encoding;
        let value_object = pkt.to_value(ctx.arena_mut(), encoding)?;
        func::update_object_state_e(ctx, state.value_arch, value_id, value_object)
    }?;
    let arch = find_arch_state(ctx, state.value_arch)?;
    // One egress copy per (port, instance) node
    for handle in handles {
        if let Some(nodes) = arch.multicast.nodes.get(&handle) {
            for node in nodes {
                let value_ctx_original = state.value_ctx;
                prepare_multicast_ctx(ctx, state, node.instance, node.port)?;
                schedule_packet(ctx, state, Entrypoint::Egress)?;
                state.value_ctx = value_ctx_original;
            }
        }
    }
    Ok(())
}

/// Queues ingress-to-egress clones for the session's group.
fn schedule_clone_i2e<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
    session: usize,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let arch = find_arch_state(ctx, state.value_arch)?;
    // An unconfigured session makes no clone
    let Some(group) = arch.mirrortable.get(&session) else {
        return Ok(());
    };
    let Some(handles) = arch.multicast.groups.get(group).cloned() else {
        return Ok(());
    };
    // Preserve the original store for ingress packet_in
    let value_arch_original = state.value_arch;
    remove_ingress_packet_in(ctx, state)?;
    let pkt = find_ingress_packet_in(ctx, state.value_arch)?;
    state.value_arch = {
        let value_name =
            value::make::text(ctx.arena_mut(), "egress_packet_in".to_owned(), Span::default())?;
        let values_name = vec![value_name];
        let typ_id = typ::make::list(typ::make::var(
            crate::phrase!(node: "id".to_owned(), span: Span::default()),
            vec![],
        ));
        let value_id =
            value::make::list(ctx.arena_mut(), typ_id.node.into(), values_name, Span::default())?;
        let encoding = ctx.external().encoding;
        let value_object = ObjectState::PacketIn(pkt).to_value(ctx.arena_mut(), encoding)?;
        func::update_object_state_e(ctx, state.value_arch, value_id, value_object)
    }?;
    let arch = find_arch_state(ctx, state.value_arch)?;
    // One egress copy per (port, instance) node
    for handle in handles {
        if let Some(nodes) = arch.multicast.nodes.get(&handle) {
            for node in nodes {
                let value_ctx_original = state.value_ctx;
                prepare_clone_i2e_ctx(ctx, state, node.instance, node.port)?;
                schedule_packet(ctx, state, Entrypoint::Egress)?;
                state.value_ctx = value_ctx_original;
            }
        }
    }
    let arch = find_arch_state(ctx, state.value_arch)?;
    // Restore the original store while retaining the current scheduler state
    state.value_arch = update_arch_state(ctx, value_arch_original, &arch)?;
    Ok(())
}

/// Queues egress-to-egress clones for the session's group.
fn schedule_clone_e2e<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
    session: usize,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let arch = find_arch_state(ctx, state.value_arch)?;
    // An unconfigured session makes no clone
    let Some(group) = arch.mirrortable.get(&session) else {
        return Ok(());
    };
    let Some(handles) = arch.multicast.groups.get(group).cloned() else {
        return Ok(());
    };
    // Preserve the original store for egress packet_in
    let value_arch_original = state.value_arch;
    let packet = {
        let pkt_in = find_egress_packet_in(ctx, state.value_arch)?;
        let pkt_out = find_egress_packet_out(ctx, state.value_arch)?;
        // Serialize the current packet to bytes
        packet::to_string(&pkt_in, &pkt_out)
    }?;
    let pkt = PacketIn::init(&packet)?;
    state.value_arch = {
        let value_name =
            value::make::text(ctx.arena_mut(), "egress_packet_in".to_owned(), Span::default())?;
        let values_name = vec![value_name];
        let typ_id = typ::make::list(typ::make::var(
            crate::phrase!(node: "id".to_owned(), span: Span::default()),
            vec![],
        ));
        let value_id =
            value::make::list(ctx.arena_mut(), typ_id.node.into(), values_name, Span::default())?;
        let encoding = ctx.external().encoding;
        let value_object = ObjectState::PacketIn(pkt).to_value(ctx.arena_mut(), encoding)?;
        func::update_object_state_e(ctx, state.value_arch, value_id, value_object)
    }?;
    let arch = find_arch_state(ctx, state.value_arch)?;
    // One egress copy per (port, instance) node
    for handle in handles {
        if let Some(nodes) = arch.multicast.nodes.get(&handle) {
            for node in nodes {
                let value_ctx_original = state.value_ctx;
                prepare_clone_e2e_ctx(ctx, state, node.instance, node.port)?;
                schedule_packet(ctx, state, Entrypoint::Egress)?;
                state.value_ctx = value_ctx_original;
            }
        }
    }
    let arch = find_arch_state(ctx, state.value_arch)?;
    // Restore the original store while retaining the current scheduler state
    state.value_arch = update_arch_state(ctx, value_arch_original, &arch)?;
    Ok(())
}

/// Rewinds the ingress input and queues it for ingress again.
pub fn schedule_resubmit<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    prepare_resubmit_ctx(ctx, state)?;
    remove_ingress_packet_in(ctx, state)?;
    schedule_packet(ctx, state, Entrypoint::Ingress)
}

/// Deparses the egress packet and queues it for ingress again.
pub fn schedule_recirculate<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let packet = {
        let pkt_in = find_egress_packet_in(ctx, state.value_arch)?;
        let pkt_out = find_egress_packet_out(ctx, state.value_arch)?;
        // Serialize the current packet to bytes
        packet::to_string(&pkt_in, &pkt_out)
    }?;
    let pkt = ObjectState::PacketIn(PacketIn::init(&packet)?);
    state.value_arch = {
        let value_name =
            value::make::text(ctx.arena_mut(), "ingress_packet_in".to_owned(), Span::default())?;
        let values_name = vec![value_name];
        let typ_id = typ::make::list(typ::make::var(
            crate::phrase!(node: "id".to_owned(), span: Span::default()),
            vec![],
        ));
        let value_id =
            value::make::list(ctx.arena_mut(), typ_id.node.into(), values_name, Span::default())?;
        let encoding = ctx.external().encoding;
        let value_object = pkt.to_value(ctx.arena_mut(), encoding)?;
        func::update_object_state_e(ctx, state.value_arch, value_id, value_object)
    }?;
    prepare_recirculate_ctx(ctx, state)?;
    schedule_packet(ctx, state, Entrypoint::Ingress)
}

/// Emits the egress packet on its egress port.
fn transfer_packet<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let port = {
        let value = rel::lvalue_read_dot_global(
            ctx,
            state.value_ctx,
            state.value_arch,
            "egress_input_metadata",
            "egress_port",
        )?;
        usize::try_from(&unpack::p4_fixed_bit(ctx.arena(), &value)?.1).map_err(ExternError::from)
    }?;
    let packet = {
        let pkt_in = find_egress_packet_in(ctx, state.value_arch)?;
        let pkt_out = find_egress_packet_out(ctx, state.value_arch)?;
        // Serialize the current packet to bytes
        packet::to_string(&pkt_in, &pkt_out)
    }?;
    state.txs.push(Tx { port, packet });
    Ok(())
}

// == Setup packets and globals

/// Installs the received packet and metadata for both pipelines.
fn setup_rx<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
    rx: &Rx,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let encoding = ctx.external().encoding;
    let pkt = ObjectState::PacketIn(PacketIn::init(&rx.packet)?);
    // Set up packet_in objects
    let value_packet = pkt.to_value(ctx.arena_mut(), encoding)?;
    let (value_ctx, value_arch) =
        rel::psa_ingress_init_packet_in(ctx, state.value_ctx, state.value_arch, value_packet)?;
    (state.value_ctx, state.value_arch) = (value_ctx, value_arch);
    let (value_ctx, value_arch) =
        rel::psa_egress_init_packet_in(ctx, state.value_ctx, state.value_arch, value_packet)?;
    (state.value_ctx, state.value_arch) = (value_ctx, value_arch);
    // Set up packet_out objects
    let value_packet =
        ObjectState::PacketOut(PacketOut::default()).to_value(ctx.arena_mut(), encoding)?;
    let (value_ctx, value_arch) =
        rel::psa_ingress_init_packet_out(ctx, state.value_ctx, state.value_arch, value_packet)?;
    (state.value_ctx, state.value_arch) = (value_ctx, value_arch);
    let (value_ctx, value_arch) =
        rel::psa_egress_init_packet_out(ctx, state.value_ctx, state.value_arch, value_packet)?;
    (state.value_ctx, state.value_arch) = (value_ctx, value_arch);
    // Set up global variables
    state.value_ctx =
        rel::psa_ingress_init_globals(ctx, state.value_ctx, state.value_arch, rx.port)?;
    state.value_ctx =
        rel::psa_egress_init_globals(ctx, state.value_ctx, state.value_arch, rx.port)?;
    Ok(())
}

// == Ingress pipeline driver

/// Runs the ingress parser; a rejection is recorded in `parser_error`.
fn drive_ip<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let (value_ctx, value_arch, value_call_result) =
        rel::psa_ingress_parser(ctx, state.value_ctx, state.value_arch)?;
    (state.value_ctx, state.value_arch) = (value_ctx, value_arch);
    let value_error = value::get::matches! { ctx.arena(), &value_call_result,
        "REJECT errorValue" => |values| match values {
            [value_error] => Some(*value_error),
            _ => return Err(ExternError::from(ValueError::CountMismatch {
                expected: 1,
                actual: values.len(),
            })),
        },
        _ => None,
    };
    // A parser rejection is visible to the control
    if let Some(value_error) = value_error {
        state.value_ctx = rel::lvalue_write_dot_global(
            ctx,
            state.value_ctx,
            state.value_arch,
            "ingress_input_metadata",
            "parser_error",
            value_error,
        )?;
    }
    Ok(())
}

/// Runs the ingress control.
fn drive_ig<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let (value_ctx, value_arch, value_result) =
        rel::psa_ingress(ctx, state.value_ctx, state.value_arch)?;
    (state.value_ctx, state.value_arch) = (value_ctx, value_arch);
    Ok(value_result)
}

/// Runs the ingress deparser.
fn drive_id<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let (value_ctx, value_arch, value_result) =
        rel::psa_ingress_deparser(ctx, state.value_ctx, state.value_arch)?;
    (state.value_ctx, state.value_arch) = (value_ctx, value_arch);
    Ok(value_result)
}

/// Runs the ingress parser, control, and deparser in order.
pub fn drive_ingress_pipe<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    drive_ip(ctx, state)?;
    drive_ig(ctx, state)?;
    remove_ingress_packet_out(ctx, state)?;
    drive_id(ctx, state)
}

// == Packet replication engine

/// The packet replication engine: clone, drop, resubmit, multicast, or unicast.
pub fn run_pre<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    if is_ingress_clone(ctx, state)? {
        let session = get_ingress_clone_session_id(ctx, state)?;
        schedule_clone_i2e(ctx, state, session)?;
    }
    // A dropped packet goes nowhere, though its clone was scheduled
    if is_ingress_drop(ctx, state)? {
        return Ok(());
    }
    // A resubmit replaces the packet's normal continuation
    if is_ingress_resubmit(ctx, state)? {
        return schedule_resubmit(ctx, state);
    }
    let group = get_multicast_group(ctx, state)?;
    // A non-zero group multicasts; otherwise unicast
    if group != 0 { schedule_multicast(ctx, state, group) } else { schedule_unicast(ctx, state) }
}

// == Egress pipeline driver

/// Runs the egress parser; a rejection is recorded in `parser_error`.
fn drive_ep<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let (value_ctx, value_arch, value_call_result) =
        rel::psa_egress_parser(ctx, state.value_ctx, state.value_arch)?;
    (state.value_ctx, state.value_arch) = (value_ctx, value_arch);
    let value_error = value::get::matches! { ctx.arena(), &value_call_result,
        "REJECT errorValue" => |values| match values {
            [value_error] => Some(*value_error),
            _ => return Err(ExternError::from(ValueError::CountMismatch {
                expected: 1,
                actual: values.len(),
            })),
        },
        _ => None,
    };
    // A parser rejection is visible to the control
    if let Some(value_error) = value_error {
        state.value_ctx = rel::lvalue_write_dot_global(
            ctx,
            state.value_ctx,
            state.value_arch,
            "egress_input_metadata",
            "parser_error",
            value_error,
        )?;
    }
    Ok(())
}

/// Runs the egress control.
fn drive_eg<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let (value_ctx, value_arch, value_result) =
        rel::psa_egress(ctx, state.value_ctx, state.value_arch)?;
    (state.value_ctx, state.value_arch) = (value_ctx, value_arch);
    Ok(value_result)
}

/// Runs the egress deparser.
fn drive_ed<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let (value_ctx, value_arch, value_result) =
        rel::psa_egress_deparser(ctx, state.value_ctx, state.value_arch)?;
    (state.value_ctx, state.value_arch) = (value_ctx, value_arch);
    Ok(value_result)
}

/// Runs the egress parser, control, and deparser in order.
pub fn drive_egress_pipe<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    drive_ep(ctx, state)?;
    drive_eg(ctx, state)?;
    remove_egress_packet_out(ctx, state)?;
    drive_ed(ctx, state)
}

// == Buffering queueing engine

/// The buffering queueing engine: clone, drop, recirculate, or transmit.
pub fn run_bqe<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    if is_egress_clone(ctx, state)? {
        let session = get_egress_clone_session_id(ctx, state)?;
        schedule_clone_e2e(ctx, state, session)?;
    }
    // A dropped packet goes nowhere, though its clone was scheduled
    if is_egress_drop(ctx, state)? {
        return Ok(());
    }
    // Recirculation returns the packet to ingress instead of transmitting
    if is_egress_recirculate(ctx, state)? {
        schedule_recirculate(ctx, state)
    } else {
        transfer_packet(ctx, state)
    }
}

// == Scheduling packets

/// Resumes a queued packet through its pipeline and replication engine.
pub fn drive_packet<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
    packet: Packet,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    let entrypoint = packet.entrypoint;
    insert_packet(ctx, state, packet)?;
    match entrypoint {
        Entrypoint::Ingress => {
            drive_ingress_pipe(ctx, state)?;
            run_pre(ctx, state)
        }
        Entrypoint::Egress => {
            drive_egress_pipe(ctx, state)?;
            run_bqe(ctx, state)
        }
    }
}

/// Drains the packet queue, running each packet to its next stop.
pub fn run_scheduler<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    loop {
        let mut arch = find_arch_state(ctx, state.value_arch)?;
        // Empty queue: the received packet is fully processed
        let Some(packet) = arch.queue.pop_front() else {
            return Ok(());
        };
        state.value_arch = update_arch_state(ctx, state.value_arch, &arch)?;
        drive_packet(ctx, state, packet)?;
    }
}

/// Processes one received packet through both pipelines and the scheduler.
pub fn drive_pipe<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Psa>,
    state: &mut SimState,
    rx: &Rx,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Psa>,
{
    // Outputs of the previous packet are gone
    state.txs.clear();
    setup_rx(ctx, state, rx)?;
    schedule_packet(ctx, state, Entrypoint::Ingress)?;
    run_scheduler(ctx, state)
}
