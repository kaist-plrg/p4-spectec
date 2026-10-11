//! V1Model parses once, runs ingress and egress, then deparses for transmission
//!
//! ```text
//! Rx -> Parser -> Verify checksum -> Ingress -> Egress
//!                                                |
//!                                                v
//! Tx <- Deparser <- Update checksum <-------------+
//! ```
//!
//! The scheduler runs queued ingress and egress packets, including clones and
//! multicast copies; dropping a packet ends its path
//!
//! Resubmit returns the original packet to the parser; recirculate returns the
//! deparsed packet to the parser

use std::{cell::RefCell, io::Write};

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
    func as v1model_func,
    object::{Counter, DirectCounter, DirectMeter, Register},
    packet::{CloneInfo, CloneType, Entrypoint, Packet},
};

// == Configuration

/// The v1model architecture, parameterized by its state encoding.
pub struct V1Model {
    /// Encoding of architecture and object states as external values.
    encoding: Encoding,
    /// Host writer for program log messages, separate from semantic states.
    output: RefCell<Box<dyn Write>>,
}

impl Default for V1Model {
    fn default() -> Self {
        Self::new(Encoding::default())
    }
}

impl V1Model {
    /// Creates the architecture with the given state encoding.
    pub fn new(encoding: Encoding) -> Self {
        Self::new_with_output(encoding, std::io::stdout())
    }

    /// Creates the architecture with a writer for program log messages.
    pub fn new_with_output(encoding: Encoding, output: impl Write + 'static) -> Self {
        Self { encoding, output: RefCell::new(Box::new(output)) }
    }

    /// Writes one program log message with its terminating newline.
    pub(super) fn write_log(&self, text: &str) -> Result<(), ExternError> {
        writeln!(self.output.borrow_mut(), "{text}")
            .map_err(|error| error::program_output_failed(error.to_string()).into())
    }
}

// == Extern objects

/// Core and v1model-specific extern objects.
#[derive(Clone, Debug, PartialEq, Eq, SerializeState, DeserializeState)]
#[serde(serialize_state = "EncodeContext<'arena>", ser_parameters = "'arena")]
#[serde(deserialize_state = "DecodeContext<'de>")]
pub enum ObjectState {
    /// The `packet_in` of the current packet.
    PacketIn(PacketIn),
    /// The `packet_out` being emitted.
    PacketOut(PacketOut),
    /// A `counter` array.
    Counter(Counter),
    /// A `register` array.
    Register(#[serde(state)] Register),
    /// A `direct_counter`.
    DirectCounter(DirectCounter),
    /// A `direct_meter`.
    DirectMeter(DirectMeter),
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

/// Rewrites p4c block names in an STF statement to the specification's.
///
/// `ingress`/`preqos` become `main.ig`, `egress`/`postqos`/`c3` become
/// `main.eg`, and `$valid$` in an `add` match becomes `isValid()`.
pub fn transform_stf_stmt(mut stmt: Statement) -> Statement {
    fn transform_name(name: crate::stf::ast::Name) -> crate::stf::ast::Name {
        name.rewrite_substring(&["ingress", "preqos"], "main.ig")
            .rewrite_substring(&["egress", "postqos", "c3"], "main.eg")
    }
    match &mut stmt {
        // Table, action, and every match of an add
        Statement::Add { table, matches, action, .. } => {
            *table = transform_name(table.clone());
            action.name = transform_name(action.name.clone());
            for mtch in matches {
                *mtch = mtch.clone().rewrite_valid();
            }
        }
        // Table and action of a default
        Statement::SetDefault { table, action } => {
            *table = transform_name(table.clone());
            action.name = transform_name(action.name.clone());
        }
        _ => {}
    }
    stmt
}

// == Architectural state

/// The initial architecture state: empty queue, tables, and requests.
pub(super) fn init_arch_state<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    let encoding = ctx.external().encoding;
    Arch::default().to_value(ctx.arena_mut(), encoding)
}

/// Decodes the architecture state stored in `value_arch`.
pub fn find_arch_state<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    value_arch: Value,
) -> Result<Arch, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    let encoding = ctx.external().encoding;
    let value_state = func::find_arch_state_e(ctx, value_arch)?;
    Arch::from_value(ctx.arena_mut(), encoding, &value_state)
}

/// Encodes `arch` back into `value_arch`.
pub fn update_arch_state<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    value_arch: Value,
    arch: &Arch,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    let encoding = ctx.external().encoding;
    let value_state = arch.to_value(ctx.arena_mut(), encoding)?;
    func::update_arch_state_e(ctx, value_arch, value_state).map_err(ExternError::from)
}

// == Object state

/// Decodes the object named `value_id` from `value_arch`.
pub fn find_object_state<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    value_arch: Value,
    value_id: Value,
) -> Result<ObjectState, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    let encoding = ctx.external().encoding;
    let value_object = func::find_object_state_e(ctx, value_arch, value_id)?;
    ObjectState::from_value(ctx.arena_mut(), encoding, &value_object)
}

/// The `packet_in` object of the current packet.
fn find_packet_in<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    value_arch: Value,
) -> Result<PacketIn, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    // The object id is the one-element path `packet_in`
    let value_name = value::make::text(ctx.arena_mut(), "packet_in".to_owned(), Span::default())?;
    let values_name = vec![value_name];
    let typ_id = typ::make::list(typ::make::var(
        crate::phrase!(node: "id".to_owned(), span: Span::default()),
        vec![],
    ));
    let value_id =
        value::make::list(ctx.arena_mut(), typ_id.node.into(), values_name, Span::default())?;
    match find_object_state(ctx, value_arch, value_id)? {
        ObjectState::PacketIn(pkt) => Ok(pkt),
        _ => Err(error::extern_object_undefined("packet_in extern not found".to_owned()).into()),
    }
}

/// The `packet_out` object of the current packet.
fn find_packet_out<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    value_arch: Value,
) -> Result<PacketOut, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    // The object id is the one-element path `packet_out`
    let value_name = value::make::text(ctx.arena_mut(), "packet_out".to_owned(), Span::default())?;
    let values_name = vec![value_name];
    let typ_id = typ::make::list(typ::make::var(
        crate::phrase!(node: "id".to_owned(), span: Span::default()),
        vec![],
    ));
    let value_id =
        value::make::list(ctx.arena_mut(), typ_id.node.into(), values_name, Span::default())?;
    match find_object_state(ctx, value_arch, value_id)? {
        ObjectState::PacketOut(pkt) => Ok(pkt),
        _ => Err(error::extern_object_undefined("packet_out extern not found".to_owned()).into()),
    }
}

// == Extern calls

// - Initialization

/// Constructs a v1model object from its constructor call.
///
/// `values` holds the object name, type arguments, parameter names,
/// and parameter values; core objects and unknown names get an empty state.
pub(super) fn eval_extern_init<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    values: &[Value],
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    let encoding = ctx.external().encoding;
    let (value_name, value_targs, value_ids, value_args) = value::get::four(values)?;
    let name = value::get::text(ctx.arena(), value_name)?.to_owned();
    let object = match name.as_str() {
        "counter" => Some(ObjectState::Counter(Counter::init(
            ctx.arena(),
            *value_targs,
            *value_ids,
            *value_args,
        )?)),
        "register" => {
            Some(ObjectState::Register(Register::init(ctx, *value_targs, *value_ids, *value_args)?))
        }
        "direct_counter" => Some(ObjectState::DirectCounter(DirectCounter::init(
            ctx.arena(),
            *value_targs,
            *value_ids,
            *value_args,
        )?)),
        "direct_meter" => Some(ObjectState::DirectMeter(DirectMeter::init(
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

/// Dispatches an extern function call by name and parameter names.
///
/// `values` holds the context, the architecture state, the name,
/// and the parameter names; the result adds the call result to the pair.
pub(super) fn eval_extern_func_call<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    values: &[Value],
) -> Result<Vec<Value>, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    let (value_ctx, value_arch, value_name, value_names) = value::get::four(values)?;
    let name = value::get::text(ctx.arena(), value_name)?.to_owned();
    let names = value::get::list(ctx.arena(), value_names)?
        .iter()
        .map(|value| value::get::text(ctx.arena(), value).map(str::to_owned))
        .collect::<Result<Vec<_>, _>>()?;
    let names_ref: Vec<_> = names.iter().map(String::as_str).collect();
    let (value_ctx, value_arch, value_call_result) = match (name.as_str(), names_ref.as_slice()) {
        ("verify", ["check", "toSignal"]) => core_func::verify(ctx, *value_ctx, *value_arch)?,
        ("digest", ["receiver", "data"]) => v1model_func::digest(ctx, *value_ctx, *value_arch)?,
        ("mark_to_drop", ["standard_metadata"]) => {
            v1model_func::mark_to_drop(ctx, *value_ctx, *value_arch)?
        }
        ("verify_checksum", ["condition", "data", "checksum", "algo"]) => {
            v1model_func::verify_checksum(ctx, *value_ctx, *value_arch)?
        }
        ("update_checksum", ["condition", "data", "checksum", "algo"]) => {
            v1model_func::update_checksum(ctx, *value_ctx, *value_arch)?
        }
        ("clone_preserving_field_list", ["type", "session", "index"]) => {
            v1model_func::clone_preserving_field_list(ctx, *value_ctx, *value_arch)?
        }
        ("resubmit_preserving_field_list", ["index"]) => {
            v1model_func::resubmit_preserving_field_list(ctx, *value_ctx, *value_arch)?
        }
        ("recirculate_preserving_field_list", ["index"]) => {
            v1model_func::recirculate_preserving_field_list(ctx, *value_ctx, *value_arch)?
        }
        ("hash", ["result", "algo", "base", "data", "max"]) => {
            v1model_func::hash(ctx, *value_ctx, *value_arch)?
        }
        ("log_msg", ["msg"]) => v1model_func::log_msg(ctx, *value_ctx, *value_arch)?,
        ("log_msg", ["msg", "data"]) => v1model_func::log_msg_format(ctx, *value_ctx, *value_arch)?,
        // The payload variants need the packet's unparsed bytes
        ("verify_checksum_with_payload", ["condition", "data", "checksum", "algo"]) => {
            let pkt = find_packet_in(ctx, *value_arch)?;
            v1model_func::verify_checksum_with_payload(ctx, *value_ctx, *value_arch, &pkt)?
        }
        ("update_checksum_with_payload", ["condition", "data", "checksum", "algo"]) => {
            let pkt = find_packet_in(ctx, *value_arch)?;
            v1model_func::update_checksum_with_payload(ctx, *value_ctx, *value_arch, &pkt)?
        }
        // Anything else, such as random or truncate, is unsupported
        _ => {
            return Err(error::extern_function_unsupported(format!(
                "unsupported extern function call: {name}({})",
                names.join(", ")
            ))
            .into());
        }
    };
    Ok(vec![value_ctx, value_arch, value_call_result])
}

// - Method calls

/// Dispatches an extern method call on the object named `value_id`.
///
/// The object is decoded, updated by its method, and written back;
/// `values` holds the context, state, object id, method name,
/// and parameter names.
pub(super) fn eval_extern_method_call<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    values: &[Value],
) -> Result<Vec<Value>, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
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
            // Counters and meters see the current packet
            (ObjectState::Counter(object), "count", ["index"]) => {
                let pkt = find_packet_in(ctx, *value_arch)?;
                let (object, value_ctx, value_arch, value_call_result) =
                    object.count(ctx, *value_ctx, *value_arch, &pkt)?;
                (ObjectState::Counter(object), value_ctx, value_arch, value_call_result)
            }
            (ObjectState::Register(object), "read", ["result", "index"]) => {
                let (object, value_ctx, value_arch, value_call_result) =
                    object.read(ctx, *value_ctx, *value_arch)?;
                (ObjectState::Register(object), value_ctx, value_arch, value_call_result)
            }
            (ObjectState::Register(object), "write", ["index", "value"]) => {
                let (object, value_ctx, value_arch, value_call_result) =
                    object.write(ctx, *value_ctx, *value_arch)?;
                (ObjectState::Register(object), value_ctx, value_arch, value_call_result)
            }
            (ObjectState::DirectCounter(object), "count", []) => {
                let pkt = find_packet_in(ctx, *value_arch)?;
                let (object, value_ctx, value_arch, value_call_result) =
                    object.count(ctx, *value_ctx, *value_arch, &pkt)?;
                (ObjectState::DirectCounter(object), value_ctx, value_arch, value_call_result)
            }
            (ObjectState::DirectMeter(object), "read", ["result"]) => {
                let pkt = find_packet_in(ctx, *value_arch)?;
                let (object, value_ctx, value_arch, value_call_result) =
                    object.read(ctx, *value_ctx, *value_arch, &pkt)?;
                (ObjectState::DirectMeter(object), value_ctx, value_arch, value_call_result)
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

// == Mirror table interface

/// Configures mirror `session` to send clones to `port`.
pub fn add_mirror_session<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    value_arch: Value,
    session: usize,
    port: usize,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    let mut arch = find_arch_state(ctx, value_arch)?;
    arch.mirrortable.insert(session, port);
    update_arch_state(ctx, value_arch, &arch)
}

/// Multicast mirror sessions are not supported.
pub fn add_mirror_session_mc<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    _value_arch: Value,
    _session: usize,
    _group: usize,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    let _ = ctx;
    Err(error::control_operation_unsupported(
        "add_mirror_session_mc is not implemented for the v1model simulator".to_owned(),
    )
    .into())
}

// == Multicast interface

/// Creates multicast group `group`.
pub fn mc_mgrp_create<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    value_arch: Value,
    group: usize,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    let mut arch = find_arch_state(ctx, value_arch)?;
    arch.multicast.group_create(group);
    update_arch_state(ctx, value_arch, &arch)
}

/// Creates a multicast node with replication id `instance` on `ports`.
pub fn mc_node_create<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    value_arch: Value,
    instance: usize,
    ports: &[usize],
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    let mut arch = find_arch_state(ctx, value_arch)?;
    arch.multicast.node_create(instance, ports);
    update_arch_state(ctx, value_arch, &arch)
}

/// Adds node `handle` to multicast group `group`.
pub fn mc_node_associate<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    value_arch: Value,
    group: usize,
    handle: usize,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    let mut arch = find_arch_state(ctx, value_arch)?;
    arch.multicast.node_associate(group, handle);
    update_arch_state(ctx, value_arch, &arch)
}

// == Register interface

/// Control-plane register reads are not supported.
pub fn register_read<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    _value_arch: Value,
    _name: &str,
    _idx: usize,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    let _ = ctx;
    Err(error::control_operation_unsupported(
        "register_read is not implemented for the v1model simulator".to_owned(),
    )
    .into())
}

/// Control-plane register writes are not supported.
pub fn register_write<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    _value_arch: Value,
    _name: &str,
    _idx: usize,
    _int: BigInt,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    let _ = ctx;
    Err(error::control_operation_unsupported(
        "register_write is not implemented for the v1model simulator".to_owned(),
    )
    .into())
}

/// Control-plane register resets are not supported.
pub fn register_reset<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    _value_arch: Value,
    _name: &str,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    let _ = ctx;
    Err(error::control_operation_unsupported(
        "register_reset is not implemented for the v1model simulator".to_owned(),
    )
    .into())
}

// == Packet state

/// Makes `packet` current: its input becomes the `packet_in` object
/// and its context the state's context.
fn insert_packet<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    state: &mut SimState,
    packet: Packet,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    state.value_arch = {
        // The object id is the one-element path `packet_in`
        let value_name =
            value::make::text(ctx.arena_mut(), "packet_in".to_owned(), Span::default())?;
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

/// Rewinds the `packet_in` cursor over the same bytes.
fn remove_packet_in<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    state: &mut SimState,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    let mut pkt = find_packet_in(ctx, state.value_arch)?;
    pkt.reset();
    state.value_arch = {
        // The object id is the one-element path `packet_in`
        let value_name =
            value::make::text(ctx.arena_mut(), "packet_in".to_owned(), Span::default())?;
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

/// Replaces `packet_out` with an empty one before deparsing.
fn remove_packet_out<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    state: &mut SimState,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    state.value_arch = {
        // The object id is the one-element path `packet_out`
        let value_name =
            value::make::text(ctx.arena_mut(), "packet_out".to_owned(), Span::default())?;
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

/// Whether `egress_spec` holds the 9-bit drop port 511.
fn is_dropped<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    state: &SimState,
) -> Result<bool, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    let value = rel::lvalue_read_dot_global(
        ctx,
        state.value_ctx,
        state.value_arch,
        "standard_metadata",
        "egress_spec",
    )?;
    let (width, int) = unpack::p4_fixed_bit(ctx.arena(), &value)?;
    Ok(width == 9.into() && int == 511.into())
}

/// The multicast group requested in `standard_metadata.mcast_grp`.
fn get_mcast_grp<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    state: &mut SimState,
) -> Result<usize, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    let group = {
        let value = rel::lvalue_read_dot_global(
            ctx,
            state.value_ctx,
            state.value_arch,
            "standard_metadata",
            "mcast_grp",
        )?;
        usize::try_from(&unpack::p4_fixed_bit(ctx.arena(), &value)?.1).map_err(ExternError::from)
    }?;
    Ok(group)
}

// == Pipeline initializer

/// Instantiates the program and returns the initial simulator state.
pub fn init_pipe<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    program: Value,
) -> Result<SimState, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    let (value_ctx, value_arch) = pgm::v1model_init(ctx, program)?;
    Ok(SimState { value_ctx, value_arch, txs: vec![] })
}

// == Pipeline driver

/// Installs the received packet as `packet_in`, an empty `packet_out`,
/// and the standard metadata for `rx.port`.
pub fn setup_rx<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    state: &mut SimState,
    rx: &Rx,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    let encoding = ctx.external().encoding;
    // Setup packet input, output and global variables in source order
    let value_packet =
        ObjectState::PacketIn(PacketIn::init(&rx.packet)?).to_value(ctx.arena_mut(), encoding)?;
    let (value_ctx, value_arch) =
        rel::v1model_init_packet_in(ctx, state.value_ctx, state.value_arch, value_packet)?;
    (state.value_ctx, state.value_arch) = (value_ctx, value_arch);
    let value_packet =
        ObjectState::PacketOut(PacketOut::default()).to_value(ctx.arena_mut(), encoding)?;
    let (value_ctx, value_arch) =
        rel::v1model_init_packet_out(ctx, state.value_ctx, state.value_arch, value_packet)?;
    (state.value_ctx, state.value_arch) = (value_ctx, value_arch);
    state.value_ctx = rel::v1model_init_globals(ctx, state.value_ctx, state.value_arch, rx.port)?;
    Ok(())
}

// == Parser + Verify

/// Runs the parser; a rejection is recorded in `parser_error`.
pub fn drive_p<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    state: &mut SimState,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    let (value_ctx, value_arch, value_call_result) =
        rel::v1model_parser(ctx, state.value_ctx, state.value_arch)?;
    (state.value_ctx, state.value_arch) = (value_ctx, value_arch);
    // A `REJECT` carries the error value
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
    // Parser errors are visible to the controls
    if let Some(value_error) = value_error {
        state.value_ctx = rel::lvalue_write_dot_global(
            ctx,
            state.value_ctx,
            state.value_arch,
            "standard_metadata",
            "parser_error",
            value_error,
        )?;
    }
    Ok(())
}

/// Runs the VerifyChecksum control.
pub fn drive_vr<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    state: &mut SimState,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    let (value_ctx, value_arch, value_result) =
        rel::v1model_verify(ctx, state.value_ctx, state.value_arch)?;
    (state.value_ctx, state.value_arch) = (value_ctx, value_arch);
    Ok(value_result)
}

/// Front half of the pipeline: reset requests, rewind the input, parse, verify.
pub fn drive_pipe_pre<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    state: &mut SimState,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    let mut arch = find_arch_state(ctx, state.value_arch)?;
    // Forget the previous pass's requests
    arch.reset();
    state.value_arch = update_arch_state(ctx, state.value_arch, &arch)?;
    remove_packet_in(ctx, state)?;
    drive_p(ctx, state)?;
    drive_vr(ctx, state)
}

// == Checksum + Deparser

/// Runs the ComputeChecksum control.
pub fn drive_ck<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    state: &mut SimState,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    let (value_ctx, value_arch, value_result) =
        rel::v1model_check(ctx, state.value_ctx, state.value_arch)?;
    (state.value_ctx, state.value_arch) = (value_ctx, value_arch);
    Ok(value_result)
}

/// Runs the deparser.
pub fn drive_dep<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    state: &mut SimState,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    let (value_ctx, value_arch, value_result) =
        rel::v1model_deparse(ctx, state.value_ctx, state.value_arch)?;
    (state.value_ctx, state.value_arch) = (value_ctx, value_arch);
    Ok(value_result)
}

/// Back half of the pipeline: checksum, deparse, and transmit on `egress_spec`.
pub fn drive_pipe_post<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    state: &mut SimState,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    drive_ck(ctx, state)?;
    remove_packet_out(ctx, state)?;
    drive_dep(ctx, state)?;
    // The output port is the final `egress_spec`
    let port = {
        let value = rel::lvalue_read_dot_global(
            ctx,
            state.value_ctx,
            state.value_arch,
            "standard_metadata",
            "egress_spec",
        )?;
        usize::try_from(&unpack::p4_fixed_bit(ctx.arena(), &value)?.1).map_err(ExternError::from)
    }?;
    // Emitted headers followed by the unparsed payload
    let packet = {
        let pkt_in = find_packet_in(ctx, state.value_arch)?;
        let pkt_out = find_packet_out(ctx, state.value_arch)?;
        packet::to_string(&pkt_in, &pkt_out)
    }?;
    state.txs.push(Tx { port, packet });
    Ok(())
}

// == Prepare context for resubmit/clone/recirculate/multicast

/// Marks the context as a resubmit preserving field list `idx`.
fn prepare_resubmit_ctx<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    state: &mut SimState,
    idx: usize,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    // Preserve the `@field_list(idx)` metadata fields
    let value_idx = pack::p4_fixed_bit(ctx.arena_mut(), 8.into(), idx.into())?;
    state.value_ctx = rel::v1model_setup_preserved_meta_fields(
        ctx,
        state.value_ctx,
        state.value_arch,
        value_idx,
    )?;
    // PKT_INSTANCE_TYPE_RESUBMIT
    let value = pack::p4_fixed_bit(ctx.arena_mut(), 32.into(), 6.into())?;
    state.value_ctx = rel::lvalue_write_dot_global(
        ctx,
        state.value_ctx,
        state.value_arch,
        "standard_metadata",
        "instance_type",
        value,
    )?;
    Ok(())
}

/// Marks the context as a clone to `port` preserving field list `idx`.
fn prepare_clone_ctx<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    state: &mut SimState,
    clone_type: CloneType,
    port: usize,
    idx: usize,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    // Preserve the `@field_list(idx)` metadata fields
    let value_idx = pack::p4_fixed_bit(ctx.arena_mut(), 8.into(), idx.into())?;
    state.value_ctx = rel::v1model_setup_preserved_meta_fields(
        ctx,
        state.value_ctx,
        state.value_arch,
        value_idx,
    )?;
    // PKT_INSTANCE_TYPE_INGRESS_CLONE or PKT_INSTANCE_TYPE_EGRESS_CLONE
    let instance = match clone_type {
        CloneType::I2E => 1,
        CloneType::E2E => 2,
    };
    let value = pack::p4_fixed_bit(ctx.arena_mut(), 32.into(), instance.into())?;
    state.value_ctx = rel::lvalue_write_dot_global(
        ctx,
        state.value_ctx,
        state.value_arch,
        "standard_metadata",
        "instance_type",
        value,
    )?;
    let value = pack::p4_fixed_bit(ctx.arena_mut(), 9.into(), port.into())?;
    state.value_ctx = rel::lvalue_write_dot_global(
        ctx,
        state.value_ctx,
        state.value_arch,
        "standard_metadata",
        "egress_spec",
        value,
    )?;
    Ok(())
}

/// Marks the context as a recirculation preserving field list `idx`.
fn prepare_recirculate_ctx<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    state: &mut SimState,
    idx: usize,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    // Preserve the `@field_list(idx)` metadata fields
    let value_idx = pack::p4_fixed_bit(ctx.arena_mut(), 8.into(), idx.into())?;
    state.value_ctx = rel::v1model_setup_preserved_meta_fields(
        ctx,
        state.value_ctx,
        state.value_arch,
        value_idx,
    )?;
    // PKT_INSTANCE_TYPE_RECIRC
    let value = pack::p4_fixed_bit(ctx.arena_mut(), 32.into(), 4.into())?;
    state.value_ctx = rel::lvalue_write_dot_global(
        ctx,
        state.value_ctx,
        state.value_arch,
        "standard_metadata",
        "instance_type",
        value,
    )?;
    Ok(())
}

/// Marks the context as replica `rid` of a multicast to `port`.
fn prepare_multicast_ctx<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    state: &mut SimState,
    rid: usize,
    port: usize,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    // Each replica carries its node's replication id and port
    let value = pack::p4_fixed_bit(ctx.arena_mut(), 16.into(), rid.into())?;
    state.value_ctx = rel::lvalue_write_dot_global(
        ctx,
        state.value_ctx,
        state.value_arch,
        "standard_metadata",
        "egress_rid",
        value,
    )?;
    let value = pack::p4_fixed_bit(ctx.arena_mut(), 9.into(), port.into())?;
    state.value_ctx = rel::lvalue_write_dot_global(
        ctx,
        state.value_ctx,
        state.value_arch,
        "standard_metadata",
        "egress_spec",
        value,
    )?;
    // PKT_INSTANCE_TYPE_REPLICATION
    let value = pack::p4_fixed_bit(ctx.arena_mut(), 32.into(), 5.into())?;
    state.value_ctx = rel::lvalue_write_dot_global(
        ctx,
        state.value_ctx,
        state.value_arch,
        "standard_metadata",
        "instance_type",
        value,
    )?;
    Ok(())
}

// == Schedule resubmit/clone/recirculate/multicast if needed

/// Queues the current packet to resume at `entrypoint`.
pub fn schedule_packet<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    state: &mut SimState,
    entrypoint: Entrypoint,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    let packet_in = find_packet_in(ctx, state.value_arch)?;
    let packet = Packet { value_ctx: state.value_ctx, packet_in, entrypoint };
    let mut arch = find_arch_state(ctx, state.value_arch)?;
    // Ingress work runs before queued egress copies
    match entrypoint {
        Entrypoint::Ingress => arch.queue.push_front(packet),
        Entrypoint::Egress => arch.queue.push_back(packet),
    }
    state.value_arch = update_arch_state(ctx, state.value_arch, &arch)?;
    Ok(())
}

/// Acts on a resubmit request: parse the input again and queue it for ingress.
pub fn schedule_resubmit<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    state: &mut SimState,
    arch: &Arch,
) -> Result<bool, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    // No resubmit requested
    let Some(idx) = arch.action.resubmit_opt else {
        return Ok(false);
    };
    // The new packet gets its own context; this one continues unchanged
    let value_ctx_original = state.value_ctx;
    prepare_resubmit_ctx(ctx, state, idx)?;
    drive_pipe_pre(ctx, state)?;
    schedule_packet(ctx, state, Entrypoint::Ingress)?;
    state.value_ctx = value_ctx_original;
    Ok(true)
}

/// Acts on a clone request: queue a copy for egress on the session's port.
pub fn schedule_clone<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    state: &mut SimState,
    arch: &Arch,
) -> Result<bool, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    // No clone requested
    let Some(CloneInfo(clone_type, session, idx)) = arch.action.clone_opt else {
        return Ok(false);
    };
    // An unconfigured session makes no clone
    let Some(&port) = arch.mirrortable.get(&session) else {
        return Ok(false);
    };
    // The new packet gets its own context; this one continues unchanged
    let value_ctx_original = state.value_ctx;
    prepare_clone_ctx(ctx, state, clone_type, port, idx)?;
    // I2E clones are parsed again; E2E clones keep the egress state
    if clone_type == CloneType::I2E {
        drive_pipe_pre(ctx, state)?;
    }
    schedule_packet(ctx, state, Entrypoint::Egress)?;
    state.value_ctx = value_ctx_original;
    Ok(true)
}

/// Acts on a recirculate request: deparse, then parse the output again.
pub fn schedule_recirculate<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    state: &mut SimState,
    arch: &Arch,
) -> Result<bool, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    // No recirculate requested
    let Some(idx) = arch.action.recirculate_opt else {
        return Ok(false);
    };
    // The new packet gets its own context; this one continues unchanged
    let value_ctx_original = state.value_ctx;
    prepare_recirculate_ctx(ctx, state, idx)?;
    // Run checksum and deparser before feeding the output back to the parser
    drive_ck(ctx, state)?;
    remove_packet_out(ctx, state)?;
    drive_dep(ctx, state)?;
    // Emitted headers followed by the unparsed payload
    let packet = {
        let pkt_in = find_packet_in(ctx, state.value_arch)?;
        let pkt_out = find_packet_out(ctx, state.value_arch)?;
        packet::to_string(&pkt_in, &pkt_out)
    }?;
    // The deparsed bytes become the new input
    let pkt = ObjectState::PacketIn(PacketIn::init(&packet)?);
    state.value_arch = {
        // The object id is the one-element path `packet_in`
        let value_name =
            value::make::text(ctx.arena_mut(), "packet_in".to_owned(), Span::default())?;
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
    // Parser and verify run before the ingress packet enters the queue
    drive_pipe_pre(ctx, state)?;
    schedule_packet(ctx, state, Entrypoint::Ingress)?;
    state.value_ctx = value_ctx_original;
    Ok(true)
}

/// Queues one egress copy per node of multicast `group`.
pub fn schedule_multicast<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    state: &mut SimState,
    arch: &Arch,
    group: usize,
) -> Result<bool, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    // Unknown group: nothing is replicated
    let Some(handles) = arch.multicast.groups.get(&group) else {
        return Ok(false);
    };
    // One egress copy per (port, rid) node, in association order
    for handle in handles {
        if let Some(nodes) = arch.multicast.nodes.get(handle) {
            for node in nodes {
                prepare_multicast_ctx(ctx, state, node.rid, node.port)?;
                schedule_packet(ctx, state, Entrypoint::Egress)?;
            }
        }
    }
    Ok(true)
}

// == Ingress + Handle clone, resubmit, drop

/// Runs ingress, then schedules the clone, resubmit, multicast, or unicast.
pub fn drive_ig<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    state: &mut SimState,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    let (value_ctx, value_arch, value_result) =
        rel::v1model_ingress(ctx, state.value_ctx, state.value_arch)?;
    (state.value_ctx, state.value_arch) = (value_ctx, value_arch);
    let arch = find_arch_state(ctx, state.value_arch)?;
    // A clone request never stops the original
    schedule_clone(ctx, state, &arch)?;
    // A resubmit replaces the packet's normal continuation
    if schedule_resubmit(ctx, state, &arch)? {
        return Ok(value_result);
    }
    let group = get_mcast_grp(ctx, state)?;
    // Multicast wins over unicast; a dropped packet goes nowhere
    if group != 0 {
        schedule_multicast(ctx, state, &arch, group)?;
    } else if !is_dropped(ctx, state)? {
        schedule_packet(ctx, state, Entrypoint::Egress)?;
    }
    Ok(value_result)
}

/// Copies `egress_spec` into `egress_port` for the egress control.
fn prepare_egress_ctx<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    state: &mut SimState,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    let value_port = rel::lvalue_read_dot_global(
        ctx,
        state.value_ctx,
        state.value_arch,
        "standard_metadata",
        "egress_spec",
    )?;
    state.value_ctx = rel::lvalue_write_dot_global(
        ctx,
        state.value_ctx,
        state.value_arch,
        "standard_metadata",
        "egress_port",
        value_port,
    )?;
    Ok(())
}

/// Runs egress; `None` when the packet was dropped or recirculated.
pub fn drive_eg<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    state: &mut SimState,
) -> Result<Option<Value>, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    prepare_egress_ctx(ctx, state)?;
    let (value_ctx, value_arch, value_result) =
        rel::v1model_egress(ctx, state.value_ctx, state.value_arch)?;
    (state.value_ctx, state.value_arch) = (value_ctx, value_arch);
    let arch = find_arch_state(ctx, state.value_arch)?;
    // A clone request never stops the original
    schedule_clone(ctx, state, &arch)?;
    // Dropped in egress: nothing to transmit
    if is_dropped(ctx, state)? {
        return Ok(None);
    }
    let arch = find_arch_state(ctx, state.value_arch)?;
    // Recirculation consumes the packet
    if schedule_recirculate(ctx, state, &arch)? { Ok(None) } else { Ok(Some(value_result)) }
}

// == Scheduling packets

/// Resumes a queued packet at its entrypoint.
pub fn drive_packet<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    state: &mut SimState,
    packet: Packet,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    let entrypoint = packet.entrypoint;
    insert_packet(ctx, state, packet)?;
    match entrypoint {
        // Ingress schedules its own continuation
        Entrypoint::Ingress => {
            drive_ig(ctx, state)?;
            Ok(())
        }
        // Egress deparses and transmits unless dropped or recirculated
        Entrypoint::Egress => match drive_eg(ctx, state)? {
            Some(_) => drive_pipe_post(ctx, state),
            None => Ok(()),
        },
    }
}

/// Drains the packet queue, running each packet to its next stop.
pub fn run_scheduler<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    state: &mut SimState,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    loop {
        let mut arch = find_arch_state(ctx, state.value_arch)?;
        // Empty queue: the received packet is fully processed
        let Some(packet) = arch.queue.pop_front() else {
            return Ok(());
        };
        // Each packet starts with no pending requests
        arch.reset();
        state.value_arch = update_arch_state(ctx, state.value_arch, &arch)?;
        drive_packet(ctx, state, packet)?;
    }
}

/// Processes one received packet through the pipeline and its scheduler.
pub fn drive_pipe<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    state: &mut SimState,
    rx: &Rx,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    // Outputs of the previous packet are gone
    state.txs.clear();
    setup_rx(ctx, state, rx)?;
    drive_pipe_pre(ctx, state)?;
    schedule_packet(ctx, state, Entrypoint::Ingress)?;
    run_scheduler(ctx, state)
}
