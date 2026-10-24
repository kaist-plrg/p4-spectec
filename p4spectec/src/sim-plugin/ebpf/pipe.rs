//! eBPF parses the packet, then filters it using the parsed headers
//!
//! ```text
//! Rx -> Parser -> Filter -> accept = true -> Tx
//!         |                  |
//!       reject          accept = false
//!         |                  |
//!         v                  v
//!        drop               drop
//! ```
//!
//! Accepted packets keep their original bytes and input port
//! There is no deparser

use serde_derive_state::{DeserializeState, SerializeState};

use crate::lang::{
    common::source::Span,
    data::{
        arena::Arena,
        value::{
            ValueError,
            extern_value::{TypDef, TypDefId},
            flat::{
                self as value, Value,
                external::{DecodeContext, EncodeContext},
            },
        },
    },
};

use crate::runner::{ExternError, Interface, Interpreter, RunnerContext};

use crate::stf::ast::{Name, Statement};

use crate::sim_plugin::error;

use super::super::{
    core::{func as core_func, object::PacketIn},
    io::{Rx, Tx},
    spec::{func, pgm, rel, unpack},
    state::SimState,
};

use super::object::CounterArray;

// == Configuration

/// The eBPF architecture, with its definitions of the state types.
pub struct Ebpf {
    /// Definition of `archState`, which carries no payload.
    typdef_arch: TypDef<()>,
    /// Definition of `objectState`; stateless objects carry `None`.
    typdef_object: TypDef<Option<ExternObject>>,
}

impl Ebpf {
    /// Defines the architecture's extern syntax types in `arena`.
    pub fn new(arena: &mut Arena) -> Result<Self, ValueError> {
        Ok(Self {
            typdef_arch: TypDef::with_codec(arena, "archState")?,
            typdef_object: TypDef::with_codec(arena, "objectState")?,
        })
    }

    /// The definitions of `archState` and `objectState`.
    pub(super) fn typdefs(&self) -> Vec<TypDefId> {
        vec![self.typdef_arch.id(), self.typdef_object.id()]
    }
}

// == Extern objects

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, SerializeState, DeserializeState)]
#[serde(serialize_state = "EncodeContext<'arena>", ser_parameters = "'arena")]
#[serde(deserialize_state = "DecodeContext<'arena>", de_parameters = "'arena")]
/// Core and eBPF-specific extern objects.
pub enum ExternObject {
    /// The `packet_in` being parsed.
    PacketIn(PacketIn),
    /// A `CounterArray`.
    CounterArray(CounterArray),
}

impl ExternObject {
    // - Wrapping

    /// Wraps the object as the specification's `objectState` value.
    pub fn to_value(
        self,
        arena: &mut Arena,
        typdef: TypDef<Option<Self>>,
    ) -> Result<Value, ExternError> {
        Ok(typdef.make(arena, Some(self), Span::default())?)
    }

    // - Reading

    /// Reads the object from an `objectState` value; stateless objects fail.
    pub fn from_value(
        arena: &Arena,
        typdef: TypDef<Option<Self>>,
        value: &Value,
    ) -> Result<Self, ExternError> {
        typdef
            .get(arena, value)?
            .clone()
            .ok_or_else(|| error::object_state_undefined("extern object has no state").into())
    }
}

// == STF transformation

/// Rewrites p4c block names in an STF statement to the specification's.
///
/// `pipe` blocks become `main.filt`, `_NoAction` becomes `NoAction`.
pub fn transform_stf_stmt(mut stmt: Statement) -> Statement {
    fn transform_name(name: Name) -> Name {
        name.replace_substring(&["pipe_c1_"], "main.filt.c1.")
            .replace_substring(&["pipe_"], "main.filt.")
            .replace_substring(&["pipe"], "main.filt")
    }
    match &mut stmt {
        // Table and action of an add or default
        Statement::Add { table, action, .. } | Statement::SetDefault { table, action } => {
            *table = transform_name(table.clone());
            *action = action
                .clone()
                .replace_substring(&["pipe_c1_"], "main.filt.c1.")
                .replace_substring(&["pipe_"], "main.filt.")
                .replace_substring(&["_NoAction"], "NoAction")
                .into_unqualified();
        }
        _ => {}
    }
    stmt
}

// == Architectural state

/// The initial architecture state, without a payload.
pub(super) fn init_arch_state<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ebpf>,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Ebpf>,
{
    let typdef_arch = ctx.external().typdef_arch;
    Ok(typdef_arch.make(ctx.arena_mut(), (), Span::default())?)
}

// == Extern calls

// - Initialization

/// Constructs a `CounterArray`; any other object gets an empty state.
pub(super) fn eval_extern_init<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ebpf>,
    values: &[Value],
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Ebpf>,
{
    let typdef_object = ctx.external().typdef_object;
    let (value_name, _value_targs, value_ids, value_args) = value::get::four(values)?;
    let name = value::get::text(ctx.arena(), value_name)?;
    // Only `CounterArray` carries state
    Ok(if name == "CounterArray" {
        let counter = CounterArray::init(ctx.arena_mut(), *value_ids, *value_args)?;
        ExternObject::CounterArray(counter).to_value(ctx.arena_mut(), typdef_object)?
    } else {
        typdef_object.make(ctx.arena_mut(), None, Span::default())?
    })
}

// - Function calls

/// Dispatches an extern function call; only `verify` is supported.
pub(super) fn eval_extern_func_call<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ebpf>,
    values: &[Value],
) -> Result<Vec<Value>, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Ebpf>,
{
    let (value_ctx, value_arch, value_name, value_names) = value::get::four(values)?;
    let name = value::get::text(ctx.arena(), value_name)?.to_owned();
    let names = value::get::list(ctx.arena(), value_names)?
        .iter()
        .map(|value| value::get::text(ctx.arena(), value).map(str::to_owned))
        .collect::<Result<Vec<_>, _>>()?;
    let (value_ctx, value_arch, value_call_result) =
        // Only `verify` is supported
        if name == "verify" && names == ["check", "toSignal"] {
            core_func::verify(ctx, *value_ctx, *value_arch)?
        } else {
            return Err(error::extern_function_unsupported(format!(
                "unsupported extern function call: {name}({})",
                names.join(", ")
            )).into());
        };
    Ok(vec![value_ctx, value_arch, value_call_result])
}

// - Method calls

/// Builds the error naming an unsupported method call.
fn unsupported_method(
    arena: &Arena,
    value_id: Value,
    name: &str,
    names: &[String],
) -> Result<ExternError, ExternError> {
    let ids = value::get::list(arena, &value_id)?
        .iter()
        .map(|value| value::get::text(arena, value).map(str::to_owned))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(error::extern_method_unsupported(format!(
        "unsupported extern method call: {}.{name}({})",
        ids.join("."),
        names.join(", ")
    ))
    .into())
}

/// Dispatches an extern method call on the object named `value_id`.
///
/// The object is read, updated by its method, and written back.
pub(super) fn eval_extern_method_call<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ebpf>,
    values: &[Value],
) -> Result<Vec<Value>, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Ebpf>,
{
    let typdef_object = ctx.external().typdef_object;
    // Context, state, object id, method name, parameter names
    let [value_ctx, value_arch, value_id, value_name, value_names] = values else {
        return Err(error::extern_argument_arity_mismatch(
            "unexpected number of arguments to extern method call".to_owned(),
        )
        .into());
    };
    let value_state = func::find_object_state_e(ctx, *value_arch, *value_id)?;
    let object = ExternObject::from_value(ctx.arena(), typdef_object, &value_state)?;
    let name = value::get::text(ctx.arena(), value_name)?.to_owned();
    let names = value::get::list(ctx.arena(), value_names)?
        .iter()
        .map(|value| value::get::text(ctx.arena(), value).map(str::to_owned))
        .collect::<Result<Vec<_>, _>>()?;
    let (object, value_ctx, value_arch, value_call_result) = match object {
        ExternObject::PacketIn(pkt) => {
            let (object, value_ctx, value_arch, value_call_result) = match (
                name.as_str(),
                names
                    .iter()
                    .map(String::as_str)
                    .collect::<Vec<_>>()
                    .as_slice(),
            ) {
                ("extract", ["hdr"]) => pkt.extract(ctx, *value_ctx, *value_arch)?,
                ("extract", ["variableSizeHeader", "variableFieldSizeInBits"]) => {
                    pkt.extract_varsize(ctx, *value_ctx, *value_arch)?
                }
                ("lookahead", []) => pkt.lookahead(ctx, *value_ctx, *value_arch)?,
                ("advance", ["sizeInBits"]) => pkt.advance(ctx, *value_ctx, *value_arch)?,
                ("length", []) => pkt.length(ctx, *value_ctx, *value_arch)?,
                _ => {
                    return Err(unsupported_method(ctx.arena(), *value_id, &name, &names)?);
                }
            };
            (ExternObject::PacketIn(object), value_ctx, value_arch, value_call_result)
        }
        ExternObject::CounterArray(counter) => {
            let (object, value_ctx, value_arch, value_call_result) = match (
                name.as_str(),
                names
                    .iter()
                    .map(String::as_str)
                    .collect::<Vec<_>>()
                    .as_slice(),
            ) {
                ("increment", ["index"]) => counter.increment(ctx, *value_ctx, *value_arch)?,
                ("add", ["index", "value"]) => counter.add(ctx, *value_ctx, *value_arch)?,
                _ => {
                    return Err(unsupported_method(ctx.arena(), *value_id, &name, &names)?);
                }
            };
            (ExternObject::CounterArray(object), value_ctx, value_arch, value_call_result)
        }
    };
    let value_state = object.to_value(ctx.arena_mut(), typdef_object)?;
    let value_arch = func::update_object_state_e(ctx, value_arch, *value_id, value_state)?;
    Ok(vec![value_ctx, value_arch, value_call_result])
}

// == Pipeline execution

// - Initialization

/// Instantiates the program and returns the initial simulator state.
pub fn init_pipe<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ebpf>,
    program: Value,
) -> Result<SimState, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Ebpf>,
{
    let (value_ctx, value_arch) = pgm::ebpf_init(ctx, program)?;
    Ok(SimState { value_ctx, value_arch, txs: vec![] })
}

// - Execution

/// Processes one received packet: parse, filter, and forward if accepted.
///
/// A parser reject or false `accept` drops the packet; no deparser runs.
pub fn drive_pipe<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ebpf>,
    state: &mut SimState,
    rx: &Rx,
) -> Result<(), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Ebpf>,
{
    let typdef_object = ctx.external().typdef_object;
    state.txs.clear();
    // Setup packet_in extern
    let pkt = ExternObject::PacketIn(PacketIn::init(&rx.packet)?);
    let value_packet = pkt.to_value(ctx.arena_mut(), typdef_object)?;
    let (value_ctx, value_arch) =
        rel::ebpf_init_packet_in(ctx, state.value_ctx, state.value_arch, value_packet)?;
    (state.value_ctx, state.value_arch) = (value_ctx, value_arch);
    // Setup global variables
    state.value_ctx = rel::ebpf_init_globals(ctx, state.value_ctx, state.value_arch)?;
    // Parse block
    let (value_ctx, value_arch, value_call_result) =
        rel::ebpf_parse(ctx, state.value_ctx, state.value_arch)?;
    (state.value_ctx, state.value_arch) = (value_ctx, value_arch);
    let rejected = value::get::matches_string! { ctx.arena_mut(), &value_call_result,
        "REJECT errorValue" => |_values| true,
        _ => false,
    };
    // A rejected packet is dropped
    if rejected {
        return Ok(());
    }
    // Filter block
    let (value_ctx, value_arch, _) = rel::ebpf_filter(ctx, state.value_ctx, state.value_arch)?;
    (state.value_ctx, state.value_arch) = (value_ctx, value_arch);
    // Check if packet is accepted
    let value_accept =
        rel::lvalue_read_var_global(ctx, state.value_ctx, state.value_arch, "accept")?;
    // Forward only when the filter accepted the packet
    if unpack::p4_bool(ctx.arena_mut(), &value_accept)? {
        state
            .txs
            .push(Tx { port: rx.port, packet: rx.packet.clone() });
    }
    Ok(())
}
