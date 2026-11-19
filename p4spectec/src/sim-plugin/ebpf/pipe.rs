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

use crate::lang::{
    common::source::Span,
    data::{
        arena::Arena,
        value::{
            flat::{self as value, Value},
            host::HostTypDef,
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

/// The eBPF architecture, with its definitions of the extern types.
pub struct Ebpf {
    /// The definition of `archState`, which eBPF leaves empty.
    pub(super) arch_state: HostTypDef<()>,
    /// The definition of `objectState`; a stateless object carries `None`.
    pub(super) object_state: HostTypDef<Option<ExternObject>>,
}

// Every instance makes definitions of its own
impl Default for Ebpf {
    fn default() -> Self {
        Self {
            arch_state: HostTypDef::new("archState"),
            object_state: HostTypDef::new("objectState"),
        }
    }
}

// == Extern objects

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
/// Core and eBPF-specific extern objects.
pub enum ExternObject {
    /// The `packet_in` being parsed.
    PacketIn(PacketIn),
    /// A `CounterArray`.
    CounterArray(CounterArray),
}

impl ExternObject {
    // - Wrapping

    /// Wraps the object as an `objectState` value of `typdef`.
    pub fn to_value(
        self,
        arena: &mut Arena,
        typdef: HostTypDef<Option<Self>>,
    ) -> Result<Value, ExternError> {
        Self::wrap(Some(self), arena, typdef)
    }

    /// Wraps an object's state, or its absence, as an `objectState` value of `typdef`.
    fn wrap(
        object: Option<Self>,
        arena: &mut Arena,
        typdef: HostTypDef<Option<Self>>,
    ) -> Result<Value, ExternError> {
        Ok(value::make::external(arena, typdef.payload(object), Span::default())?)
    }

    // - Reading

    /// Reads an object from an `objectState` value of `typdef`; a stateless object fails.
    pub fn from_value(
        arena: &Arena,
        value: &Value,
        typdef: HostTypDef<Option<Self>>,
    ) -> Result<Self, ExternError> {
        value::get::external(arena, typdef, value)?
            .clone()
            .ok_or_else(|| {
                error::extern_object_undefined("extern object has no state".to_owned()).into()
            })
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

/// The initial architecture state: the unit payload.
pub(super) fn init_arch_state<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ebpf>,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Ebpf>,
{
    let payload = ctx.external().arch_state.payload(());
    Ok(value::make::external(ctx.arena_mut(), payload, Span::default())?)
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
    let (value_name, _value_targs, value_ids, value_args) = value::get::four(values)?;
    let name = value::get::text(ctx.arena(), value_name)?;
    // Only `CounterArray` carries state
    let object = if name == "CounterArray" {
        let counter = CounterArray::init(ctx.arena_mut(), *value_ids, *value_args)?;
        Some(ExternObject::CounterArray(counter))
    } else {
        None
    };
    let typdef = ctx.external().object_state;
    ExternObject::wrap(object, ctx.arena_mut(), typdef)
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
/// The object is decoded, updated by its method, and written back.
pub(super) fn eval_extern_method_call<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ebpf>,
    values: &[Value],
) -> Result<Vec<Value>, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Ebpf>,
{
    // Context, state, object id, method name, parameter names
    let [value_ctx, value_arch, value_id, value_name, value_names] = values else {
        return Err(error::extern_argument_arity_mismatch(
            "unexpected number of arguments to extern method call".to_owned(),
        )
        .into());
    };
    let value_state = func::find_object_state_e(ctx, *value_arch, *value_id)?;
    let object = ExternObject::from_value(ctx.arena(), &value_state, ctx.external().object_state)?;
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
    let typdef = ctx.external().object_state;
    let value_state = object.to_value(ctx.arena_mut(), typdef)?;
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
    state.txs.clear();
    // Setup packet_in extern
    let pkt = ExternObject::PacketIn(PacketIn::init(&rx.packet)?);
    let typdef = ctx.external().object_state;
    let value_packet = pkt.to_value(ctx.arena_mut(), typdef)?;
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
