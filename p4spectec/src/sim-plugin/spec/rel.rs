//! Helpers for invoking relations in the spec
//!
//! Each wrapper calls a relation by name and unpacks its outputs;
//! pipeline relations return the context, the architecture, and a call result.
//! `Lvalue_read` and `Lvalue_write` take a cursor (`LOCAL` or `GLOBAL`)
//! and a reference.

use crate::lang::data::value::{Value, get};

use crate::runner::{Extern, ExternError, Interface, Interpreter, InterpreterError, RunnerContext};

// == Lvalues

// - Read

/// Reads a global variable.
pub fn lvalue_read_var_global<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
    name: &str,
) -> Result<Value, InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    // A bare name read at the global cursor
    let value_cursor = crate::lang::data::value::make::case_shaped! {
        arena: ctx.arena_mut(),
        shape: "GLOBAL",
        args: vec![],
        typ: "cursor",
        span: crate::lang::common::source::Span::default(),
    }
    .map_err(ExternError::from)?;
    let value_name = super::func::bare_name(ctx.arena_mut(), name)?;
    // The relation returns the single value read
    let values = ctx.call_rel("Lvalue_read", &[value_cursor, value_ctx, value_arch, value_name])?;
    Ok(*get::one(&values).map_err(ExternError::from)?)
}

/// Reads a member of a global variable.
pub fn lvalue_read_dot_global<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
    name: &str,
    member: &str,
) -> Result<Value, InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    use crate::lang::{common::source::Span, data::value::make};
    let value_cursor = make::case_shaped! {
        arena: ctx.arena_mut(),
        shape: "GLOBAL",
        args: vec![],
        typ: "cursor",
        span: Span::default(),
    }
    .map_err(ExternError::from)?;
    let value_base = super::func::bare_name(ctx.arena_mut(), name)?;
    let value_member = make::text(ctx.arena_mut(), member.to_owned(), Span::default())
        .map_err(ExternError::from)?;
    // The reference is `name.member`
    let value_ref = make::case_shaped! {
        arena: ctx.arena_mut(),
        shape: "storageReference '.' nameIR",
        args: vec![value_base, value_member],
        typ: "storageReference",
        span: Span::default(),
    }
    .map_err(ExternError::from)?;
    // The relation returns the single value read
    let values = ctx.call_rel("Lvalue_read", &[value_cursor, value_ctx, value_arch, value_ref])?;
    Ok(*get::one(&values).map_err(ExternError::from)?)
}

// - Write

/// Writes a local variable; returns the new context.
pub fn lvalue_write_var_local<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
    name: &str,
    value: Value,
) -> Result<Value, InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    // Writing returns the updated context
    let value_cursor = super::func::local_cursor(ctx.arena_mut())?;
    let value_name = super::func::bare_name(ctx.arena_mut(), name)?;
    let values =
        ctx.call_rel("Lvalue_write", &[value_cursor, value_ctx, value_arch, value_name, value])?;
    Ok(*get::one(&values).map_err(ExternError::from)?)
}

/// Writes a member of a local variable; returns the new context.
pub fn lvalue_write_dot_local<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
    name: &str,
    member: &str,
    value: Value,
) -> Result<Value, InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    use crate::lang::{common::source::Span, data::value::make};
    let value_cursor = make::case_shaped! {
        arena: ctx.arena_mut(),
        shape: "LOCAL",
        args: vec![],
        typ: "cursor",
        span: Span::default(),
    }
    .map_err(ExternError::from)?;
    let value_base = super::func::bare_name(ctx.arena_mut(), name)?;
    let value_member = make::text(ctx.arena_mut(), member.to_owned(), Span::default())
        .map_err(ExternError::from)?;
    // The reference is `name.member`
    let value_ref = make::case_shaped! {
        arena: ctx.arena_mut(),
        shape: "storageReference '.' nameIR",
        args: vec![value_base, value_member],
        typ: "storageReference",
        span: Span::default(),
    }
    .map_err(ExternError::from)?;
    // Writing returns the updated context
    let values =
        ctx.call_rel("Lvalue_write", &[value_cursor, value_ctx, value_arch, value_ref, value])?;
    Ok(*get::one(&values).map_err(ExternError::from)?)
}

/// Writes a member of a global variable; returns the new context.
pub fn lvalue_write_dot_global<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
    name: &str,
    member: &str,
    value: Value,
) -> Result<Value, InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    use crate::lang::{common::source::Span, data::value::make};
    let value_cursor = make::case_shaped! {
        arena: ctx.arena_mut(),
        shape: "GLOBAL",
        args: vec![],
        typ: "cursor",
        span: Span::default(),
    }
    .map_err(ExternError::from)?;
    let value_base = super::func::bare_name(ctx.arena_mut(), name)?;
    let value_member = make::text(ctx.arena_mut(), member.to_owned(), Span::default())
        .map_err(ExternError::from)?;
    // The reference is `name.member`
    let value_ref = make::case_shaped! {
        arena: ctx.arena_mut(),
        shape: "storageReference '.' nameIR",
        args: vec![value_base, value_member],
        typ: "storageReference",
        span: Span::default(),
    }
    .map_err(ExternError::from)?;
    // Writing returns the updated context
    let values =
        ctx.call_rel("Lvalue_write", &[value_cursor, value_ctx, value_arch, value_ref, value])?;
    Ok(*get::one(&values).map_err(ExternError::from)?)
}

// == eBPF

// - Initialization

/// Installs the input packet.
pub fn ebpf_init_packet_in<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
    value_packet: Value,
) -> Result<(Value, Value), InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let values = ctx.call_rel("EBPF_init_packet_in", &[value_ctx, value_arch, value_packet])?;
    let (value_ctx, value_arch) = get::two(&values).map_err(ExternError::from)?;
    Ok((*value_ctx, *value_arch))
}

/// Initializes the global variables.
pub fn ebpf_init_globals<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
) -> Result<Value, InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let values = ctx.call_rel("EBPF_init_globals", &[value_ctx, value_arch])?;
    Ok(*get::one(&values).map_err(ExternError::from)?)
}

// - Pipeline

/// Runs the parser.
pub fn ebpf_parse<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
) -> Result<(Value, Value, Value), InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let values = ctx.call_rel("EBPF_parse", &[value_ctx, value_arch])?;
    let (value_ctx, value_arch, value_call_result) =
        get::three(&values).map_err(ExternError::from)?;
    Ok((*value_ctx, *value_arch, *value_call_result))
}

/// Runs the filter control.
pub fn ebpf_filter<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
) -> Result<(Value, Value, Value), InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let values = ctx.call_rel("EBPF_filter", &[value_ctx, value_arch])?;
    let (value_ctx, value_arch, value_call_result) =
        get::three(&values).map_err(ExternError::from)?;
    Ok((*value_ctx, *value_arch, *value_call_result))
}

// == PSA

// - Ingress initialization

/// Installs the ingress input packet.
pub fn psa_ingress_init_packet_in<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
    value_packet: Value,
) -> Result<(Value, Value), InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let values =
        ctx.call_rel("PSA_ingress_init_packet_in", &[value_ctx, value_arch, value_packet])?;
    let (value_ctx, value_arch) = get::two(&values).map_err(ExternError::from)?;
    Ok((*value_ctx, *value_arch))
}

/// Installs the ingress output packet.
pub fn psa_ingress_init_packet_out<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
    value_packet: Value,
) -> Result<(Value, Value), InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let values =
        ctx.call_rel("PSA_ingress_init_packet_out", &[value_ctx, value_arch, value_packet])?;
    let (value_ctx, value_arch) = get::two(&values).map_err(ExternError::from)?;
    Ok((*value_ctx, *value_arch))
}

/// Initializes ingress globals for an input port.
pub fn psa_ingress_init_globals<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
    port: usize,
) -> Result<Value, InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    // The port becomes an integer value
    let value_port = crate::lang::data::value::make::int(
        ctx.arena_mut(),
        port.into(),
        crate::lang::common::source::Span::default(),
    )
    .map_err(ExternError::from)?;
    let values = ctx.call_rel("PSA_ingress_init_globals", &[value_ctx, value_arch, value_port])?;
    Ok(*get::one(&values).map_err(ExternError::from)?)
}

/// Initializes ingress metadata for an input port and packet path.
pub fn psa_ingress_init_metadata<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
    port: usize,
    path: &str,
) -> Result<Value, InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    use crate::lang::{common::source::Span, data::value::make};
    let value_port =
        make::int(ctx.arena_mut(), port.into(), Span::default()).map_err(ExternError::from)?;
    let value_path =
        make::text(ctx.arena_mut(), path.to_owned(), Span::default()).map_err(ExternError::from)?;
    let values = ctx
        .call_rel("PSA_ingress_init_metadata", &[value_ctx, value_arch, value_port, value_path])?;
    Ok(*get::one(&values).map_err(ExternError::from)?)
}

// - Ingress pipeline

/// Runs the ingress parser.
pub fn psa_ingress_parser<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
) -> Result<(Value, Value, Value), InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let values = ctx.call_rel("PSA_ingress_parser", &[value_ctx, value_arch])?;
    let (value_ctx, value_arch, value_call_result) =
        get::three(&values).map_err(ExternError::from)?;
    Ok((*value_ctx, *value_arch, *value_call_result))
}

/// Runs the ingress control.
pub fn psa_ingress<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
) -> Result<(Value, Value, Value), InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let values = ctx.call_rel("PSA_ingress", &[value_ctx, value_arch])?;
    let (value_ctx, value_arch, value_call_result) =
        get::three(&values).map_err(ExternError::from)?;
    Ok((*value_ctx, *value_arch, *value_call_result))
}

/// Runs the ingress deparser.
pub fn psa_ingress_deparser<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
) -> Result<(Value, Value, Value), InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let values = ctx.call_rel("PSA_ingress_deparser", &[value_ctx, value_arch])?;
    let (value_ctx, value_arch, value_call_result) =
        get::three(&values).map_err(ExternError::from)?;
    Ok((*value_ctx, *value_arch, *value_call_result))
}

// - Egress initialization

/// Installs the egress input packet.
pub fn psa_egress_init_packet_in<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
    value_packet: Value,
) -> Result<(Value, Value), InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let values =
        ctx.call_rel("PSA_egress_init_packet_in", &[value_ctx, value_arch, value_packet])?;
    let (value_ctx, value_arch) = get::two(&values).map_err(ExternError::from)?;
    Ok((*value_ctx, *value_arch))
}

/// Installs the egress output packet.
pub fn psa_egress_init_packet_out<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
    value_packet: Value,
) -> Result<(Value, Value), InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let values =
        ctx.call_rel("PSA_egress_init_packet_out", &[value_ctx, value_arch, value_packet])?;
    let (value_ctx, value_arch) = get::two(&values).map_err(ExternError::from)?;
    Ok((*value_ctx, *value_arch))
}

/// Initializes egress globals for an output port.
pub fn psa_egress_init_globals<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
    port: usize,
) -> Result<Value, InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    // The port becomes an integer value
    let value_port = crate::lang::data::value::make::int(
        ctx.arena_mut(),
        port.into(),
        crate::lang::common::source::Span::default(),
    )
    .map_err(ExternError::from)?;
    let values = ctx.call_rel("PSA_egress_init_globals", &[value_ctx, value_arch, value_port])?;
    Ok(*get::one(&values).map_err(ExternError::from)?)
}

/// Initializes egress metadata: port, packet path, class of service, instance.
pub fn psa_egress_init_metadata<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
    port: usize,
    path: &str,
    cos: usize,
    instance: usize,
) -> Result<Value, InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    use crate::lang::{common::source::Span, data::value::make};
    // Egress metadata also carries class of service and instance
    let value_port =
        make::int(ctx.arena_mut(), port.into(), Span::default()).map_err(ExternError::from)?;
    let value_path =
        make::text(ctx.arena_mut(), path.to_owned(), Span::default()).map_err(ExternError::from)?;
    let value_cos =
        make::int(ctx.arena_mut(), cos.into(), Span::default()).map_err(ExternError::from)?;
    let value_instance =
        make::int(ctx.arena_mut(), instance.into(), Span::default()).map_err(ExternError::from)?;
    let values = ctx.call_rel(
        "PSA_egress_init_metadata",
        &[value_ctx, value_arch, value_port, value_path, value_cos, value_instance],
    )?;
    Ok(*get::one(&values).map_err(ExternError::from)?)
}

// - Egress pipeline

/// Runs the egress parser.
pub fn psa_egress_parser<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
) -> Result<(Value, Value, Value), InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let values = ctx.call_rel("PSA_egress_parser", &[value_ctx, value_arch])?;
    let (value_ctx, value_arch, value_call_result) =
        get::three(&values).map_err(ExternError::from)?;
    Ok((*value_ctx, *value_arch, *value_call_result))
}

/// Runs the egress control.
pub fn psa_egress<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
) -> Result<(Value, Value, Value), InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let values = ctx.call_rel("PSA_egress", &[value_ctx, value_arch])?;
    let (value_ctx, value_arch, value_call_result) =
        get::three(&values).map_err(ExternError::from)?;
    Ok((*value_ctx, *value_arch, *value_call_result))
}

/// Runs the egress deparser.
pub fn psa_egress_deparser<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
) -> Result<(Value, Value, Value), InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let values = ctx.call_rel("PSA_egress_deparser", &[value_ctx, value_arch])?;
    let (value_ctx, value_arch, value_call_result) =
        get::three(&values).map_err(ExternError::from)?;
    Ok((*value_ctx, *value_arch, *value_call_result))
}

// == v1model

// - Initialization

/// Installs the input packet.
pub fn v1model_init_packet_in<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
    value_packet: Value,
) -> Result<(Value, Value), InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let values = ctx.call_rel("V1Model_init_packet_in", &[value_ctx, value_arch, value_packet])?;
    let (value_ctx, value_arch) = get::two(&values).map_err(ExternError::from)?;
    Ok((*value_ctx, *value_arch))
}

/// Installs the output packet.
pub fn v1model_init_packet_out<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
    value_packet: Value,
) -> Result<(Value, Value), InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let values = ctx.call_rel("V1Model_init_packet_out", &[value_ctx, value_arch, value_packet])?;
    let (value_ctx, value_arch) = get::two(&values).map_err(ExternError::from)?;
    Ok((*value_ctx, *value_arch))
}

/// Initializes globals for an input port.
pub fn v1model_init_globals<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
    port: usize,
) -> Result<Value, InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    // The port becomes an integer value
    let value_port = crate::lang::data::value::make::int(
        ctx.arena_mut(),
        port.into(),
        crate::lang::common::source::Span::default(),
    )
    .map_err(ExternError::from)?;
    let values = ctx.call_rel("V1Model_init_globals", &[value_ctx, value_arch, value_port])?;
    Ok(*get::one(&values).map_err(ExternError::from)?)
}

// - Pipeline

/// Runs the parser.
pub fn v1model_parser<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
) -> Result<(Value, Value, Value), InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let values = ctx.call_rel("V1Model_parser", &[value_ctx, value_arch])?;
    let (value_ctx, value_arch, value_call_result) =
        get::three(&values).map_err(ExternError::from)?;
    Ok((*value_ctx, *value_arch, *value_call_result))
}

/// Runs the checksum verification control.
pub fn v1model_verify<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
) -> Result<(Value, Value, Value), InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let values = ctx.call_rel("V1Model_verify", &[value_ctx, value_arch])?;
    let (value_ctx, value_arch, value_call_result) =
        get::three(&values).map_err(ExternError::from)?;
    Ok((*value_ctx, *value_arch, *value_call_result))
}

/// Runs the ingress control.
pub fn v1model_ingress<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
) -> Result<(Value, Value, Value), InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let values = ctx.call_rel("V1Model_ingress", &[value_ctx, value_arch])?;
    let (value_ctx, value_arch, value_call_result) =
        get::three(&values).map_err(ExternError::from)?;
    Ok((*value_ctx, *value_arch, *value_call_result))
}

/// Runs the egress control.
pub fn v1model_egress<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
) -> Result<(Value, Value, Value), InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let values = ctx.call_rel("V1Model_egress", &[value_ctx, value_arch])?;
    let (value_ctx, value_arch, value_call_result) =
        get::three(&values).map_err(ExternError::from)?;
    Ok((*value_ctx, *value_arch, *value_call_result))
}

/// Runs the checksum computation control.
pub fn v1model_check<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
) -> Result<(Value, Value, Value), InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let values = ctx.call_rel("V1Model_check", &[value_ctx, value_arch])?;
    let (value_ctx, value_arch, value_call_result) =
        get::three(&values).map_err(ExternError::from)?;
    Ok((*value_ctx, *value_arch, *value_call_result))
}

/// Runs the deparser.
pub fn v1model_deparse<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
) -> Result<(Value, Value, Value), InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let values = ctx.call_rel("V1Model_deparse", &[value_ctx, value_arch])?;
    let (value_ctx, value_arch, value_call_result) =
        get::three(&values).map_err(ExternError::from)?;
    Ok((*value_ctx, *value_arch, *value_call_result))
}

// - Preserved metadata

/// Restores the metadata fields preserved across a resubmit or clone.
pub fn v1model_setup_preserved_meta_fields<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
    value_idx: Value,
) -> Result<Value, InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let values =
        ctx.call_rel("V1Model_setup_preserved_meta_fields", &[value_ctx, value_arch, value_idx])?;
    Ok(*get::one(&values).map_err(ExternError::from)?)
}
