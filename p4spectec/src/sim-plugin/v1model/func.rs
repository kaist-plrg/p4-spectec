//! v1model extern functions
//!
//! Doc comments summarize the extern's description in `v1model.p4`.
//! random, clone, truncate, assert, and assume are not implemented;
//! the pipeline's dispatch rejects them as unsupported calls.

use num_bigint::BigInt;
use num_traits::Zero;

use crate::util::bigint::remainder;

use crate::lang::{
    common::source::Span,
    data::{
        arena::Arena,
        typ,
        value::flat::{self as value, Value},
    },
    traits::print::Print,
};

use crate::runner::{Extern, ExternError, Interface, Interpreter, RunnerContext};

use crate::sim_plugin::{
    core::object::PacketIn,
    error, hash,
    spec::{func, pack, rel, unpack},
};

use super::{V1Model, packet::CloneInfo, pipe};

/// Calling digest causes a message containing the values specified in
/// the data parameter to be sent to the control plane software.  It is
/// similar to sending a clone of the packet to the control plane
/// software, except that it can be more efficient because the messages
/// are typically smaller than packets, and many such small digest
/// messages are typically coalesced together into a larger "batch"
/// which the control plane software processes all at once.
///
/// The value of the fields that are sent in the message to the control
/// plane is the value they have at the time the digest call occurs,
/// even if those field values are changed by later ingress control
/// code.  See Note 3.
///
/// Calling digest is only supported in the ingress control.  There is
/// no way to undo its effects once it has been called.
///
/// If the type T is a named struct, the name is used to generate the
/// control plane API.
///
/// The BMv2 implementation of the v1model architecture ignores the
/// value of the receiver parameter.
///
/// `extern void digest<T>(in bit<32> receiver, in T data);`
pub fn digest<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
) -> Result<(Value, Value, Value), ExternError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    // No-op in the source simulator
    // Return without a value
    let typ = typ::make::opt(typ::make::var(
        crate::phrase!(node: "value".to_owned(), span: Span::default()),
        Vec::new(),
    ));
    let value_opt = value::make::opt(ctx.arena_mut(), typ.node.into(), None, Span::default())?;
    let value_call_result = value::make::case_shaped! {
        arena: ctx.arena_mut(),
        shape: "RETURN value?",
        args: vec![value_opt],
        typ: "returnResult",
        span: Span::default(),
    }?;
    Ok((value_ctx, value_arch, value_call_result))
}

/// mark_to_drop(`standard_metadata`) is a primitive action that modifies
/// `standard_metadata.egress_spec` to an implementation-specific special
/// value that in some cases causes the packet to be dropped at the end
/// of ingress or egress processing. It also asssigs 0 to
/// `standard_metadata.mcast_grp`. Either of those metadata fields may
/// be changed by executing later P4 code, after calling
/// `mark_to_drop()`, and this can change the resulting behavior of the
/// packet to do something other than drop.
///
/// `extern void mark_to_drop(inout standard_metadata_t standard_metadata);`
pub fn mark_to_drop<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
) -> Result<(Value, Value, Value), ExternError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    // The drop port is 511 on 9 bits
    let value_egress_spec = pack::p4_fixed_bit(ctx.arena_mut(), 9.into(), 511.into())?;
    let value_ctx = rel::lvalue_write_dot_local(
        ctx,
        value_ctx,
        value_arch,
        "standard_metadata",
        "egress_spec",
        value_egress_spec,
    )?;
    // No multicast for a dropped packet
    let value_mcast_grp = pack::p4_fixed_bit(ctx.arena_mut(), 16.into(), 0.into())?;
    let value_ctx = rel::lvalue_write_dot_local(
        ctx,
        value_ctx,
        value_arch,
        "standard_metadata",
        "mcast_grp",
        value_mcast_grp,
    )?;
    // Return without a value
    let typ = typ::make::opt(typ::make::var(
        crate::phrase!(node: "value".to_owned(), span: Span::default()),
        Vec::new(),
    ));
    let value_opt = value::make::opt(ctx.arena_mut(), typ.node.into(), None, Span::default())?;
    let value_call_result = value::make::case_shaped! {
        arena: ctx.arena_mut(),
        shape: "RETURN value?",
        args: vec![value_opt],
        typ: "returnResult",
        span: Span::default(),
    }?;
    Ok((value_ctx, value_arch, value_call_result))
}

/// Calculate a hash function of the value specified by the data
/// parameter.  The value written to the out parameter named result
/// will always be in the range [base, base+max-1] inclusive, if max >=
/// 1.  If max=0, the value written to result will always be base.
///
/// Note that the types of all of the parameters may be the same as, or
/// different from, each other, and thus their bit widths are allowed
/// to be different.
///
/// @param O          Must be a type `bit<W>`
/// @param D          Must be a tuple type where all the fields are bit-fields
///                   (type `bit<W>` or `int<W>`) or varbits.
/// @param T          Must be a type `bit<W>`
/// @param M          Must be a type `bit<W>`
///
/// `extern void hash<O, T, D, M>(out O result, in HashAlgorithm algo,`
///                              `in T base, in D data, in M max);`
pub fn hash<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
) -> Result<(Value, Value, Value), ExternError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let value_base = func::find_var_e_local(ctx, value_ctx, "base")?;
    let int_base = unpack::p4_fixed_bit(ctx.arena(), &value_base)?.1;
    let value_max = func::find_var_e_local(ctx, value_ctx, "max")?;
    let int_max = unpack::p4_fixed_bit(ctx.arena(), &value_max)?.1;
    let int = compute_checksum(ctx, value_ctx, None)?;
    // The source simulator uses max - base as the range divisor
    let int = adjust(&int_base, &int_max, &int)?;
    // Cast the arbitrary-precision result to the output type `O`
    let value_typ = func::find_type_e_local(ctx, value_ctx, "O")?;
    let value_result = pack::p4_arbitrary_int(ctx.arena_mut(), int)?;
    let value_result = func::cast_op(ctx, value_typ, value_result)?;
    let value_ctx =
        rel::lvalue_write_var_local(ctx, value_ctx, value_arch, "result", value_result)?;
    // Return without a value
    let typ = typ::make::opt(typ::make::var(
        crate::phrase!(node: "value".to_owned(), span: Span::default()),
        Vec::new(),
    ));
    let value_opt = value::make::opt(ctx.arena_mut(), typ.node.into(), None, Span::default())?;
    let value_call_result = value::make::case_shaped! {
        arena: ctx.arena_mut(),
        shape: "RETURN value?",
        args: vec![value_opt],
        typ: "returnResult",
        span: Span::default(),
    }?;
    Ok((value_ctx, value_arch, value_call_result))
}

/// Folds `int` into `[base, rmax)`, or returns `base` when `rmax` is zero.
pub fn adjust(base: &BigInt, rmax: &BigInt, int: &BigInt) -> Result<BigInt, ExternError> {
    // max = 0: the result is always base
    if rmax.is_zero() {
        return Ok(base.clone());
    }
    let int_range = rmax - base;
    // The divisor max - base must be positive
    if int_range <= BigInt::zero() {
        return Err(
            error::hash_range_invalid("hash range divisor must be positive".to_owned()).into()
        );
    }
    Ok(remainder(int, &int_range) + base)
}

/// Hashes the `data` tuple, plus the unparsed payload if given, with `algo`.
fn compute_checksum<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    payload: Option<&PacketIn>,
) -> Result<BigInt, ExternError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let value_data = func::find_var_e_local(ctx, value_ctx, "data")?;
    let mut values = unpack::p4_tuple(ctx.arena(), &value_data)?;
    // The payload is appended as 8-bit values
    if let Some(packet_in) = payload {
        for byte in packet_in.payload_bytes()? {
            values.push(pack::p4_fixed_bit(ctx.arena_mut(), 8.into(), byte)?);
        }
    }
    let value_algo = func::find_var_e_local(ctx, value_ctx, "algo")?;
    let (id_enum, id_field) = unpack::p4_enum(ctx.arena(), &value_algo)?;
    // Only a `HashAlgorithm` enumerator selects the algorithm
    if id_enum != "HashAlgorithm" {
        return Err(error::hash_algorithm_invalid(format!(
            "invalid HashAlgorithm enum value: {id_enum}.{id_field}"
        ))
        .into());
    }
    hash::compute_checksum(ctx.arena(), &id_field, None, &values)
}

/// Shared body of `verify_checksum` and its `_with_payload` variant.
fn do_verify_checksum<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
    payload: Option<&PacketIn>,
) -> Result<(Value, Value, Value), ExternError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let value_condition = func::find_var_e_local(ctx, value_ctx, "condition")?;
    // A false condition skips the checksum
    if !unpack::p4_bool(ctx.arena(), &value_condition)? {
        // Return without a value
        let typ = typ::make::opt(typ::make::var(
            crate::phrase!(node: "value".to_owned(), span: Span::default()),
            Vec::new(),
        ));
        let value_opt = value::make::opt(ctx.arena_mut(), typ.node.into(), None, Span::default())?;
        let value_call_result = value::make::case_shaped! {
            arena: ctx.arena_mut(),
            shape: "RETURN value?",
            args: vec![value_opt],
            typ: "returnResult",
            span: Span::default(),
        }?;
        return Ok((value_ctx, value_arch, value_call_result));
    }
    let value_checksum = func::find_var_e_local(ctx, value_ctx, "checksum")?;
    let int_expect = unpack::p4_fixed_bit(ctx.arena(), &value_checksum)?.1;
    let int_actual = compute_checksum(ctx, value_ctx, payload)?;
    // A mismatch sets `checksum_error` to 1
    let value_ctx = if int_expect == int_actual {
        value_ctx
    } else {
        let value_error = pack::p4_fixed_bit(ctx.arena_mut(), 1.into(), 1.into())?;
        rel::lvalue_write_dot_global(
            ctx,
            value_ctx,
            value_arch,
            "standard_metadata",
            "checksum_error",
            value_error,
        )?
    };
    // Return without a value
    let typ = typ::make::opt(typ::make::var(
        crate::phrase!(node: "value".to_owned(), span: Span::default()),
        Vec::new(),
    ));
    let value_opt = value::make::opt(ctx.arena_mut(), typ.node.into(), None, Span::default())?;
    let value_call_result = value::make::case_shaped! {
        arena: ctx.arena_mut(),
        shape: "RETURN value?",
        args: vec![value_opt],
        typ: "returnResult",
        span: Span::default(),
    }?;
    Ok((value_ctx, value_arch, value_call_result))
}

/// Verifies the checksum of the supplied data.  If this method detects
/// that a checksum of the data is not correct, then the value of the
/// `standard_metadata` checksum_error field will be equal to 1 when the
/// packet begins ingress processing.
///
/// Calling verify_checksum is only supported in the VerifyChecksum
/// control.
///
/// @param T          Must be a tuple type where all the tuple elements
///                   are of type `bit<W>`, `int<W>`, or `varbit<W>`.  The
///                   total length of the fields must be a multiple of
///                   the output size.
/// @param O          Checksum type; must be `bit<X>` type.
/// @param condition  If 'false' the verification always succeeds.
/// @param data       Data whose checksum is verified.
/// @param checksum   Expected checksum of the data; note that it must
///                   be a left-value.
/// @param algo       Algorithm to use for checksum (not all algorithms
///                   may be supported).  Must be a compile-time
///                   constant.
///
/// `extern void verify_checksum<T, O>(in bool condition, in T data,`
///                                   `in O checksum, HashAlgorithm algo);`
///
/// verify_checksum_with_payload is identical in all ways to
/// verify_checksum, except that it includes the payload of the packet
/// in the checksum calculation.  The payload is defined as "all bytes
/// of the packet which were not parsed by the parser".
///
/// Calling verify_checksum_with_payload is only supported in the
/// VerifyChecksum control.
///
/// `extern void verify_checksum_with_payload<T, O>(`
/// `    in bool condition, in T data, in O checksum, HashAlgorithm algo);`
pub fn verify_checksum<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
) -> Result<(Value, Value, Value), ExternError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    do_verify_checksum(ctx, value_ctx, value_arch, None)
}

/// `verify_checksum` over `data` plus the packet's unparsed payload.
///
/// Only supported in the VerifyChecksum control.
pub fn verify_checksum_with_payload<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
    packet_in: &PacketIn,
) -> Result<(Value, Value, Value), ExternError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    do_verify_checksum(ctx, value_ctx, value_arch, Some(packet_in))
}

/// Shared body of `update_checksum` and its `_with_payload` variant.
fn do_update_checksum<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
    payload: Option<&PacketIn>,
) -> Result<(Value, Value, Value), ExternError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let value_condition = func::find_var_e_local(ctx, value_ctx, "condition")?;
    // A false condition skips the checksum
    if !unpack::p4_bool(ctx.arena(), &value_condition)? {
        // Return without a value
        let typ = typ::make::opt(typ::make::var(
            crate::phrase!(node: "value".to_owned(), span: Span::default()),
            Vec::new(),
        ));
        let value_opt = value::make::opt(ctx.arena_mut(), typ.node.into(), None, Span::default())?;
        let value_call_result = value::make::case_shaped! {
            arena: ctx.arena_mut(),
            shape: "RETURN value?",
            args: vec![value_opt],
            typ: "returnResult",
            span: Span::default(),
        }?;
        return Ok((value_ctx, value_arch, value_call_result));
    }
    let int = compute_checksum(ctx, value_ctx, payload)?;
    // Cast the arbitrary-precision result to the output type `O`
    let value_typ = func::find_type_e_local(ctx, value_ctx, "O")?;
    let value_checksum = pack::p4_arbitrary_int(ctx.arena_mut(), int)?;
    let value_checksum = func::cast_op(ctx, value_typ, value_checksum)?;
    let value_ctx =
        rel::lvalue_write_var_local(ctx, value_ctx, value_arch, "checksum", value_checksum)?;
    // Return without a value
    let typ = typ::make::opt(typ::make::var(
        crate::phrase!(node: "value".to_owned(), span: Span::default()),
        Vec::new(),
    ));
    let value_opt = value::make::opt(ctx.arena_mut(), typ.node.into(), None, Span::default())?;
    let value_call_result = value::make::case_shaped! {
        arena: ctx.arena_mut(),
        shape: "RETURN value?",
        args: vec![value_opt],
        typ: "returnResult",
        span: Span::default(),
    }?;
    Ok((value_ctx, value_arch, value_call_result))
}

/// Computes the checksum of the supplied data and writes it to the
/// checksum parameter.
///
/// Calling update_checksum is only supported in the ComputeChecksum
/// control.
///
/// @param T          Must be a tuple type where all the tuple elements
///                   are of type `bit<W>`, `int<W>`, or `varbit<W>`.  The
///                   total length of the fields must be a multiple of
///                   the output size.
/// @param O          Output type; must be `bit<X>` type.
/// @param condition  If 'false' the checksum parameter is not changed
/// @param data       Data whose checksum is computed.
/// @param checksum   Checksum of the data.
/// @param algo       Algorithm to use for checksum (not all algorithms
///                   may be supported).  Must be a compile-time
///                   constant.
///
/// `extern void update_checksum<T, O>(in bool condition, in T data,`
///                                   `inout O checksum, HashAlgorithm algo);`
///
/// update_checksum_with_payload is identical in all ways to
/// update_checksum, except that it includes the payload of the packet
/// in the checksum calculation.  The payload is defined as "all bytes
/// of the packet which were not parsed by the parser".
///
/// Calling update_checksum_with_payload is only supported in the
/// ComputeChecksum control.
///
/// `extern void update_checksum_with_payload<T, O>(`
/// `    in bool condition, in T data, inout O checksum, HashAlgorithm algo);`
pub fn update_checksum<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
) -> Result<(Value, Value, Value), ExternError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    do_update_checksum(ctx, value_ctx, value_arch, None)
}

/// `update_checksum` over `data` plus the packet's unparsed payload.
///
/// Only supported in the ComputeChecksum control.
pub fn update_checksum_with_payload<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
    packet_in: &PacketIn,
) -> Result<(Value, Value, Value), ExternError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    do_update_checksum(ctx, value_ctx, value_arch, Some(packet_in))
}

/// Calling resubmit_preserving_field_list during execution of the
/// ingress control will cause the packet to be resubmitted, i.e. it
/// will begin processing again with the parser, with the contents of
/// the packet exactly as they were when it last began parsing.  The
/// only difference is in the value of the `standard_metadata`
/// instance_type field, and any user-defined metadata fields that the
/// resubmit_preserving_field_list operation causes to be preserved.
///
/// The user metadata fields that are tagged with `@field_list(index)` will
/// be sent to the parser together with the packet.
///
/// Calling resubmit_preserving_field_list is only supported in the
/// ingress control.  There is no way to undo its effects once it has
/// been called.  If resubmit_preserving_field_list is called multiple
/// times during a single execution of the ingress control, only one
/// packet is resubmitted, and only the user-defined metadata fields
/// specified by the field list index from the last such call are
/// preserved.  See the v1model architecture documentation (Note 1) for
/// more details.
///
/// For example, the user metadata fields can be annotated as follows:
/// ```text
/// struct UM {
///    @field_list(1)
///    bit<32> x;
///    @field_list(1, 2)
///    bit<32> y;
///    bit<32> z;
/// }
/// ```
///
/// Calling `resubmit_preserving_field_list(1)` will resubmit the packet
/// and preserve fields x and y of the user metadata.  Calling
/// `resubmit_preserving_field_list(2)` will only preserve field y.
///
/// `extern void resubmit_preserving_field_list(bit<8> index);`
pub fn resubmit_preserving_field_list<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    value_ctx: Value,
    value_arch: Value,
) -> Result<(Value, Value, Value), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    let value_idx = func::find_var_e_local(ctx, value_ctx, "index")?;
    let idx = usize::try_from(&unpack::p4_fixed_bit(ctx.arena(), &value_idx)?.1)?;
    // The scheduler acts on the request after the control returns
    let mut arch = pipe::find_arch_state(ctx, value_arch)?;
    arch.action.resubmit_opt = Some(idx);
    let value_arch = pipe::update_arch_state(ctx, value_arch, &arch)?;
    // Return without a value
    let typ = typ::make::opt(typ::make::var(
        crate::phrase!(node: "value".to_owned(), span: Span::default()),
        Vec::new(),
    ));
    let value_opt = value::make::opt(ctx.arena_mut(), typ.node.into(), None, Span::default())?;
    let value_call_result = value::make::case_shaped! {
        arena: ctx.arena_mut(),
        shape: "RETURN value?",
        args: vec![value_opt],
        typ: "returnResult",
        span: Span::default(),
    }?;
    Ok((value_ctx, value_arch, value_call_result))
}

/// Calling recirculate_preserving_field_list during execution of the
/// egress control will cause the packet to be recirculated, i.e. it
/// will begin processing again with the parser, with the contents of
/// the packet as they are created by the deparser.  Recirculated
/// packets can be distinguished from new packets in ingress processing
/// by the value of the `standard_metadata` instance_type field.  The
/// caller may request that some user-defined metadata fields be
/// preserved with the recirculated packet.
///
/// The user metadata fields that are tagged with `@field_list(index)` will be
/// sent to the parser together with the packet.
///
/// Calling recirculate_preserving_field_list is only supported in the
/// egress control.  There is no way to undo its effects once it has
/// been called.  If recirculate_preserving_field_list is called
/// multiple times during a single execution of the egress control,
/// only one packet is recirculated, and only the user-defined metadata
/// fields specified by the field list index from the last such call
/// are preserved.  See the v1model architecture documentation (Note 1)
/// for more details.
///
/// `extern void recirculate_preserving_field_list(bit<8> index);`
pub fn recirculate_preserving_field_list<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    value_ctx: Value,
    value_arch: Value,
) -> Result<(Value, Value, Value), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    let value_idx = func::find_var_e_local(ctx, value_ctx, "index")?;
    let idx = usize::try_from(&unpack::p4_fixed_bit(ctx.arena(), &value_idx)?.1)?;
    // The scheduler acts on the request after the control returns
    let mut arch = pipe::find_arch_state(ctx, value_arch)?;
    arch.action.recirculate_opt = Some(idx);
    let value_arch = pipe::update_arch_state(ctx, value_arch, &arch)?;
    // Return without a value
    let typ = typ::make::opt(typ::make::var(
        crate::phrase!(node: "value".to_owned(), span: Span::default()),
        Vec::new(),
    ));
    let value_opt = value::make::opt(ctx.arena_mut(), typ.node.into(), None, Span::default())?;
    let value_call_result = value::make::case_shaped! {
        arena: ctx.arena_mut(),
        shape: "RETURN value?",
        args: vec![value_opt],
        typ: "returnResult",
        span: Span::default(),
    }?;
    Ok((value_ctx, value_arch, value_call_result))
}

/// Calling clone_preserving_field_list during execution of the ingress
/// or egress control will cause the packet to be cloned, sometimes
/// also called mirroring, i.e. zero or more copies of the packet are
/// made, and each will later begin egress processing as an independent
/// packet from the original packet.  The original packet continues
/// with its normal next steps independent of the clone(s).
///
/// The session parameter is an integer identifying a clone session id
/// (sometimes called a mirror session id).  The control plane software
/// must configure each session you wish to use, or else no clones will
/// be made using that session.  Typically this will involve the
/// control plane software specifying one output port to which the
/// cloned packet should be sent, or a list of (port, egress_rid) pairs
/// to which a separate clone should be created for each, similar to
/// multicast packets.
///
/// Cloned packets can be distinguished from others by the value of the
/// `standard_metadata` instance_type field.
///
/// The user metadata fields that are tagged with `@field_list(index)` will be
/// sent to the parser together with a clone of the packet.
///
/// If clone_preserving_field_list is called during ingress processing,
/// the first parameter must be `CloneType.I2E`.  If
/// clone_preserving_field_list is called during egress processing, the
/// first parameter must be `CloneType.E2E`.
///
/// There is no way to undo its effects once it has been called.  If
/// there are multiple calls to clone_preserving_field_list and/or
/// clone during a single execution of the same ingress (or egress)
/// control, only the last clone session and index are used.  See the
/// v1model architecture documentation (Note 1) for more details.
///
/// `extern void clone_preserving_field_list(in CloneType type,`
///                                         `in bit<32> session, bit<8> index);`
pub fn clone_preserving_field_list<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    value_ctx: Value,
    value_arch: Value,
) -> Result<(Value, Value, Value), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    // The scheduler acts on the request after the control returns
    let mut arch = pipe::find_arch_state(ctx, value_arch)?;
    let value_type = func::find_var_e_local(ctx, value_ctx, "type")?;
    let value_session = func::find_var_e_local(ctx, value_ctx, "session")?;
    let value_idx = func::find_var_e_local(ctx, value_ctx, "index")?;
    arch.action.clone_opt =
        Some(CloneInfo::new(ctx.arena(), &value_type, &value_session, &value_idx)?);
    let value_arch = pipe::update_arch_state(ctx, value_arch, &arch)?;
    // Return without a value
    let typ = typ::make::opt(typ::make::var(
        crate::phrase!(node: "value".to_owned(), span: Span::default()),
        Vec::new(),
    ));
    let value_opt = value::make::opt(ctx.arena_mut(), typ.node.into(), None, Span::default())?;
    let value_call_result = value::make::case_shaped! {
        arena: ctx.arena_mut(),
        shape: "RETURN value?",
        args: vec![value_opt],
        typ: "returnResult",
        span: Span::default(),
    }?;
    Ok((value_ctx, value_arch, value_call_result))
}

/// Log user defined messages
/// Example: log_msg("User defined message");
/// or log_msg("Value1 = {}, Value2 = {}",{value1, value2});
///
/// `extern void log_msg(string msg);`
/// `extern void log_msg<T>(string msg, in T data);`
pub fn log_msg<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    value_ctx: Value,
    value_arch: Value,
) -> Result<(Value, Value, Value), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    let value_msg = func::find_var_e_local(ctx, value_ctx, "msg")?;
    let msg = unpack::p4_string(ctx.arena(), &value_msg)?;
    ctx.external().write_log(&msg)?;
    // Return without a value
    let typ = typ::make::opt(typ::make::var(
        crate::phrase!(node: "value".to_owned(), span: Span::default()),
        Vec::new(),
    ));
    let value_opt = value::make::opt(ctx.arena_mut(), typ.node.into(), None, Span::default())?;
    let value_call_result = value::make::case_shaped! {
        arena: ctx.arena_mut(),
        shape: "RETURN value?",
        args: vec![value_opt],
        typ: "returnResult",
        span: Span::default(),
    }?;
    Ok((value_ctx, value_arch, value_call_result))
}

/// Expands `{}` holes in `fmt` with `args`; `{{` and `}}` are literal braces.
pub fn format_braces(arena: &Arena, fmt: &str, args: &[Value]) -> Result<String, ExternError> {
    let mut chars = fmt.chars().peekable();
    let mut args = args.iter();
    let mut text = String::with_capacity(fmt.len());
    while let Some(char) = chars.next() {
        match (char, chars.peek().copied()) {
            // Doubled braces escape themselves
            ('{', Some('{')) | ('}', Some('}')) => {
                chars.next();
                text.push(char);
            }
            // `{}` consumes the next argument
            ('{', Some('}')) => {
                chars.next();
                let value = args.next().ok_or_else(|| {
                    error::format_argument_arity_mismatch(
                        "not enough arguments for format string in log_msg".to_owned(),
                    )
                })?;
                text.push_str(&value.view(arena).to_string());
            }
            // Anything else is copied
            _ => text.push(char),
        }
    }
    // Every argument must be consumed
    if args.next().is_some() {
        return Err(error::format_argument_arity_mismatch(
            "too many arguments for format string in log_msg".to_owned(),
        )
        .into());
    }
    Ok(text)
}

/// Prints `msg` with each `{}` replaced by the next element of `data`.
pub fn log_msg_format<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, V1Model>,
    value_ctx: Value,
    value_arch: Value,
) -> Result<(Value, Value, Value), ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, V1Model>,
{
    let value_msg = func::find_var_e_local(ctx, value_ctx, "msg")?;
    let msg = unpack::p4_string(ctx.arena(), &value_msg)?;
    let value_data = func::find_var_e_local(ctx, value_ctx, "data")?;
    let values = unpack::p4_tuple(ctx.arena(), &value_data)?;
    let text = format_braces(ctx.arena(), &msg, &values)?;
    ctx.external().write_log(&text)?;
    // Return without a value
    let typ = typ::make::opt(typ::make::var(
        crate::phrase!(node: "value".to_owned(), span: Span::default()),
        Vec::new(),
    ));
    let value_opt = value::make::opt(ctx.arena_mut(), typ.node.into(), None, Span::default())?;
    let value_call_result = value::make::case_shaped! {
        arena: ctx.arena_mut(),
        shape: "RETURN value?",
        args: vec![value_opt],
        typ: "returnResult",
        span: Span::default(),
    }?;
    Ok((value_ctx, value_arch, value_call_result))
}
