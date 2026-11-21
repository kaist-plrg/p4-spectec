//! v1model extern functions
//!
//! Each reads its arguments from the `ExternEnv`;
//! clone, resubmit, and recirculate record their request
//! in the architecture state the dispatcher passes in.
//! Doc comments summarize the extern's description in `v1model.p4`.
//! clone, truncate, assert, and assume are not implemented;
//! the dispatcher rejects them as unsupported calls.

use std::hash::{BuildHasher, Hasher, RandomState};
use std::io::Write;

use num_bigint::BigInt;
use num_traits::Zero;

use p4spectec::util::bigint::remainder;

use p4spectec::runner::ExternError;

use crate::p4::{CallResult, ExternEnv, P4Ops, P4Value, P4Vars};

use crate::{arch::core::object::PacketIn, arch::hash, error};

use super::super::{
    DEFAULT_DROP_PORT, PORT_WIDTH,
    target::{packet::CloneInfo, state::ArchState},
    types::HashAlgorithm,
};

// == Packet decisions

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
pub fn digest(_env: &mut ExternEnv) -> Result<CallResult, ExternError> {
    // No-op in the source simulator
    Ok(CallResult::Return(None))
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
pub fn mark_to_drop(env: &mut ExternEnv) -> Result<CallResult, ExternError> {
    // Send the packet to the drop port
    let port_drop = P4Value::Bit { width: PORT_WIDTH, int: DEFAULT_DROP_PORT.into() };
    env.set_local("standard_metadata.egress_spec", port_drop)?;
    // No multicast for a dropped packet
    env.set_local("standard_metadata.mcast_grp", P4Value::Bit { width: 16, int: 0.into() })?;
    Ok(CallResult::Return(None))
}

// == Random numbers and hashes

/// Generate a random number in the range lo..hi, inclusive, and write
/// it to the result parameter.  The value written to result is not
/// specified if lo > hi.
///
/// This model draws uniformly from the range and writes `lo` if lo > hi.
/// A draw is an effect, so no cached call result replays it.
///
/// @param T          Must be a type `bit<W>`
///
/// `extern void random<T>(out T result, in T lo, in T hi);`
pub fn random(env: &mut ExternEnv) -> Result<CallResult, ExternError> {
    /// Draws a uniform integer in `[0, bound)` for a positive `bound`.
    ///
    /// Each `RandomState` carries fresh SipHash keys, so hashing nothing yields 64 random bits.
    fn draw_below(bound: &BigInt) -> BigInt {
        let bits = bound.bits();
        let int_mask = (BigInt::from(1) << bits) - 1;
        loop {
            // Fill the bound's width in 64-bit chunks, then drop the excess bits
            let mut int = BigInt::zero();
            for _ in 0..bits.div_ceil(64) {
                int = (int << 64) | BigInt::from(RandomState::new().build_hasher().finish());
            }
            int &= &int_mask;
            // Reject a draw at or above the bound to keep the draw uniform
            if &int < bound {
                return int;
            }
        }
    }
    // `T` is `bit<W>`, so the result takes the width of `lo`
    let (width, int_lo) = match env.get_local("lo")? {
        P4Value::Bit { width, int } => (width, int),
        lo => return Err(lo.mismatch("a bit<W>")),
    };
    let int_hi: BigInt = env.get_local("hi")?.try_into()?;
    // Draw from the inclusive range, or keep the lower bound if it is empty
    let int = if int_lo <= int_hi {
        let int_bound = &int_hi - &int_lo + 1;
        int_lo + draw_below(&int_bound)
    } else {
        int_lo
    };
    // Mark the draw as an effect so that enclosing calls are not cached
    env.mark_impure();
    env.set_local("result", P4Value::Bit { width, int })?;
    Ok(CallResult::Return(None))
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
pub fn hash(env: &mut ExternEnv) -> Result<CallResult, ExternError> {
    let int_base: BigInt = env.get_local("base")?.try_into()?;
    let int_max: BigInt = env.get_local("max")?.try_into()?;
    let int = hash_data(env, None)?;
    // The source simulator uses max - base as the range divisor
    let int = adjust(&int_base, &int_max, &int)?;
    // Cast the arbitrary-precision result to the output type `O`
    let typ = env.get_type("O")?;
    let result = env.cast(&typ, P4Value::InfInt(int))?;
    env.set_local("result", result)?;
    Ok(CallResult::Return(None))
}

/// Folds `int` into `[int_base, int_max)`, or returns `int_base` when `int_max` is zero.
fn adjust(int_base: &BigInt, int_max: &BigInt, int: &BigInt) -> Result<BigInt, ExternError> {
    // max = 0: the result is always base
    if int_max.is_zero() {
        return Ok(int_base.clone());
    }
    let int_range = int_max - int_base;
    // The divisor max - base must be positive
    if int_range <= BigInt::zero() {
        return Err(
            error::hash_range_invalid("hash range divisor must be positive".to_owned()).into()
        );
    }
    Ok(remainder(int, &int_range) + int_base)
}

/// Hashes the `data` tuple, plus the unparsed payload if given, with `algo`.
fn hash_data(env: &mut ExternEnv, payload: Option<&PacketIn>) -> Result<BigInt, ExternError> {
    let mut data = match env.get_local("data")? {
        P4Value::Tuple(data) => data,
        data => return Err(data.mismatch("a tuple")),
    };
    // The payload is appended as 8-bit values
    if let Some(packet) = payload {
        for byte in packet.payload_bytes()? {
            data.push(P4Value::Bit { width: 8, int: byte });
        }
    }
    let algo = HashAlgorithm::try_from(&env.get_local("algo")?)?;
    hash::hash_values(algo.into(), None, &data)
}

// == Checksums

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
///
/// `payload` is the packet for the `_with_payload` variant.
pub fn verify_checksum(
    env: &mut ExternEnv,
    payload: Option<&PacketIn>,
) -> Result<CallResult, ExternError> {
    // A false condition skips the checksum
    let condition: bool = env.get_local("condition")?.try_into()?;
    if !condition {
        return Ok(CallResult::Return(None));
    }
    let int_expect: BigInt = env.get_local("checksum")?.try_into()?;
    let int_actual = hash_data(env, payload)?;
    // A mismatch sets `checksum_error` to 1
    if int_expect != int_actual {
        env.set_global(
            "standard_metadata.checksum_error",
            P4Value::Bit { width: 1, int: 1.into() },
        )?;
    }
    Ok(CallResult::Return(None))
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
///
/// `payload` is the packet for the `_with_payload` variant.
pub fn update_checksum(
    env: &mut ExternEnv,
    payload: Option<&PacketIn>,
) -> Result<CallResult, ExternError> {
    // A false condition skips the checksum
    let condition: bool = env.get_local("condition")?.try_into()?;
    if !condition {
        return Ok(CallResult::Return(None));
    }
    let int = hash_data(env, payload)?;
    // Cast the arbitrary-precision result to the output type `O`
    let typ = env.get_type("O")?;
    let checksum = env.cast(&typ, P4Value::InfInt(int))?;
    env.set_local("checksum", checksum)?;
    Ok(CallResult::Return(None))
}

// == Clone, resubmit, and recirculate

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
pub fn resubmit_preserving_field_list(
    env: &mut ExternEnv,
    arch: &mut ArchState,
) -> Result<CallResult, ExternError> {
    let idx: usize = env.get_local("index")?.try_into()?;
    // The scheduler acts on the request after the control returns
    arch.action.resubmit_opt = Some(idx);
    Ok(CallResult::Return(None))
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
pub fn recirculate_preserving_field_list(
    env: &mut ExternEnv,
    arch: &mut ArchState,
) -> Result<CallResult, ExternError> {
    let idx: usize = env.get_local("index")?.try_into()?;
    // The scheduler acts on the request after the control returns
    arch.action.recirculate_opt = Some(idx);
    Ok(CallResult::Return(None))
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
pub fn clone_preserving_field_list(
    env: &mut ExternEnv,
    arch: &mut ArchState,
) -> Result<CallResult, ExternError> {
    let kind = env.get_local("type")?;
    let session = env.get_local("session")?;
    let idx = env.get_local("index")?;
    // The scheduler acts on the request after the control returns
    arch.action.clone_opt = Some(CloneInfo::new(&kind, session, idx)?);
    Ok(CallResult::Return(None))
}

// == Log messages

/// Log user defined messages.
/// Example: log_msg("User defined message");
/// or log_msg("Value1 = {}, Value2 = {}",{value1, value2});
///
/// `extern void log_msg(string msg);`
/// `extern void log_msg<T>(string msg, in T data);`
pub fn log_msg(env: &mut ExternEnv) -> Result<CallResult, ExternError> {
    let msg: String = env.get_local("msg")?.try_into()?;
    env.mark_impure();
    writeln!(std::io::stdout(), "{msg}")
        .map_err(|error| error::program_output_failed(error.to_string()))?;
    Ok(CallResult::Return(None))
}

/// Expands `{}` holes in `fmt` with `values`; `{{` and `}}` are literal braces.
fn format_braces(
    env: &mut ExternEnv,
    fmt: &str,
    values: &[P4Value],
) -> Result<String, ExternError> {
    let mut chars = fmt.chars().peekable();
    let mut values = values.iter();
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
                let value = values.next().ok_or_else(|| {
                    error::format_argument_arity_mismatch(
                        "not enough arguments for format string in log_msg".to_owned(),
                    )
                })?;
                text.push_str(&env.format(value)?);
            }
            // Anything else is copied
            _ => text.push(char),
        }
    }
    // Every argument must be consumed
    if values.next().is_some() {
        return Err(error::format_argument_arity_mismatch(
            "too many arguments for format string in log_msg".to_owned(),
        )
        .into());
    }
    Ok(text)
}

/// Prints `msg` with each `{}` replaced by the next element of `data`.
pub fn log_msg_format(env: &mut ExternEnv) -> Result<CallResult, ExternError> {
    let msg: String = env.get_local("msg")?.try_into()?;
    let data = match env.get_local("data")? {
        P4Value::Tuple(data) => data,
        data => return Err(data.mismatch("a tuple")),
    };
    let text = format_braces(env, &msg, &data)?;
    env.mark_impure();
    writeln!(std::io::stdout(), "{text}")
        .map_err(|error| error::program_output_failed(error.to_string()))?;
    Ok(CallResult::Return(None))
}
