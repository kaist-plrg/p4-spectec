//! The `counter` extern with indexed packet and byte counters
//!
//! An array of `size` counters, each counting packets, bytes, or both;
//! an out-of-range index leaves every counter unchanged.

use num_bigint::BigInt;
use num_traits::{One, Zero};
use serde::{Deserialize, Serialize};

use crate::lang::{
    common::source::Span,
    data::{
        arena::Arena,
        typ,
        value::flat::{self as value, Value},
    },
};

use crate::runner::{Extern, ExternError, Interface, Interpreter, RunnerContext};

use crate::sim_plugin::{
    core::object::PacketIn,
    error,
    spec::{args, func, unpack},
};

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
/// Counter array by `CounterType`.
pub enum Counter {
    /// Packet counts.
    Packets(Vec<BigInt>),
    /// Byte counts.
    Bytes(Vec<BigInt>),
    /// Packet and byte counts.
    PacketsAndBytes(Vec<(BigInt, BigInt)>),
}

impl Counter {
    /// A counter object is created by calling its constructor.  This
    /// creates an array of counter states, with the number of counter
    /// states specified by the size parameter.  The array indices are
    /// in the range [0, size-1].
    ///
    /// You must provide a choice of whether to maintain only a packet
    /// count (`CounterType.packets`), only a byte count
    /// (`CounterType.bytes`), or both (`CounterType.packets_and_bytes`).
    ///
    /// Counters can be updated from your P4 program, but can only be
    /// read from the control plane.  If you need something that can be
    /// both read and written from the P4 program, consider using a
    /// register.
    ///
    /// `counter(bit<32> size, CounterType type);`
    pub fn init(
        arena: &Arena,
        _value_targs: Value,
        value_ids: Value,
        value_args: Value,
    ) -> Result<Self, ExternError> {
        let args = args::assoc(arena, value_ids, value_args)?;
        let value_size = args::find(&args, "size")?;
        let value_type = args::find(&args, "type")?;
        let size = usize::try_from(&unpack::p4_fixed_bit(arena, &value_size)?.1)?;
        let (id_enum, id_type) = unpack::p4_enum(arena, &value_type)?;
        // The type argument selects what is counted
        match (id_enum.as_str(), id_type.as_str()) {
            ("CounterType", "packets") => Ok(Self::Packets(vec![BigInt::zero(); size])),
            ("CounterType", "bytes") => Ok(Self::Bytes(vec![BigInt::zero(); size])),
            ("CounterType", "packets_and_bytes") => {
                Ok(Self::PacketsAndBytes(vec![(BigInt::zero(), BigInt::zero()); size]))
            }
            _ => Err(error::counter_type_invalid(format!(
                "invalid CounterType enum value: {id_enum}.{id_type}"
            ))
            .into()),
        }
    }

    /// `count()` causes the counter state with the specified index to be
    ///  read, modified, and written back, atomically relative to the
    ///  processing of other packets, updating the packet count, byte
    ///  count, or both, depending upon the CounterType of the counter
    ///  instance used when it was constructed.
    ///
    ///  @param index The index of the counter state in the array to be
    ///               updated, normally a value in the range [0,
    ///               size-1].  If index >= size, no counter state will be
    ///               updated.
    ///
    /// `void count(in bit<32> index);`
    pub fn count<Interp, Iface, Ext>(
        mut self,
        ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
        value_ctx: Value,
        value_arch: Value,
        packet_in: &PacketIn,
    ) -> Result<(Self, Value, Value, Value), ExternError>
    where
        Iface: Interface,
        Ext: Extern,
        Interp: Interpreter<Iface, Ext>,
    {
        let value_idx = func::find_var_e_local(ctx, value_ctx, "index")?;
        let idx = usize::try_from(&unpack::p4_fixed_bit(ctx.arena(), &value_idx)?.1)?;
        // An out-of-range index leaves the array untouched
        match &mut self {
            Self::Packets(counts) => {
                if let Some(count) = counts.get_mut(idx) {
                    *count += BigInt::one();
                }
            }
            Self::Bytes(counts) => {
                if let Some(count) = counts.get_mut(idx) {
                    *count += BigInt::from(packet_in.len);
                }
            }
            Self::PacketsAndBytes(counts) => {
                if let Some((count_packets, count_bytes)) = counts.get_mut(idx) {
                    *count_packets += BigInt::one();
                    *count_bytes += BigInt::from(packet_in.len);
                }
            }
        }
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
        Ok((self, value_ctx, value_arch, value_call_result))
    }
}
