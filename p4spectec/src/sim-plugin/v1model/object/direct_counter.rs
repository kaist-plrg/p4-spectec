//! The `direct_counter` extern, a counter attached to a table
//!
//! The simulator keeps one count per object,
//! bumped only when `count()` is called.

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
    spec::{args, unpack},
};

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
/// Direct counter by `CounterType`.
pub enum DirectCounter {
    /// Packet count.
    Packets(BigInt),
    /// Byte count.
    Bytes(BigInt),
    /// Packet and byte counts.
    PacketsAndBytes((BigInt, BigInt)),
}

impl DirectCounter {
    /// A direct_counter object is created by calling its constructor.
    /// You must provide a choice of whether to maintain only a packet
    /// count (`CounterType.packets`), only a byte count
    /// (`CounterType.bytes`), or both (`CounterType.packets_and_bytes`).
    /// After constructing the object, you can associate it with at
    /// most one table, by adding the following table property to the
    /// definition of that table:
    ///
    /// ```text
    ///     counters = <object_name>;
    ///
    /// ```
    /// Counters can be updated from your P4 program, but can only be
    /// read from the control plane.  If you need something that can be
    /// both read and written from the P4 program, consider using a
    /// register.
    ///
    /// `direct_counter(CounterType type);`
    pub fn init(
        arena: &Arena,
        _value_targs: Value,
        value_ids: Value,
        value_args: Value,
    ) -> Result<Self, ExternError> {
        let args = args::assoc(arena, value_ids, value_args)?;
        let value_type = args::find(&args, "type")?;
        let (id_enum, id_type) = unpack::p4_enum(arena, &value_type)?;
        // The type argument selects what is counted
        match (id_enum.as_str(), id_type.as_str()) {
            ("CounterType", "packets") => Ok(Self::Packets(BigInt::zero())),
            ("CounterType", "bytes") => Ok(Self::Bytes(BigInt::zero())),
            ("CounterType", "packets_and_bytes") => {
                Ok(Self::PacketsAndBytes((BigInt::zero(), BigInt::zero())))
            }
            _ => Err(error::counter_type_invalid(format!(
                "invalid CounterType enum value: {id_enum}.{id_type}"
            ))
            .into()),
        }
    }

    /// The `count()` method is actually unnecessary in the v1model
    /// architecture.  This is because after a direct_counter object
    /// has been associated with a table as described in the
    /// documentation for the direct_counter constructor, every time
    /// the table is applied and a table entry is matched, the counter
    /// state associated with the matching entry is read, modified, and
    /// written back, atomically relative to the processing of other
    /// packets, regardless of whether the `count()` method is called in
    /// the body of that action.
    ///
    /// `void count();`
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
        match &mut self {
            Self::Packets(count) => *count += BigInt::one(),
            Self::Bytes(count) => *count += BigInt::from(packet_in.len),
            Self::PacketsAndBytes((count_packets, count_bytes)) => {
                *count_packets += BigInt::one();
                *count_bytes += BigInt::from(packet_in.len);
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
