//! The `direct_counter` extern, a counter attached to a table
//!
//! The simulator keeps one count per object,
//! bumped only when `count()` is called.

use num_bigint::BigInt;
use num_traits::{One, Zero};

use p4spectec::runner::ExternError;

use crate::p4::{CallResult, ExternEnv, InitEnv};

use crate::arch::{core::object::PacketIn, v1model::types::CounterType};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
    pub fn init(env: &mut InitEnv) -> Result<Self, ExternError> {
        let kind = env.get("type")?;
        // The type argument selects what is counted
        match CounterType::try_from(&kind)? {
            CounterType::Packets => Ok(Self::Packets(BigInt::zero())),
            CounterType::Bytes => Ok(Self::Bytes(BigInt::zero())),
            CounterType::PacketsAndBytes => {
                Ok(Self::PacketsAndBytes((BigInt::zero(), BigInt::zero())))
            }
        }
    }

    // - Methods

    /// Runs the method `name`, whose parameters are `params`,
    /// on the current packet `packet`.
    pub fn call(
        &mut self,
        env: &mut ExternEnv,
        name: &str,
        params: &[&str],
        packet: &PacketIn,
    ) -> Result<CallResult, ExternError> {
        match (name, params) {
            ("count", []) => {
                self.count(packet);
                Ok(CallResult::Return(None))
            }
            _ => env.unsupported(),
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
    pub fn count(&mut self, packet: &PacketIn) {
        // `PacketIn::len` counts bits
        let bytes = BigInt::from(packet.len / 8);
        match self {
            Self::Packets(count) => *count += BigInt::one(),
            Self::Bytes(count) => *count += bytes,
            Self::PacketsAndBytes((count_packets, count_bytes)) => {
                *count_packets += BigInt::one();
                *count_bytes += bytes;
            }
        }
    }
}
