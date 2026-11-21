//! The `counter` extern with indexed packet and byte counters
//!
//! An array of `size` counters, each counting packets, bytes, or both;
//! an out-of-range index leaves every counter unchanged.

use num_bigint::BigInt;
use num_traits::{One, Zero};

use p4spectec::runner::ExternError;

use crate::p4::{CallResult, ExternEnv, InitEnv, P4Vars};

use crate::arch::{core::object::PacketIn, v1model::types::CounterType};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
    pub fn init(env: &mut InitEnv) -> Result<Self, ExternError> {
        let size = env.get("size")?;
        let kind = env.get("type")?;
        let size: usize = size.try_into()?;
        // The type argument selects what is counted
        match CounterType::try_from(&kind)? {
            CounterType::Packets => Ok(Self::Packets(vec![BigInt::zero(); size])),
            CounterType::Bytes => Ok(Self::Bytes(vec![BigInt::zero(); size])),
            CounterType::PacketsAndBytes => {
                Ok(Self::PacketsAndBytes(vec![(BigInt::zero(), BigInt::zero()); size]))
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
            ("count", ["index"]) => {
                let idx: usize = env.get_local("index")?.try_into()?;
                self.count(idx, packet);
                Ok(CallResult::Return(None))
            }
            _ => env.unsupported(),
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
    pub fn count(&mut self, idx: usize, packet: &PacketIn) {
        // `PacketIn::len` counts bits
        let bytes = BigInt::from(packet.len / 8);
        // An out-of-range index leaves the array untouched
        match self {
            Self::Packets(counts) => {
                if let Some(count) = counts.get_mut(idx) {
                    *count += BigInt::one();
                }
            }
            Self::Bytes(counts) => {
                if let Some(count) = counts.get_mut(idx) {
                    *count += bytes;
                }
            }
            Self::PacketsAndBytes(counts) => {
                if let Some((count_packets, count_bytes)) = counts.get_mut(idx) {
                    *count_packets += BigInt::one();
                    *count_bytes += bytes;
                }
            }
        }
    }
}
