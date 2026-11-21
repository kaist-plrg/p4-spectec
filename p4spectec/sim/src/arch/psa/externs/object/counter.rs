//! The PSA `Counter` extern, an indexed packet or byte counter
//!
//! Only the `PACKETS` counter type is supported by `count`.

use num_bigint::BigInt;
use num_traits::{One, Zero};

use p4spectec::runner::ExternError;

use crate::error;

use crate::p4::{CallResult, ExternEnv, InitEnv, P4Vars};

use crate::arch::psa::types::CounterType;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
/// Counter array by `PSA_CounterType_t`.
pub enum Counter {
    /// Packet counts.
    Packets(Vec<BigInt>),
    /// Byte counts.
    Bytes(Vec<BigInt>),
    /// Packet and byte counts.
    PacketsAndBytes(Vec<(BigInt, BigInt)>),
}

impl Counter {
    /// Indirect counter with `n_counters` independent counter values, where
    /// every counter value has a data plane size specified by type `W`.
    ///
    /// ```text
    /// extern Counter<W, S>
    /// Counter(bit<32> n_counters, PSA_CounterType_t type);
    /// ```
    pub fn init(env: &mut InitEnv) -> Result<Self, ExternError> {
        let n_counters = env.get("n_counters")?;
        let kind = env.get("type")?;
        let size: usize = n_counters.try_into()?;
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

    /// Runs the method `name`, whose parameters are `params`.
    pub fn call(
        &mut self,
        env: &mut ExternEnv,
        name: &str,
        params: &[&str],
    ) -> Result<CallResult, ExternError> {
        match (name, params) {
            ("count", ["index"]) => {
                let idx: usize = env.get_local("index")?.try_into()?;
                self.count(idx)?;
                Ok(CallResult::Return(None))
            }
            _ => env.unsupported(),
        }
    }

    /// Counts a packet at `idx`; an out-of-range index counts nothing.
    ///
    /// `void count(in S index);`
    pub fn count(&mut self, idx: usize) -> Result<(), ExternError> {
        // Only the `PACKETS` type is supported here
        let Self::Packets(counts) = self else {
            return Err(error::counter_type_unsupported(
                "Only enum value PACKETS of PSA_CounterType_t is supported".to_owned(),
            )
            .into());
        };
        if let Some(count) = counts.get_mut(idx) {
            *count += BigInt::one();
        }
        Ok(())
    }
}
