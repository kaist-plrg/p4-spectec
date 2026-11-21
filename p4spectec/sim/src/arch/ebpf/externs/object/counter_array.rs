//! The eBPF `CounterArray` extern
//!
//! A dense array of 32-bit counters the data plane increments.

use num_bigint::BigInt;

use p4spectec::runner::ExternError;

use crate::error;

use crate::p4::{CallResult, ExternEnv, InitEnv, P4Vars};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
/// A counter array as a vector of 32-bit values.
pub struct CounterArray {
    /// The counter values, indexed from zero.
    pub counts: Vec<u32>,
}

impl CounterArray {
    /// A counter array is a dense or sparse array of unsigned 32-bit values,
    /// visible to the control-plane as an EBPF map (array or hash).
    /// Each counter is addressed by a 32-bit index.
    /// Counters can only be incremented by the data-plane, but they can be read
    /// or reset by the control-plane.
    ///
    /// Allocate an array of counters.
    /// - `max_index`: Maximum counter index supported.
    /// - `sparse`: The counter array is supposed to be sparse.
    ///
    /// ```p4
    /// CounterArray(bit<32> max_index, bool sparse);
    /// ```
    pub fn init(env: &mut InitEnv) -> Result<Self, ExternError> {
        let max = env.get("max_index")?;
        let sparse = env.get("sparse")?;
        let len: usize = max.try_into()?;
        // A sparse array is kept dense, but `sparse` must still be a bool
        let _: bool = sparse.try_into()?;
        let mut counts = Vec::new();
        counts.try_reserve_exact(len)?;
        counts.resize(len, 0);
        Ok(Self { counts })
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
            ("increment", ["index"]) => {
                let idx: usize = env.get_local("index")?.try_into()?;
                self.increment(idx);
                Ok(CallResult::Return(None))
            }
            ("add", ["index", "value"]) => {
                let idx: usize = env.get_local("index")?.try_into()?;
                let int_add: BigInt = env.get_local("value")?.try_into()?;
                let int = u32::try_from(&int_add).map_err(|_| {
                    error::counter_value_out_of_bounds("counter value exceeds 32 bits".to_owned())
                })?;
                self.add(idx, int);
                Ok(CallResult::Return(None))
            }
            _ => env.unsupported(),
        }
    }

    /// Increment counter with specified index.
    ///
    /// ```p4
    /// void increment(in bit<32> index);
    /// ```
    pub fn increment(&mut self, idx: usize) {
        self.add(idx, 1);
    }

    /// Add value to counter with specified index.
    ///
    /// Adds with wrapping; an out-of-range index updates no counter:
    /// ```p4
    /// void add(in bit<32> index, in bit<32> value);
    /// ```
    pub fn add(&mut self, idx: usize, int: u32) {
        if let Some(count) = self.counts.get_mut(idx) {
            *count = count.wrapping_add(int);
        }
    }
}
