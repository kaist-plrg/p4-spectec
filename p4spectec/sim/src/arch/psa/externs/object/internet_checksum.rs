//! The PSA `InternetChecksum` extern, a ones-complement 16-bit checksum
//!
//! State is the running ones-complement sum;
//! `add` and `subtract` fold data in and out, `get` returns the complement.

use num_bigint::BigInt;
use num_traits::Zero;

use p4spectec::util::bigint;

use p4spectec::runner::ExternError;

use crate::arch::hash;

use crate::p4::{CallResult, ExternEnv, P4Value, P4Vars};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
/// The running ones-complement sum.
pub struct InternetChecksum {
    /// The accumulated 16-bit sum.
    pub int: BigInt,
}

impl InternetChecksum {
    /// Checksum based on the `ONES_COMPLEMENT16` algorithm used in IPv4,
    /// TCP and UDP. Supports incremental updates through `subtract`
    /// (see IETF RFC 1624).
    ///
    /// ```text
    /// extern InternetChecksum
    /// InternetChecksum();
    /// ```
    pub fn init() -> Self {
        Self { int: BigInt::zero() }
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
            ("clear", []) => {
                self.clear();
                Ok(CallResult::Return(None))
            }
            ("add", ["data"]) => {
                let data = match env.get_local("data")? {
                    P4Value::Tuple(data) => data,
                    data => return Err(data.mismatch("a tuple")),
                };
                self.add(&data)?;
                Ok(CallResult::Return(None))
            }
            ("subtract", ["data"]) => {
                let data = match env.get_local("data")? {
                    P4Value::Tuple(data) => data,
                    data => return Err(data.mismatch("a tuple")),
                };
                self.subtract(&data)?;
                Ok(CallResult::Return(None))
            }
            ("get", []) => Ok(CallResult::Return(Some(self.get()?))),
            ("get_state", []) => Ok(CallResult::Return(Some(self.get_state()))),
            ("set_state", ["checksum_state"]) => {
                let int_state: BigInt = env.get_local("checksum_state")?.try_into()?;
                self.set_state(int_state);
                Ok(CallResult::Return(None))
            }
            _ => env.unsupported(),
        }
    }

    /// Reset internal state and prepare the unit for computation.
    ///
    /// Every InternetChecksum instance is automatically initialized as if
    /// `clear()` had been called whenever the parser or control containing
    /// its instantiation executes. All maintained state is independent per
    /// packet
    ///
    /// `void clear();`
    pub fn clear(&mut self) {
        *self = Self::init();
    }

    /// Add data to the checksum; `data` must be a multiple of 16 bits long.
    ///
    /// `void add<T>(in T data);`
    pub fn add(&mut self, data: &[P4Value]) -> Result<(), ExternError> {
        self.update(data, hash::Algorithm::Csum16)
    }

    /// Subtract data from the existing checksum; `data` must be a multiple
    /// of 16 bits long.
    ///
    /// `void subtract<T>(in T data);`
    pub fn subtract(&mut self, data: &[P4Value]) -> Result<(), ExternError> {
        self.update(data, hash::Algorithm::Csum16Sub)
    }

    /// Folds `data` into the sum with `algo`, then stores its complement.
    fn update(&mut self, data: &[P4Value], algo: hash::Algorithm) -> Result<(), ExternError> {
        let int = hash::hash_values(algo, Some(&self.int), data)?;
        self.int = bigint::bitwise_neg(&int, &16.into())?;
        Ok(())
    }

    /// Get the checksum for data added and not removed since the last clear.
    ///
    /// `bit<16> get();`
    pub fn get(&mut self) -> Result<P4Value, ExternError> {
        self.int = bigint::bitwise_neg(&self.int, &16.into())?;
        Ok(self.get_state())
    }

    /// Get the current checksum computation state. The return value is only
    /// intended for a future call to `set_state`.
    ///
    /// `bit<16> get_state();`
    pub fn get_state(&self) -> P4Value {
        P4Value::Bit { width: 16, int: self.int.clone() }
    }

    /// Restore state returned by an earlier `get_state` call on this
    /// InternetChecksum instance or a different one.
    ///
    /// `void set_state(in bit<16> checksum_state);`
    pub fn set_state(&mut self, int_state: BigInt) {
        self.int = int_state;
    }
}
