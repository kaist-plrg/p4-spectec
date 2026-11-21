//! The PSA `Hash` extern, a hash over a data tuple
//!
//! The constructor's `PSA_HashAlgorithm_t` member selects the `hash::Algorithm`.

use num_bigint::BigInt;
use num_traits::Zero;

use p4spectec::runner::ExternError;

use crate::p4::{CallResult, ExternEnv, InitEnv, P4Ops, P4Value, P4Vars};

use crate::{
    arch::{hash, psa::types::HashAlgorithm},
    error,
};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
/// A hash configured with one algorithm.
pub struct HashExtern {
    /// The algorithm.
    pub algo: hash::Algorithm,
}

impl HashExtern {
    /// ```text
    /// extern Hash<O>
    /// Hash(PSA_HashAlgorithm_t algo);
    /// ```
    pub fn init(env: &mut InitEnv) -> Result<Self, ExternError> {
        // An algorithm the simulator does not compute fails when hashing
        let algo = HashAlgorithm::try_from(&env.get("algo")?)?;
        Ok(Self { algo: algo.into() })
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
            ("get_hash", ["data"]) => {
                let data = match env.get_local("data")? {
                    P4Value::Tuple(data) => data,
                    data => return Err(data.mismatch("a tuple")),
                };
                let hash = self.get_hash(env, &data)?;
                Ok(CallResult::Return(Some(hash)))
            }
            ("get_hash", ["base", "data", "max"]) => {
                let int_base: BigInt = env.get_local("base")?.try_into()?;
                let int_max: BigInt = env.get_local("max")?.try_into()?;
                let data = match env.get_local("data")? {
                    P4Value::Tuple(data) => data,
                    data => return Err(data.mismatch("a tuple")),
                };
                let hash = self.get_hash_adjust(env, &int_base, &data, &int_max)?;
                Ok(CallResult::Return(Some(hash)))
            }
            _ => env.unsupported(),
        }
    }

    /// Compute and return the hash for `data`.
    ///
    /// `O get_hash<D>(in D data);`
    pub fn get_hash(&self, env: &mut ExternEnv, data: &[P4Value]) -> Result<P4Value, ExternError> {
        let int_hash = hash::hash_values(self.algo, None, data)?;
        // Cast the arbitrary-precision hash to the output type `O`
        let typ = env.get_type("O")?;
        env.cast(&typ, P4Value::InfInt(int_hash))
    }

    /// Compute the hash for `data`, reduce it modulo `max`, then add `base`.
    ///
    /// `base` specifies the minimum return value. `max` is the hash modulus;
    /// an implementation may limit its largest supported value, for example
    /// to 32 or 256, and may only support powers of two. P4 developers should
    /// limit their choice to such values to maximize portability.
    ///
    /// Returns `base + (h % max)`, where `h` is the hash value.
    ///
    /// `O get_hash<T, D>(in T base, in D data, in T max);`
    pub fn get_hash_adjust(
        &self,
        env: &mut ExternEnv,
        int_base: &BigInt,
        data: &[P4Value],
        int_max: &BigInt,
    ) -> Result<P4Value, ExternError> {
        let int_hash = hash::hash_values(self.algo, None, data)?;
        // The modulus must be positive
        if *int_max <= BigInt::zero() {
            return Err(
                error::hash_range_invalid("hash modulus must be positive".to_owned()).into()
            );
        }
        let int_hash = ((int_hash % int_max) + int_max) % int_max + int_base;
        // Cast the arbitrary-precision hash to the output type `O`
        let typ = env.get_type("O")?;
        env.cast(&typ, P4Value::InfInt(int_hash))
    }
}
