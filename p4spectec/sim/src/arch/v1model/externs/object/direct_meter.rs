//! The `direct_meter` extern, a meter attached to a table
//!
//! Metering is not modeled; `read()` always reports the color green.

use num_bigint::BigInt;
use num_traits::Zero;

use p4spectec::runner::ExternError;

use crate::p4::{CallResult, ExternEnv, InitEnv, P4Ops, P4Value, P4Vars};

use crate::arch::v1model::types::MeterType;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
/// Direct meter by `MeterType`; the state is never updated.
pub enum DirectMeter {
    /// Meters packets regardless of size.
    Packets(BigInt),
    /// Meters bytes.
    Bytes(BigInt),
}

impl DirectMeter {
    /// A direct_meter object is created by calling its constructor.
    /// You must provide a choice of whether to meter based on the
    /// number of packets, regardless of their size
    /// (`MeterType.packets`), or based upon the number of bytes the
    /// packets contain (`MeterType.bytes`).  After constructing the
    /// object, you can associate it with at most one table, by adding
    /// the following table property to the definition of that table:
    ///
    /// ```text
    ///     meters = <object_name>;
    ///
    /// ```
    /// `direct_meter(MeterType type);`
    pub fn init(env: &mut InitEnv) -> Result<Self, ExternError> {
        let kind = env.get("type")?;
        // The type argument selects what is metered
        match MeterType::try_from(&kind)? {
            MeterType::Packets => Ok(Self::Packets(BigInt::zero())),
            MeterType::Bytes => Ok(Self::Bytes(BigInt::zero())),
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
            ("read", ["result"]) => {
                self.read(env)?;
                Ok(CallResult::Return(None))
            }
            _ => env.unsupported(),
        }
    }

    /// After a direct_meter object has been associated with a table as
    /// described in the documentation for the direct_meter
    /// constructor, every time the table is applied and a table entry
    /// is matched, the meter state associated with the matching entry
    /// is read, modified, and written back, atomically relative to the
    /// processing of other packets, regardless of whether the `read()`
    /// method is called in the body of that action.
    ///
    /// `read()` may only be called within an action executed as a result
    /// of matching a table entry, of a table that has a direct_meter
    /// associated with it.  Calling `read()` causes an integer encoding
    /// of one of the colors green, yellow, or red to be written to the
    /// result out parameter.
    ///
    /// @param result Type T must be `bit<W>` with W >= 2.  The value of
    ///              result will be assigned 0 for color GREEN, 1 for
    ///              color YELLOW, and 2 for color RED (see RFC 2697
    ///              and RFC 2698 for the meaning of these colors).
    ///
    /// `void read(out T result);`
    pub fn read(&self, env: &mut ExternEnv) -> Result<(), ExternError> {
        // The color width comes from the instantiated `T`
        let typ = env.get_type("T")?;
        let size = env.max_size_in_bits(&typ)?;
        // Metering is not modeled: always GREEN
        env.set_local("result", P4Value::Bit { width: size, int: BigInt::zero() })
    }
}
