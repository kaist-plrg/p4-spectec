//! The PSA `Meter` extern, an indexed meter
//!
//! Metering is not modeled; `execute` always returns green.

use p4spectec::runner::ExternError;

use crate::p4::{CallResult, ExternEnv, InitEnv, P4Value};

use crate::arch::psa::types::{MeterColor, MeterType};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
/// Meter array by `PSA_MeterType_t`; the state is never updated.
pub enum Meter {
    /// Meters packets regardless of size.
    Packets(Vec<MeterColor>),
    /// Meters bytes.
    Bytes(Vec<MeterColor>),
}

impl Meter {
    /// Indexed meter with `n_meters` independent meter states.
    ///
    /// ```text
    /// extern Meter<S>
    /// Meter(bit<32> n_meters, PSA_MeterType_t type);
    /// ```
    pub fn init(env: &mut InitEnv) -> Result<Self, ExternError> {
        let n_meters = env.get("n_meters")?;
        let kind = env.get("type")?;
        let size: usize = n_meters.try_into()?;
        // The type argument selects what is metered
        match MeterType::try_from(&kind)? {
            MeterType::Packets => Ok(Self::Packets(vec![MeterColor::Green; size])),
            MeterType::Bytes => Ok(Self::Bytes(vec![MeterColor::Green; size])),
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
            ("execute", ["index", "color"]) => {
                Ok(CallResult::Return(Some(self.execute_color_aware())))
            }
            ("execute", ["index"]) => Ok(CallResult::Return(Some(self.execute_color_blind()))),
            _ => env.unsupported(),
        }
    }

    /// Perform a color aware meter update (see RFC 2698). The `color`
    /// parameter specifies the packet's color before the method call.
    ///
    /// ```text
    /// PSA_MeterColor_t execute(in S index, in PSA_MeterColor_t color);
    /// ```
    pub fn execute_color_aware(&self) -> P4Value {
        // Metering is not modeled: always GREEN
        MeterColor::Green.into()
    }

    /// Perform a color blind meter update (see RFC 2698). This may call
    /// `execute(index, MeterColor_t.GREEN)`, which has the same behavior.
    ///
    /// `PSA_MeterColor_t execute(in S index);`
    pub fn execute_color_blind(&self) -> P4Value {
        // Color-blind update defers to the color-aware path, returning GREEN
        self.execute_color_aware()
    }
}
