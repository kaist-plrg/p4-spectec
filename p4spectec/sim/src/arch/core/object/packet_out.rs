//! The `packet_out` extern object
//!
//! Emitting a header appends its bits;
//! the buffer is prepended to the payload when the packet leaves.

use p4spectec::runner::ExternError;

use crate::p4::{CallResult, ExternEnv, P4Ops, P4Value, P4Vars};

/// Output packet data accumulated by emission.
#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PacketOut {
    /// Emitted bits so far, most significant first.
    pub bits: Vec<bool>,
}

impl PacketOut {
    /// Runs the method `name`, whose parameters are `params`.
    pub fn call(
        &mut self,
        env: &mut ExternEnv,
        name: &str,
        params: &[&str],
    ) -> Result<CallResult, ExternError> {
        match (name, params) {
            ("emit", ["hdr"]) => {
                let hdr = env.get_local("hdr")?;
                self.emit(env, &hdr)?;
                Ok(CallResult::Return(None))
            }
            _ => env.unsupported(),
        }
    }

    /// Appends the header's bits to the output packet.
    ///
    /// Only a valid header serializes to bits:
    /// ```text
    /// void emit<T>(in T hdr);
    /// ```
    pub fn emit(&mut self, env: &mut ExternEnv, hdr: &P4Value) -> Result<(), ExternError> {
        let bits = env.to_bits(hdr)?;
        self.bits.extend(bits);
        Ok(())
    }
}
