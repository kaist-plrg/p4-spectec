//! The `packet_out` extern object
//!
//! Emitting a header appends its bits;
//! the buffer is prepended to the payload when the packet leaves.

use serde::{Deserialize, Serialize};

use crate::lang::{
    common::source::Span,
    data::{
        typ,
        value::flat::{self as value, Value},
    },
};

use crate::runner::{Extern, ExternError, Interface, Interpreter, RunnerContext};

use crate::sim_plugin::spec::func;

/// Output packet data accumulated by emission.
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct PacketOut {
    /// Emitted bits so far, most significant first.
    pub bits: Vec<bool>,
}

impl PacketOut {
    /// Appends the header's bits to the output packet
    ///
    /// ```text
    /// void emit<T>(in T hdr);
    /// ```
    pub fn emit<Interp, Iface, Ext>(
        &self,
        ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
        value_ctx: Value,
        value_arch: Value,
    ) -> Result<(Self, Value, Value, Value), ExternError>
    where
        Iface: Interface,
        Ext: Extern,
        Interp: Interpreter<Iface, Ext>,
    {
        // Only a valid header serializes to bits
        let value_hdr = func::find_var_e_local(ctx, value_ctx, "hdr")?;
        let value_bits = func::write_bits_from_value(ctx, value_hdr)?;
        let bits = value::get::list(ctx.arena(), &value_bits)?
            .iter()
            .map(|value| value::get::bool(ctx.arena(), value))
            .collect::<Result<Vec<_>, _>>()?;
        // Append to a copy; objects are immutable values
        let pkt = Self { bits: self.bits.iter().copied().chain(bits).collect() };
        // `emit` returns nothing: a `RETURN` with no value
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
        Ok((pkt, value_ctx, value_arch, value_call_result))
    }
}
