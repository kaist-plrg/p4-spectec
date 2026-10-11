//! The PSA `Meter` extern, an indexed meter
//!
//! Metering is not modeled; `execute` always returns green.

use serde::{Deserialize, Serialize};

use crate::lang::{
    common::source::Span,
    data::{
        arena::Arena,
        typ,
        value::flat::{self as value, Value},
    },
};

use crate::runner::{Extern, ExternError, Interface, Interpreter, RunnerContext};

use crate::sim_plugin::{
    error,
    spec::{args, pack, unpack},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
/// A meter color (RFC 2698).
pub enum Color {
    /// Over the peak rate.
    Red,
    /// Within the committed rate.
    Green,
    /// Between the committed and peak rates.
    Yellow,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
/// Meter array by `PSA_MeterType_t`; the state is never updated.
pub enum Meter {
    /// Meters packets regardless of size.
    Packets(Vec<Color>),
    /// Meters bytes.
    Bytes(Vec<Color>),
}

impl Meter {
    /// Indexed meter with `n_meters` independent meter states
    ///
    /// ```text
    /// extern Meter<S>
    /// Meter(bit<32> n_meters, PSA_MeterType_t type);
    /// ```
    pub fn init(
        arena: &Arena,
        _value_targs: Value,
        value_ids: Value,
        value_args: Value,
    ) -> Result<Self, ExternError> {
        let args = args::assoc(arena, value_ids, value_args)?;
        let value_size = args::find(&args, "n_meters")?;
        let value_type = args::find(&args, "type")?;
        let size = usize::try_from(&unpack::p4_fixed_bit(arena, &value_size)?.1)?;
        let (id_enum, id_type) = unpack::p4_enum(arena, &value_type)?;
        // The type argument selects what is metered
        match (id_enum.as_str(), id_type.as_str()) {
            ("PSA_MeterType_t", "PACKETS") => Ok(Self::Packets(vec![Color::Green; size])),
            ("PSA_MeterType_t", "BYTES") => Ok(Self::Bytes(vec![Color::Green; size])),
            _ => Err(error::meter_type_invalid(format!(
                "invalid PSA_MeterType_t enum value: {id_enum}.{id_type}"
            ))
            .into()),
        }
    }

    /// Perform a color aware meter update (see RFC 2698). The `color`
    /// parameter specifies the packet's color before the method call
    ///
    /// ```text
    /// PSA_MeterColor_t execute(in S index, in PSA_MeterColor_t color);
    /// ```
    pub fn execute_color_aware<Interp, Iface, Ext>(
        self,
        ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
        value_ctx: Value,
        value_arch: Value,
    ) -> Result<(Self, Value, Value, Value), ExternError>
    where
        Iface: Interface,
        Ext: Extern,
        Interp: Interpreter<Iface, Ext>,
    {
        // Metering is not modeled: always GREEN
        let value_color = pack::p4_enum(ctx.arena_mut(), "PSA_MeterColor_t", "GREEN")?;
        let typ = typ::make::opt(typ::make::var(
            crate::phrase!(node: "value".to_owned(), span: Span::default()),
            Vec::new(),
        ));
        let value_opt =
            value::make::opt(ctx.arena_mut(), typ.node.into(), Some(value_color), Span::default())?;
        let value_call_result = value::make::case_shaped! {
            arena: ctx.arena_mut(),
            shape: "RETURN value?",
            args: vec![value_opt],
            typ: "returnResult",
            span: Span::default(),
        }?;
        Ok((self, value_ctx, value_arch, value_call_result))
    }

    /// Perform a color blind meter update (see RFC 2698). This may call
    /// `execute(index, MeterColor_t.GREEN)`, which has the same behavior
    ///
    /// `PSA_MeterColor_t execute(in S index);`
    pub fn execute_color_blind<Interp, Iface, Ext>(
        self,
        ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
        value_ctx: Value,
        value_arch: Value,
    ) -> Result<(Self, Value, Value, Value), ExternError>
    where
        Iface: Interface,
        Ext: Extern,
        Interp: Interpreter<Iface, Ext>,
    {
        // Color-blind update defers to the color-aware path, returning GREEN
        self.execute_color_aware(ctx, value_ctx, value_arch)
    }
}
