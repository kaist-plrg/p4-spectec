//! The `direct_meter` extern, a meter attached to a table
//!
//! Metering is not modeled; `read()` always reports the color green.

use num_bigint::BigInt;
use num_traits::Zero;
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
    core::object::PacketIn,
    error,
    spec::{args, func, pack, rel, unpack},
};

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
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
    pub fn init(
        arena: &Arena,
        _value_targs: Value,
        value_ids: Value,
        value_args: Value,
    ) -> Result<Self, ExternError> {
        let args = args::assoc(arena, value_ids, value_args)?;
        let value_type = args::find(&args, "type")?;
        let (id_enum, id_type) = unpack::p4_enum(arena, &value_type)?;
        // The type argument selects what is metered
        match (id_enum.as_str(), id_type.as_str()) {
            ("MeterType", "packets") => Ok(Self::Packets(BigInt::zero())),
            ("MeterType", "bytes") => Ok(Self::Bytes(BigInt::zero())),
            _ => Err(error::meter_type_invalid(format!(
                "invalid MeterType enum value: {id_enum}.{id_type}"
            ))
            .into()),
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
    pub fn read<Interp, Iface, Ext>(
        self,
        ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
        value_ctx: Value,
        value_arch: Value,
        _packet_in: &PacketIn,
    ) -> Result<(Self, Value, Value, Value), ExternError>
    where
        Iface: Interface,
        Ext: Extern,
        Interp: Interpreter<Iface, Ext>,
    {
        // The color width comes from the instantiated `T`
        let value_typ = func::find_type_e_local(ctx, value_ctx, "T")?;
        let value_typ = func::subst_type_e_local(ctx, value_ctx, value_typ)?;
        let size = func::sizeof_max_size_in_bits(ctx, value_typ)?;
        // Metering is not modeled: always GREEN
        let value = pack::p4_fixed_bit(ctx.arena_mut(), size, BigInt::zero())?;
        let value_ctx = rel::lvalue_write_var_local(ctx, value_ctx, value_arch, "result", value)?;
        // Return without a value
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
        Ok((self, value_ctx, value_arch, value_call_result))
    }
}
