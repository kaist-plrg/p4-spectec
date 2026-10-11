//! The PSA `Counter` extern, an indexed packet or byte counter
//!
//! Only the `PACKETS` counter type is supported by `count`.

use num_bigint::BigInt;
use num_traits::{One, Zero};
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
    spec::{args, func, unpack},
};

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
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
    /// every counter value has a data plane size specified by type `W`
    ///
    /// ```text
    /// extern Counter<W, S>
    /// Counter(bit<32> n_counters, PSA_CounterType_t type);
    /// ```
    pub fn init(
        arena: &Arena,
        _value_targs: Value,
        value_ids: Value,
        value_args: Value,
    ) -> Result<Self, ExternError> {
        let args = args::assoc(arena, value_ids, value_args)?;
        let value_size = args::find(&args, "n_counters")?;
        let value_type = args::find(&args, "type")?;
        let size = usize::try_from(&unpack::p4_fixed_bit(arena, &value_size)?.1)?;
        let (id_enum, id_type) = unpack::p4_enum(arena, &value_type)?;
        // The type argument selects what is counted
        match (id_enum.as_str(), id_type.as_str()) {
            ("PSA_CounterType_t", "PACKETS") => Ok(Self::Packets(vec![BigInt::zero(); size])),
            ("PSA_CounterType_t", "BYTES") => Ok(Self::Bytes(vec![BigInt::zero(); size])),
            ("PSA_CounterType_t", "PACKETS_AND_BYTES") => {
                Ok(Self::PacketsAndBytes(vec![(BigInt::zero(), BigInt::zero()); size]))
            }
            _ => Err(error::counter_type_invalid(format!(
                "invalid PSA_CounterType_t enum value: {id_enum}.{id_type}"
            ))
            .into()),
        }
    }

    /// `void count(in S index);`
    pub fn count<Interp, Iface, Ext>(
        mut self,
        ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
        value_ctx: Value,
        value_arch: Value,
    ) -> Result<(Self, Value, Value, Value), ExternError>
    where
        Iface: Interface,
        Ext: Extern,
        Interp: Interpreter<Iface, Ext>,
    {
        let value_idx = func::find_var_e_local(ctx, value_ctx, "index")?;
        let idx = usize::try_from(&unpack::p4_fixed_bit(ctx.arena(), &value_idx)?.1)?;
        // Only the `PACKETS` type is supported here
        let Self::Packets(counts) = &mut self else {
            return Err(error::counter_type_unsupported(
                "Only enum value PACKETS of PSA_CounterType_t is supported".to_owned(),
            )
            .into());
        };
        if let Some(count) = counts.get_mut(idx) {
            *count += BigInt::one();
        }
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
