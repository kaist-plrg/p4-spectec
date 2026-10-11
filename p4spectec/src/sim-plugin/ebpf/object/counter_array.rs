//! The eBPF `CounterArray` extern
//!
//! A dense array of 32-bit counters the data plane increments.

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
    pub fn init(arena: &Arena, value_ids: Value, value_args: Value) -> Result<Self, ExternError> {
        let args = args::assoc(arena, value_ids, value_args)?;
        let value_max = args::find(&args, "max_index")?;
        let value_sparse = args::find(&args, "sparse")?;
        let len = usize::try_from(&unpack::p4_fixed_bit(arena, &value_max)?.1)?;
        unpack::p4_bool(arena, &value_sparse)?;
        let mut counts = Vec::new();
        counts.try_reserve_exact(len)?;
        counts.resize(len, 0);
        Ok(Self { counts })
    }

    /// Increment counter with specified index
    ///
    /// ```p4
    /// void increment(in bit<32> index);
    /// ```
    pub fn increment<Interp, Iface, Ext>(
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
        // Get "index"
        let value_idx = func::find_var_e_local(ctx, value_ctx, "index")?;
        let idx = usize::try_from(&unpack::p4_fixed_bit(ctx.arena(), &value_idx)?.1)?;
        self.update(ctx, value_ctx, value_arch, idx, 1)
    }

    /// Add value to counter with specified index
    ///
    /// ```p4
    /// void add(in bit<32> index, in bit<32> value);
    /// ```
    pub fn add<Interp, Iface, Ext>(
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
        // Get "index"
        let value_idx = func::find_var_e_local(ctx, value_ctx, "index")?;
        let idx = usize::try_from(&unpack::p4_fixed_bit(ctx.arena(), &value_idx)?.1)?;
        // Get "value"
        let value_add = func::find_var_e_local(ctx, value_ctx, "value")?;
        let (_, int_add) = unpack::p4_fixed_bit(ctx.arena(), &value_add)?;
        let int = u32::try_from(&int_add).map_err(|_| {
            error::counter_value_out_of_bounds("counter value exceeds 32 bits".to_owned())
        })?;
        self.update(ctx, value_ctx, value_arch, idx, int)
    }

    /// Adds `int` to the counter at `idx`, wrapping; out of range does nothing.
    fn update<Interp, Iface, Ext>(
        mut self,
        ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
        value_ctx: Value,
        value_arch: Value,
        idx: usize,
        int: u32,
    ) -> Result<(Self, Value, Value, Value), ExternError>
    where
        Iface: Interface,
        Ext: Extern,
        Interp: Interpreter<Iface, Ext>,
    {
        // Update counter
        // Out of range: no counter is updated
        if let Some(count) = self.counts.get_mut(idx) {
            *count = count.wrapping_add(int);
        }
        // Create call result
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
