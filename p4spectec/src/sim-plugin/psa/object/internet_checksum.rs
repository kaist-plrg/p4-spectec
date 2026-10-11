//! The PSA `InternetChecksum` extern, a ones-complement 16-bit checksum
//!
//! State is the running ones-complement sum;
//! `add` and `subtract` fold data in and out, `get` returns the complement.

use num_bigint::BigInt;
use num_traits::Zero;
use serde::{Deserialize, Serialize};

use crate::util::bigint;

use crate::lang::{
    common::source::Span,
    data::{
        typ,
        value::flat::{self as value, Value},
    },
};

use crate::runner::{Extern, ExternError, Interface, Interpreter, RunnerContext};

use crate::sim_plugin::{
    hash,
    spec::{func, pack, unpack},
};

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
/// The running ones-complement sum.
pub struct InternetChecksum {
    /// The accumulated 16-bit sum.
    pub int: BigInt,
}

impl InternetChecksum {
    /// Checksum based on the `ONES_COMPLEMENT16` algorithm used in IPv4,
    /// TCP and UDP. Supports incremental updates through `subtract`
    /// (see IETF RFC 1624)
    ///
    /// ```text
    /// extern InternetChecksum
    /// InternetChecksum();
    /// ```
    pub fn init() -> Self {
        Self { int: BigInt::zero() }
    }

    /// Reset internal state and prepare the unit for computation
    ///
    /// Every InternetChecksum instance is automatically initialized as if
    /// `clear()` had been called whenever the parser or control containing
    /// its instantiation executes. All maintained state is independent per
    /// packet
    ///
    /// `void clear();`
    pub fn clear<Interp, Iface, Ext>(
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
        Ok((Self::init(), value_ctx, value_arch, value_call_result))
    }

    /// Add data to the checksum; `data` must be a multiple of 16 bits long
    ///
    /// `void add<T>(in T data);`
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
        self.update(ctx, value_ctx, value_arch, "csum16")
    }

    /// Subtract data from the existing checksum; `data` must be a multiple
    /// of 16 bits long
    ///
    /// `void subtract<T>(in T data);`
    pub fn subtract<Interp, Iface, Ext>(
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
        self.update(ctx, value_ctx, value_arch, "csum16_sub")
    }

    /// Folds `data` into the sum with `algo`, then stores its complement.
    fn update<Interp, Iface, Ext>(
        mut self,
        ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
        value_ctx: Value,
        value_arch: Value,
        algo: &str,
    ) -> Result<(Self, Value, Value, Value), ExternError>
    where
        Iface: Interface,
        Ext: Extern,
        Interp: Interpreter<Iface, Ext>,
    {
        let value_data = func::find_var_e_local(ctx, value_ctx, "data")?;
        let values = unpack::p4_tuple(ctx.arena(), &value_data)?;
        let int = hash::compute_checksum(ctx.arena(), algo, Some(&self.int), &values)?;
        self.int = bigint::bitwise_neg(&int, &16.into())?;
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

    /// Get the checksum for data added and not removed since the last clear
    ///
    /// `bit<16> get();`
    pub fn get<Interp, Iface, Ext>(
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
        self.int = bigint::bitwise_neg(&self.int, &16.into())?;
        self.get_state(ctx, value_ctx, value_arch)
    }

    /// Get the current checksum computation state. The return value is only
    /// intended for a future call to `set_state`
    ///
    /// `bit<16> get_state();`
    pub fn get_state<Interp, Iface, Ext>(
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
        let value_checksum = pack::p4_fixed_bit(ctx.arena_mut(), 16.into(), self.int.clone())?;
        let typ = typ::make::opt(typ::make::var(
            crate::phrase!(node: "value".to_owned(), span: Span::default()),
            Vec::new(),
        ));
        let value_opt = value::make::opt(
            ctx.arena_mut(),
            typ.node.into(),
            Some(value_checksum),
            Span::default(),
        )?;
        let value_call_result = value::make::case_shaped! {
            arena: ctx.arena_mut(),
            shape: "RETURN value?",
            args: vec![value_opt],
            typ: "returnResult",
            span: Span::default(),
        }?;
        Ok((self, value_ctx, value_arch, value_call_result))
    }

    /// Restore state returned by an earlier `get_state` call on this
    /// InternetChecksum instance or a different one
    ///
    /// `void set_state(in bit<16> checksum_state);`
    pub fn set_state<Interp, Iface, Ext>(
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
        let value_state = func::find_var_e_local(ctx, value_ctx, "checksum_state")?;
        self.int = unpack::p4_fixed_bit(ctx.arena(), &value_state)?.1;
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
