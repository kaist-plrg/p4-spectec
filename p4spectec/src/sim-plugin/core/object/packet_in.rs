//! The `packet_in` extern object
//!
//! The packet is a bit vector with a cursor;
//! extraction reads bits at the cursor and advances it.
//! Every method returns the new object, the updated context and architecture,
//! and a parser result: `RETURN` with an optional value,
//! or `REJECT` with `PacketTooShort`, `HeaderTooShort`,
//! or `ParserInvalidArgument`.

use num_bigint::BigInt;
use num_traits::ToPrimitive;
use serde::{Deserialize, Serialize};

use crate::lang::{
    common::{prim::num, source::Span},
    data::{
        typ,
        value::flat::{self as value, Value},
    },
};

use crate::runner::{Extern, ExternError, Interface, Interpreter, RunnerContext};

use crate::sim_plugin::{
    error,
    spec::{func, pack, rel, unpack},
};

use super::bits::{bits_to_int_unsigned, string_to_bits};

/// Input packet data and its extraction cursor.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct PacketIn {
    /// The whole packet.
    pub bits: Vec<bool>,
    /// Cursor: bits before it have been extracted.
    pub idx: usize,
    /// Length in bits; the payload is `bits[idx..len]`.
    pub len: usize,
}

impl PacketIn {
    /// A packet from hex text with the cursor at the start.
    pub fn init(text: &str) -> Result<Self, ExternError> {
        let bits = string_to_bits(text)?;
        let len = bits.len();
        Ok(Self { bits, idx: 0, len })
    }

    /// Moves the cursor back to the start.
    pub fn reset(&mut self) {
        self.idx = 0;
    }

    /// Guards against a cursor or length past the packet.
    fn check_bounds(&self) -> Result<(), ExternError> {
        if self.idx > self.len || self.len > self.bits.len() {
            return Err(
                error::packet_cursor_invalid("invalid packet cursor or length".to_owned()).into()
            );
        }
        Ok(())
    }

    /// Whether `size` bits remain after the cursor.
    fn has_size(&self, size: usize) -> Result<bool, ExternError> {
        self.check_bounds()?;
        Ok(size <= self.len - self.idx)
    }

    /// Takes `size` bits at the cursor; returns the moved packet and the bits.
    pub fn parse(&self, size: usize) -> Result<(Self, Vec<bool>), ExternError> {
        if !self.has_size(size)? {
            return Err(error::packet_size_out_of_bounds(
                "packet parse exceeds available bits".to_owned(),
            )
            .into());
        }
        let bits = self.bits[self.idx..self.idx + size].to_vec();
        let pkt = Self { idx: self.idx + size, ..self.clone() };
        Ok((pkt, bits))
    }

    /// The bits not yet extracted.
    pub fn payload(&self) -> Result<&[bool], ExternError> {
        self.check_bounds()?;
        Ok(&self.bits[self.idx..self.len])
    }

    /// The payload as whole bytes; a trailing partial byte is dropped.
    pub fn payload_bytes(&self) -> Result<Vec<BigInt>, ExternError> {
        Ok(self
            .payload()?
            .chunks_exact(8)
            .map(bits_to_int_unsigned)
            .collect())
    }

    /// Reads a fixed-size header into `hdr` and advances the packet cursor
    ///
    /// `T` must be a fixed-size header type.
    /// Extraction may trigger `PacketTooShort` or `StackOutOfBounds`:
    /// ```text
    /// void extract<T>(out T hdr);
    /// ```
    pub fn extract<Interp, Iface, Ext>(
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
        // The header size comes from the instantiated type argument
        let value_typ = func::find_type_e_local(ctx, value_ctx, "T")?;
        let value_typ_subst = func::subst_type_e_local(ctx, value_ctx, value_typ)?;
        let size = (func::sizeof_max_size_in_bits(ctx, value_typ_subst)?)
            .to_usize()
            .ok_or_else(|| error::packet_size_invalid("invalid packet size".to_owned()))?;
        // Too few bits: reject with `PacketTooShort`
        if !self.has_size(size)? {
            let value_name =
                value::make::text(ctx.arena_mut(), "PacketTooShort".to_owned(), Span::default())?;
            let value_err = value::make::case_shaped! {
                arena: ctx.arena_mut(),
                shape: "ERROR '.' nameIR",
                args: vec![value_name],
                typ: "errorValue",
                span: Span::default(),
            }?;
            let value_call_result = value::make::case_shaped! {
                arena: ctx.arena_mut(),
                shape: "REJECT errorValue",
                args: vec![value_err],
                typ: "rejectTransitionResult",
                span: Span::default(),
            }?;
            return Ok((self.clone(), value_ctx, value_arch, value_call_result));
        }
        // Fill the header from the bits and write it to the `out` argument
        let (pkt, bits) = self.parse(size)?;
        let value_hdr = func::find_var_e_local(ctx, value_ctx, "hdr")?;
        let value_hdr = func::write_value_from_bits(ctx, value_hdr, 0, &bits)?;
        let value_ctx = rel::lvalue_write_var_local(ctx, value_ctx, value_arch, "hdr", value_hdr)?;
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

    /// Extracts a header with a variable-size field
    ///
    /// ```text
    /// void extract<T>(out T variableSizeHeader,
    ///                 in bit<32> variableFieldSizeInBits);
    /// ```
    pub fn extract_varsize<Interp, Iface, Ext>(
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
        let value_typ = func::find_type_e_local(ctx, value_ctx, "T")?;
        let value_typ_subst = func::subst_type_e_local(ctx, value_ctx, value_typ)?;
        // Fixed part and maximum of the header type
        let size_min = (func::sizeof_min_size_in_bits(ctx, value_typ_subst)?)
            .to_usize()
            .ok_or_else(|| error::packet_size_invalid("invalid packet size".to_owned()))?;
        let size_max = (func::sizeof_max_size_in_bits(ctx, value_typ_subst)?)
            .to_usize()
            .ok_or_else(|| error::packet_size_invalid("invalid packet size".to_owned()))?;
        // The variable size must be byte-aligned: test its low three bits
        let value_size = func::find_var_e_local(ctx, value_ctx, "variableFieldSizeInBits")?;
        let value_hi = pack::p4_arbitrary_int(ctx.arena_mut(), 2.into())?;
        let value_lo = pack::p4_arbitrary_int(ctx.arena_mut(), 0.into())?;
        let value_alignment = func::bitacc_range_op(ctx, value_size, value_hi, value_lo)?;
        let alignment = (unpack::p4_fixed_bit(ctx.arena(), &value_alignment)?.1)
            .to_usize()
            .ok_or_else(|| error::packet_size_invalid("invalid packet size".to_owned()))?;
        // The variable size as a number
        let values_size = value::get::case(ctx.arena(), &value_size)?.args();
        let value_varsize = values_size.get(1).ok_or_else(|| {
            ExternError::from(crate::lang::data::value::ValueError::IndexOutOfBounds {
                index: 1,
                len: values_size.len(),
            })
        })?;
        let size_varsize = (num::to_int(value::get::num(ctx.arena(), value_varsize)?))
            .to_usize()
            .ok_or_else(|| error::packet_size_invalid("invalid packet size".to_owned()))?;
        // Total size is the fixed part plus the variable part
        let size = size_min
            .checked_add(size_varsize)
            .ok_or_else(|| error::packet_size_out_of_bounds("packet size overflow".to_owned()))?;
        // Misaligned: reject with `ParserInvalidArgument`
        if alignment != 0 {
            let value_name = value::make::text(
                ctx.arena_mut(),
                "ParserInvalidArgument".to_owned(),
                Span::default(),
            )?;
            let value_err = value::make::case_shaped! {
                arena: ctx.arena_mut(),
                shape: "ERROR '.' nameIR",
                args: vec![value_name],
                typ: "errorValue",
                span: Span::default(),
            }?;
            let value_call_result = value::make::case_shaped! {
                arena: ctx.arena_mut(),
                shape: "REJECT errorValue",
                args: vec![value_err],
                typ: "rejectTransitionResult",
                span: Span::default(),
            }?;
            return Ok((self.clone(), value_ctx, value_arch, value_call_result));
        }
        // Too few bits: reject with `PacketTooShort`
        if !self.has_size(size)? {
            let value_name =
                value::make::text(ctx.arena_mut(), "PacketTooShort".to_owned(), Span::default())?;
            let value_err = value::make::case_shaped! {
                arena: ctx.arena_mut(),
                shape: "ERROR '.' nameIR",
                args: vec![value_name],
                typ: "errorValue",
                span: Span::default(),
            }?;
            let value_call_result = value::make::case_shaped! {
                arena: ctx.arena_mut(),
                shape: "REJECT errorValue",
                args: vec![value_err],
                typ: "rejectTransitionResult",
                span: Span::default(),
            }?;
            return Ok((self.clone(), value_ctx, value_arch, value_call_result));
        }
        // Larger than the type allows: reject with `HeaderTooShort`
        if size > size_max {
            let value_name =
                value::make::text(ctx.arena_mut(), "HeaderTooShort".to_owned(), Span::default())?;
            let value_err = value::make::case_shaped! {
                arena: ctx.arena_mut(),
                shape: "ERROR '.' nameIR",
                args: vec![value_name],
                typ: "errorValue",
                span: Span::default(),
            }?;
            let value_call_result = value::make::case_shaped! {
                arena: ctx.arena_mut(),
                shape: "REJECT errorValue",
                args: vec![value_err],
                typ: "rejectTransitionResult",
                span: Span::default(),
            }?;
            return Ok((self.clone(), value_ctx, value_arch, value_call_result));
        }
        // Fill the header, sizing its variable field, and write it back
        let (pkt, bits) = self.parse(size)?;
        let value_hdr = func::find_var_e_local(ctx, value_ctx, "variableSizeHeader")?;
        let value_hdr = func::write_value_from_bits(ctx, value_hdr, size_varsize, &bits)?;
        let value_ctx = rel::lvalue_write_var_local(
            ctx,
            value_ctx,
            value_arch,
            "variableSizeHeader",
            value_hdr,
        )?;
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

    /// Reads a value without advancing the packet cursor
    ///
    /// ```text
    /// T lookahead<T>();
    /// ```
    pub fn lookahead<Interp, Iface, Ext>(
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
        // The header size comes from the instantiated type argument
        let value_typ = func::find_type_e_local(ctx, value_ctx, "T")?;
        let value_typ_subst = func::subst_type_e_local(ctx, value_ctx, value_typ)?;
        let size = (func::sizeof_max_size_in_bits(ctx, value_typ_subst)?)
            .to_usize()
            .ok_or_else(|| error::packet_size_invalid("invalid packet size".to_owned()))?;
        // Too few bits: reject with `PacketTooShort`
        let value_hdr = func::default(ctx, value_typ)?;
        if !self.has_size(size)? {
            let value_name =
                value::make::text(ctx.arena_mut(), "PacketTooShort".to_owned(), Span::default())?;
            let value_err = value::make::case_shaped! {
                arena: ctx.arena_mut(),
                shape: "ERROR '.' nameIR",
                args: vec![value_name],
                typ: "errorValue",
                span: Span::default(),
            }?;
            let value_call_result = value::make::case_shaped! {
                arena: ctx.arena_mut(),
                shape: "REJECT errorValue",
                args: vec![value_err],
                typ: "rejectTransitionResult",
                span: Span::default(),
            }?;
            return Ok((self.clone(), value_ctx, value_arch, value_call_result));
        }
        // Read without moving the cursor; the value is returned, not stored
        let bits = &self.bits[self.idx..self.idx + size];
        let value_hdr = func::write_value_from_bits(ctx, value_hdr, 0, bits)?;
        let typ = typ::make::opt(typ::make::var(
            crate::phrase!(node: "value".to_owned(), span: Span::default()),
            Vec::new(),
        ));
        let value_opt =
            value::make::opt(ctx.arena_mut(), typ.node.into(), Some(value_hdr), Span::default())?;
        let value_call_result = value::make::case_shaped! {
            arena: ctx.arena_mut(),
            shape: "RETURN value?",
            args: vec![value_opt],
            typ: "returnResult",
            span: Span::default(),
        }?;
        Ok((self.clone(), value_ctx, value_arch, value_call_result))
    }

    /// Advances the packet cursor by the requested number of bits
    ///
    /// ```text
    /// void advance(in bit<32> sizeInBits);
    /// ```
    pub fn advance<Interp, Iface, Ext>(
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
        // The advance is a runtime `bit<32>`
        let value_size = func::find_var_e_local(ctx, value_ctx, "sizeInBits")?;
        let size = (unpack::p4_fixed_bit(ctx.arena(), &value_size)?.1)
            .to_usize()
            .ok_or_else(|| error::packet_size_invalid("invalid packet size".to_owned()))?;
        // Too few bits: reject with `PacketTooShort`
        if !self.has_size(size)? {
            let value_name =
                value::make::text(ctx.arena_mut(), "PacketTooShort".to_owned(), Span::default())?;
            let value_err = value::make::case_shaped! {
                arena: ctx.arena_mut(),
                shape: "ERROR '.' nameIR",
                args: vec![value_name],
                typ: "errorValue",
                span: Span::default(),
            }?;
            let value_call_result = value::make::case_shaped! {
                arena: ctx.arena_mut(),
                shape: "REJECT errorValue",
                args: vec![value_err],
                typ: "rejectTransitionResult",
                span: Span::default(),
            }?;
            return Ok((self.clone(), value_ctx, value_arch, value_call_result));
        }
        let pkt = Self { idx: self.idx + size, ..self.clone() };
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

    /// Returns the total packet length in bytes
    ///
    /// ```text
    /// bit<32> length();
    /// ```
    pub fn length<Interp, Iface, Ext>(
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
        // Length in whole bytes, rounding a partial byte up
        // Return the length as a `bit<32>` value
        let value_len =
            pack::p4_fixed_bit(ctx.arena_mut(), 32.into(), self.len.div_ceil(8).into())?;
        let typ = typ::make::opt(typ::make::var(
            crate::phrase!(node: "value".to_owned(), span: Span::default()),
            Vec::new(),
        ));
        let value_opt =
            value::make::opt(ctx.arena_mut(), typ.node.into(), Some(value_len), Span::default())?;
        let value_call_result = value::make::case_shaped! {
            arena: ctx.arena_mut(),
            shape: "RETURN value?",
            args: vec![value_opt],
            typ: "returnResult",
            span: Span::default(),
        }?;
        Ok((self.clone(), value_ctx, value_arch, value_call_result))
    }
}
