//! The `packet_in` extern object
//!
//! The packet is a bit vector with a cursor;
//! extraction reads bits at the cursor and advances it.
//! `call` dispatches a method by name;
//! each returns a parser result: a return with an optional value,
//! or a rejection with `PacketTooShort`, `HeaderTooShort`,
//! or `ParserInvalidArgument`.

use num_bigint::BigInt;

use p4spectec::runner::ExternError;

use crate::error;

use crate::p4::{CallResult, ExternEnv, P4Ops, P4Value, P4Vars};

use super::bits::{bits_to_int_unsigned, string_to_bits};

/// Input packet data and its extraction cursor.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
    pub fn new(text: &str) -> Result<Self, ExternError> {
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
        let packet_in = Self { idx: self.idx + size, ..self.clone() };
        Ok((packet_in, bits))
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

    // - Methods

    /// Runs the method `name`, whose parameters are `params`.
    pub fn call(
        &mut self,
        env: &mut ExternEnv,
        name: &str,
        params: &[&str],
    ) -> Result<CallResult, ExternError> {
        match (name, params) {
            ("extract", ["hdr"]) => self.extract(env),
            ("extract", ["variableSizeHeader", "variableFieldSizeInBits"]) => {
                let width_varsize: usize = env.get_local("variableFieldSizeInBits")?.try_into()?;
                self.extract_varsize(env, width_varsize)
            }
            ("lookahead", []) => self.lookahead(env),
            ("advance", ["sizeInBits"]) => {
                let size: usize = env.get_local("sizeInBits")?.try_into()?;
                self.advance(size)
            }
            ("length", []) => Ok(CallResult::Return(Some(self.length()))),
            _ => env.unsupported(),
        }
    }

    /// Reads a fixed-size header into `hdr` and advances the packet cursor.
    ///
    /// `T` must be a fixed-size header type.
    /// Extraction may trigger `PacketTooShort` or `StackOutOfBounds`:
    /// ```text
    /// void extract<T>(out T hdr);
    /// ```
    pub fn extract(&mut self, env: &mut ExternEnv) -> Result<CallResult, ExternError> {
        // The header size comes from the instantiated type argument
        let typ = env.get_type("T")?;
        let size = env.max_size_in_bits(&typ)?;
        // Too few bits: reject with `PacketTooShort`
        if !self.has_size(size)? {
            return Ok(CallResult::Reject("PacketTooShort".to_owned()));
        }
        // Take the header's bits and advance the cursor past them
        let (packet_in, bits) = self.parse(size)?;
        *self = packet_in;
        // The `out` argument's current value gives the header's layout
        let hdr = env.get_local("hdr")?;
        // Fill the header from the bits and write it to the `out` argument
        let hdr = env.from_bits(hdr, 0, &bits)?;
        env.set_local("hdr", hdr)?;
        Ok(CallResult::Return(None))
    }

    /// Extracts a header whose variable-size field takes `width_varsize` bits.
    ///
    /// ```text
    /// void extract<T>(out T variableSizeHeader,
    ///                 in bit<32> variableFieldSizeInBits);
    /// ```
    pub fn extract_varsize(
        &mut self,
        env: &mut ExternEnv,
        width_varsize: usize,
    ) -> Result<CallResult, ExternError> {
        // Fixed part and maximum of the header type
        let typ = env.get_type("T")?;
        let size_min = env.min_size_in_bits(&typ)?;
        let size_max = env.max_size_in_bits(&typ)?;
        // Total size is the fixed part plus the variable part
        let size = size_min
            .checked_add(width_varsize)
            .ok_or_else(|| error::packet_size_out_of_bounds("packet size overflow".to_owned()))?;
        // Not a whole number of bytes: reject with `ParserInvalidArgument`
        if !width_varsize.is_multiple_of(8) {
            return Ok(CallResult::Reject("ParserInvalidArgument".to_owned()));
        }
        // Too few bits: reject with `PacketTooShort`
        if !self.has_size(size)? {
            return Ok(CallResult::Reject("PacketTooShort".to_owned()));
        }
        // Larger than the type allows: reject with `HeaderTooShort`
        if size > size_max {
            return Ok(CallResult::Reject("HeaderTooShort".to_owned()));
        }
        // Take the header's bits and advance the cursor past them
        let (packet_in, bits) = self.parse(size)?;
        *self = packet_in;
        // The `out` argument's current value gives the header's layout
        let hdr_varsize = env.get_local("variableSizeHeader")?;
        // Fill the header, sizing its variable field, and write it back
        let hdr_varsize = env.from_bits(hdr_varsize, width_varsize, &bits)?;
        env.set_local("variableSizeHeader", hdr_varsize)?;
        Ok(CallResult::Return(None))
    }

    /// Reads a value without advancing the packet cursor.
    ///
    /// ```text
    /// T lookahead<T>();
    /// ```
    pub fn lookahead(&self, env: &mut ExternEnv) -> Result<CallResult, ExternError> {
        // The header size comes from the instantiated type argument
        let typ = env.get_type("T")?;
        let size = env.max_size_in_bits(&typ)?;
        // With no `out` argument, the default value of `T` gives the layout
        let hdr = env.default_of(&typ)?;
        // Too few bits: reject with `PacketTooShort`
        if !self.has_size(size)? {
            return Ok(CallResult::Reject("PacketTooShort".to_owned()));
        }
        // Read without moving the cursor; the value is returned, not stored
        let bits = &self.bits[self.idx..self.idx + size];
        let hdr = env.from_bits(hdr, 0, bits)?;
        Ok(CallResult::Return(Some(hdr)))
    }

    /// Advances the packet cursor by `size` bits.
    ///
    /// ```text
    /// void advance(in bit<32> sizeInBits);
    /// ```
    pub fn advance(&mut self, size: usize) -> Result<CallResult, ExternError> {
        // Too few bits: reject with `PacketTooShort`
        if !self.has_size(size)? {
            return Ok(CallResult::Reject("PacketTooShort".to_owned()));
        }
        self.idx += size;
        Ok(CallResult::Return(None))
    }

    /// The total packet length in whole bytes, rounding a partial byte up.
    ///
    /// ```text
    /// bit<32> length();
    /// ```
    pub fn length(&self) -> P4Value {
        P4Value::Bit { width: 32, int: self.len.div_ceil(8).into() }
    }
}
