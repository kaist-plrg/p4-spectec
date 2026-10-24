//! A decoder of binary KORE, as the LLVM backend serializes terms
//!
//! The format is llvm-backend `docs/binary-kore.md` (versions 1.0.0 to 1.2.0):
//! a header, then a postfix sequence of sorts, symbols, and patterns.
//! For example, `\dv{SortInt{}}("5")` is the string pattern `5`,
//! the sort `SortInt{}`, the symbol `\dv` with one sort, and arity 1.
//! Strings are byte strings, kept with one character per byte (`parse`).

use super::{
    ast::{Pattern, Sort},
    parse::text_of_bytes,
};

/// The first bytes of binary KORE.
const MAGIC: &[u8] = b"\x7fKORE";

/// An item on the decoder's stack.
enum Item {
    Sort(Sort),
    Symbol(String, Vec<Sort>),
    Pattern(Pattern),
}

/// A reader of binary KORE at a position.
struct Decoder<'a> {
    src: &'a [u8],
    pos: usize,
    /// Version 1.0.0 has fixed-length fields.
    fixed: bool,
}

/// A decoding result; an error says what is malformed.
type Result<T> = std::result::Result<T, String>;

impl Decoder<'_> {
    /// The next byte.
    fn byte(&mut self) -> Result<u8> {
        let byte = *self
            .src
            .get(self.pos)
            .ok_or("unexpected end of binary KORE")?;
        self.pos += 1;
        Ok(byte)
    }

    /// A little-endian number of a fixed number of bytes.
    fn fixed(&mut self, bytes: usize) -> Result<usize> {
        let mut value = 0;
        for shift in 0..bytes {
            value |= usize::from(self.byte()?) << (8 * shift);
        }
        Ok(value)
    }

    /// A variable-length field: 7 bits per byte, least significant first.
    fn length(&mut self, bytes_fixed: usize) -> Result<usize> {
        if self.fixed {
            return self.fixed(bytes_fixed);
        }
        let mut value = 0usize;
        for shift in (0..63).step_by(7) {
            let byte = self.byte()?;
            value |= usize::from(byte & 0x7f) << shift;
            if byte & 0x80 == 0 {
                return Ok(value);
            }
        }
        Err("malformed length in binary KORE".to_owned())
    }

    /// A direct string at the current position, as its bytes.
    fn direct(&mut self) -> Result<Vec<u8>> {
        let len = self.length(4)?;
        let bytes = self
            .src
            .get(self.pos..self.pos + len)
            .ok_or("string beyond the end of binary KORE")?;
        self.pos += len;
        Ok(bytes.to_vec())
    }

    /// A direct or interned string, as its bytes.
    fn string(&mut self) -> Result<Vec<u8>> {
        match self.byte()? {
            0x01 => self.direct(),
            0x02 => {
                let back = self.length(4)?;
                // Counted back from the byte after the reference, to the length field
                let target = self
                    .pos
                    .checked_sub(back)
                    .ok_or("bad string reference in binary KORE")?;
                let pos = std::mem::replace(&mut self.pos, target);
                let bytes = self.direct();
                self.pos = pos;
                bytes
            }
            byte => Err(format!("expected a string in binary KORE, got {byte:#04x}")),
        }
    }

    /// A string as a name or text, one character per byte.
    fn name(&mut self) -> Result<String> {
        Ok(text_of_bytes(&self.string()?))
    }
}

/// The last `n` items of the stack, which must be sorts.
fn pop_sorts(stack: &mut Vec<Item>, n: usize) -> Result<Vec<Sort>> {
    let start = stack
        .len()
        .checked_sub(n)
        .ok_or("too few sorts in binary KORE")?;
    stack
        .drain(start..)
        .map(|item| match item {
            Item::Sort(sort) => Ok(sort),
            _ => Err("expected a sort in binary KORE".to_owned()),
        })
        .collect()
}

/// Decodes one binary KORE pattern.
pub fn decode(src: &[u8]) -> Result<Pattern> {
    if !src.starts_with(MAGIC) || src.len() < 11 {
        return Err("not binary KORE".to_owned());
    }
    let major = u16::from_le_bytes([src[5], src[6]]);
    let minor = u16::from_le_bytes([src[7], src[8]]);
    let mut decoder = Decoder { src, pos: 11, fixed: (major, minor) == (1, 0) };
    // From 1.2.0, an 8-byte length of the pattern follows the version
    if (major, minor) >= (1, 2) {
        let len = decoder.fixed(8)?;
        if len != 0 {
            decoder.src = src
                .get(..decoder.pos + len)
                .ok_or("pattern beyond the end of binary KORE")?;
        }
    }
    let mut stack: Vec<Item> = Vec::new();
    while decoder.pos < decoder.src.len() {
        match decoder.byte()? {
            // A string pattern
            0x05 => {
                let text = text_of_bytes(&decoder.string()?);
                stack.push(Item::Pattern(Pattern::Str(text)));
            }
            // A composite sort: its arguments, then arity and name
            0x06 => {
                let arity = decoder.length(2)?;
                let name = decoder.name()?;
                let sorts = pop_sorts(&mut stack, arity)?;
                stack.push(Item::Sort(Sort::Sort(name, sorts)));
            }
            0x07 => {
                let name = decoder.name()?;
                stack.push(Item::Sort(Sort::Var(name)));
            }
            // A symbol: its formal sorts, then their number and its name
            0x08 => {
                let arity = decoder.length(2)?;
                let name = decoder.name()?;
                let sorts = pop_sorts(&mut stack, arity)?;
                stack.push(Item::Symbol(name, sorts));
            }
            // A composite pattern: its arguments, then its symbol and arity
            0x04 => {
                let arity = decoder.length(2)?;
                let Some(Item::Symbol(name, sorts)) = stack.pop() else {
                    return Err("expected a symbol in binary KORE".to_owned());
                };
                let start = stack
                    .len()
                    .checked_sub(arity)
                    .ok_or("too few arguments in binary KORE")?;
                let args = stack
                    .drain(start..)
                    .map(|item| match item {
                        Item::Pattern(pattern) => Ok(pattern),
                        _ => Err("expected a pattern in binary KORE".to_owned()),
                    })
                    .collect::<Result<Vec<_>>>()?;
                stack.push(Item::Pattern(Pattern::App(name, sorts, args)));
            }
            // A variable: its sort, then the variable
            0x09 => {
                if decoder.byte()? != 0x0d {
                    return Err("expected a variable in binary KORE".to_owned());
                }
                let name = decoder.name()?;
                let Some(Item::Sort(sort)) = stack.pop() else {
                    return Err("expected the sort of a variable in binary KORE".to_owned());
                };
                let pattern = if name.starts_with('@') {
                    Pattern::SetVar(name, sort)
                } else {
                    Pattern::Var(name, sort)
                };
                stack.push(Item::Pattern(pattern));
            }
            byte => return Err(format!("unexpected byte {byte:#04x} in binary KORE")),
        }
    }
    match (stack.pop(), stack.is_empty()) {
        (Some(Item::Pattern(pattern)), true) => Ok(pattern),
        _ => Err("binary KORE is not one pattern".to_owned()),
    }
}
