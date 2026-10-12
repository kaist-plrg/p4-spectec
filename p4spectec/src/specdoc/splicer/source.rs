//! Byte-positioned skeleton source
//!
//! Marker syntax is ASCII; untouched text is copied as UTF-8 slices.
//! Positions use one-based lines and zero-based byte columns.

use crate::lang::common::source::{FileId, Position, Span};

/// A skeleton and its current byte offset.
pub struct Source<'a> {
    file: FileId,
    text: &'a str,
    pos: usize,
}

impl<'a> Source<'a> {
    /// Starts reading a skeleton at its first byte.
    pub fn new(file: &'a str, text: &'a str) -> Self {
        Self { file: FileId::intern(file), text, pos: 0 }
    }

    /// Reports whether the entire skeleton has been consumed.
    pub fn eos(&self) -> bool {
        self.pos == self.text.len()
    }

    /// Returns the next byte, if present.
    pub fn get(&self) -> Option<u8> {
        self.text.as_bytes().get(self.pos).copied()
    }

    /// Returns the unconsumed source text.
    pub fn remaining(&self) -> &'a str {
        &self.text[self.pos..]
    }

    /// Returns the current byte offset.
    pub fn offset(&self) -> usize {
        self.pos
    }

    /// Returns the current source position.
    pub fn position(&self) -> Position {
        // Count source lines before the cursor
        let text = &self.text[..self.pos];
        let line = text.bytes().filter(|ch| *ch == b'\n').count() + 1;
        // Locate the byte column within the final line
        let column = text
            .rfind('\n')
            .map_or(text.len(), |pos| text.len() - pos - 1);
        Position::new(self.file, line, column)
    }

    /// Locates the cursor without consuming input.
    pub fn span(&self) -> Span {
        let pos = self.position();
        Span::new(pos, pos)
    }

    /// Advances by a byte count ending on a UTF-8 boundary.
    pub fn advn_bytes(&mut self, len: usize) {
        assert!(self.text.is_char_boundary(self.pos + len));
        self.pos += len;
    }

    /// Advances over one complete character.
    pub fn advn_char(&mut self) {
        if let Some(ch) = self.remaining().chars().next() {
            self.advn_bytes(ch.len_utf8());
        }
    }
}
