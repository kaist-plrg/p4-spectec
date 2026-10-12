//! Visible widths of prose before AsciiDoc markup
//!
//! ```text
//! Seq([Text("a "), Code(Token("bc"))])   -> 4
//! ```

use super::doc::{Code, Prose};

// == Visible widths
//
//   Seq([Text("a "), Code(Token("bc"))]).width()   -> 4

impl Prose {
    /// Measures prose before adding code and link markup.
    pub fn width(&self) -> usize {
        match self {
            Prose::Text(text) => text.len(),
            Prose::Code(code) | Prose::PlainCode(code) => code.width(),
            Prose::Link(_, prose) => prose.width(),
            Prose::Empty => 0,
            Prose::Seq(proses) => proses.iter().map(Prose::width).sum(),
        }
    }
}

impl Code {
    /// Measures code before adding monospace and link markup.
    pub fn width(&self) -> usize {
        match self {
            Code::Token(text) => text.len(),
            Code::Link(_, code) => code.width(),
            Code::Seq(codes) => codes.iter().map(Code::width).sum(),
            Code::Empty => 0,
        }
    }
}
