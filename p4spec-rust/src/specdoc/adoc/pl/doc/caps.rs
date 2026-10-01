//! Capitalization of the first eligible prose text
//!
//! ```text
//! Seq([Empty, Text("hello")])                         -> Seq([Empty, Text("Hello")])
//! Item { 0, Ordered(None), Code(Token("x")), Empty }  -> unchanged, code stops the search
//! ```

use super::doc::{Block, Item, Prose};

// == Capitalization

/// Whether capitalization found text, can continue, or reached protected prose.
enum CapStep {
    /// Capitalized an initial letter, ending the search.
    Done,
    /// Found no eligible initial letter, allowing the next piece to be tried.
    Skip,
    /// Reached protected code or a link, ending the search without changes.
    Stop,
}

impl Prose {
    // - Prose capitalization
    //
    //   Seq([Empty, Text("hello")])               -> Hello
    //   Seq([Text("("), Text("a")])               -> (A
    //   Seq([Code(Token("x")), Text(" stays")])   -> ``x`` stays

    fn capitalize_step(&mut self) -> CapStep {
        match self {
            Prose::Text(text) => {
                // Capitalize only an initial ASCII letter
                if text.starts_with(|character: char| character.is_ascii_alphabetic()) {
                    text[..1].make_ascii_uppercase();
                    CapStep::Done
                } else {
                    CapStep::Skip
                }
            }
            Prose::Code(_) | Prose::PlainCode(_) | Prose::Link(..) | Prose::Fallthrough(..) => {
                CapStep::Stop
            }
            Prose::Seq(proses) => {
                // Empty or punctuation-only pieces leave the next piece eligible
                for prose in proses {
                    match prose.capitalize_step() {
                        CapStep::Skip => {}
                        step => return step,
                    }
                }
                CapStep::Skip
            }
            Prose::Empty => CapStep::Skip,
        }
    }

    /// Capitalizes the first eligible text without changing embedded code or links.
    pub fn capitalize_first(mut self) -> Prose {
        self.capitalize_step();
        self
    }
}

impl Block {
    // - Block capitalization
    //
    //   Item { 0, Ordered(None), Text("if x"), Empty }                      -> . If x
    //   Seq([Item { 0, Ordered(None), Empty, Empty }, Inline(Text("x"))])   -> .
    //                                                                          X
    //   Seq([Table { .. }, Inline(Text("x"))])                              -> x unchanged

    fn capitalize_step(&mut self) -> CapStep {
        match self {
            Block::Empty | Block::Raw(_) => CapStep::Skip,
            Block::Inline(prose) => prose.capitalize_step(),
            Block::Item(Item { prose_head, block_body, .. }) => {
                match prose_head.capitalize_step() {
                    // An item without eligible heading text continues into its body
                    CapStep::Skip => block_body.capitalize_step(),
                    step => step,
                }
            }
            Block::Concat(blocks) | Block::Seq(blocks) => {
                for block in blocks {
                    match block.capitalize_step() {
                        CapStep::Skip => {}
                        step => return step,
                    }
                }
                CapStep::Skip
            }
            Block::Table(_) => CapStep::Stop,
        }
    }

    /// Capitalizes the first eligible block heading or inline text.
    pub fn capitalize_first(mut self) -> Block {
        self.capitalize_step();
        self
    }
}
