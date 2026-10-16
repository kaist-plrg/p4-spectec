//! Width-fitting layout of elaboration-language AsciiDoc documents
//!
//! ```text
//! Doc::group(Doc::concat([
//!     Doc::text("f("),
//!     Doc::nest(4, Doc::concat([
//!         Doc::break_(""), Doc::text("x,"), Doc::break_(" "), Doc::text("y"),
//!     ])),
//!     Doc::text(")"),
//! ]))
//!
//!   render(7)   -> f(x, y)
//!   render(6)   -> f(
//!                      x,
//!                      y)
//! ```

use super::doc::Doc;

// == Layout state

// - Layout modes
//
//   Flat, Break(" ")          -> " "
//   Broken, Break(" ")        -> newline and indentation

/// Controls whether optional breaks emit text or start a new line.
#[derive(Clone, Copy)]
enum Mode {
    /// Emits each break's stored text within a group that fits.
    Flat,
    /// Starts indented lines, allowing nested groups to fit independently.
    Broken,
}

// - Rendering commands
//
//   Command { indent: 4, mode: Broken, doc: Break("") }   -> newline and four spaces

/// One pending document on the layout work stack.
#[derive(Clone, Copy)]
struct Command<'a> {
    indent: usize,
    mode: Mode,
    doc: &'a Doc,
}

// == Layout

impl Doc {
    // - Fitting
    //
    //   fits(3, [Text("ab"), Break(" "), Text("c")] in Flat)     -> false
    //   fits(3, [Text("ab"), Break(" "), Text("c")] in Broken)   -> true

    /// Tests whether the queued flat layout reaches a forced newline within width.
    fn fits(mut width_remaining: isize, mut commands: Vec<Command<'_>>) -> bool {
        // Stop lookahead when the current line exceeds its available width
        while width_remaining >= 0 {
            // Exhausting the pending document means the line fits
            let Some(Command { indent, mode, doc }) = commands.pop() else {
                return true;
            };
            match (doc, mode) {
                // Empty documents consume no space
                (Doc::Empty, _) => {}
                // Literal text and flat breaks consume their byte width
                (Doc::Text(text), _) | (Doc::Break(text), Mode::Flat) => {
                    width_remaining -= text.len() as isize
                }
                // Broken breaks and forced lines end the current line
                (Doc::Break(_), Mode::Broken) | (Doc::Line, _) => return true,
                (Doc::Concat(docs), _) => {
                    // Push in reverse order to visit documents in source order
                    commands.extend(docs.iter().rev().map(|doc| Command { indent, mode, doc }));
                }
                (Doc::Nest(offset, doc), _) => {
                    // Indentation takes effect at the next line break
                    commands.push(Command { indent: indent + offset, mode, doc });
                }
                (Doc::Group(doc), _) => {
                    // Lookahead retains the enclosing mode until a line ends
                    commands.push(Command { indent, mode, doc });
                }
            }
        }
        false
    }

    // - Rendering
    //
    //   group("f(" nest(4, break("") "x," break(" ") "y") ")").render(7)   -> f(x, y)
    //   group("f(" nest(4, break("") "x," break(" ") "y") ")").render(6)   -> f(
    //                                                                             x,
    //                                                                             y)

    /// Renders a document using group fitting at the requested positive width.
    pub(crate) fn render(&self, width: usize) -> String {
        assert!(width > 0, "Doc::render: width must be positive");

        let mut output = String::with_capacity(256);
        let mut column = 0;
        let mut commands = vec![Command { indent: 0, mode: Mode::Broken, doc: self }];

        // Keep nested layout traversal off the call stack
        while let Some(Command { indent, mode, doc }) = commands.pop() {
            match (doc, mode) {
                // Empty documents consume no space
                (Doc::Empty, _) => {}
                // Literal text and flat breaks consume their byte width
                (Doc::Text(text), _) | (Doc::Break(text), Mode::Flat) => {
                    output.push_str(text);
                    column += text.len();
                }
                // Broken breaks and forced lines start an indented line
                (Doc::Break(_), Mode::Broken) | (Doc::Line, _) => {
                    output.push('\n');
                    output.push_str(&" ".repeat(indent));
                    column = indent;
                }
                (Doc::Concat(docs), _) => {
                    // Push in reverse order to visit documents in source order
                    commands.extend(docs.iter().rev().map(|doc| Command { indent, mode, doc }));
                }
                (Doc::Nest(offset, doc), _) => {
                    // Indentation takes effect at the next line break
                    commands.push(Command { indent: indent + offset, mode, doc });
                }
                (Doc::Group(doc), _) => {
                    // Flatten the group only when it and the following text fit
                    let mode = match mode {
                        Mode::Flat => Mode::Flat,
                        Mode::Broken => {
                            let mut commands_flat = commands.clone();
                            commands_flat.push(Command { indent, mode: Mode::Flat, doc });
                            let width_remaining = width as isize - column as isize;
                            if Doc::fits(width_remaining, commands_flat) {
                                Mode::Flat
                            } else {
                                Mode::Broken
                            }
                        }
                    };
                    commands.push(Command { indent, mode, doc });
                }
            }
        }

        output
    }
}
