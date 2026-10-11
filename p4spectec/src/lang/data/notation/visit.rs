//! Shared items of notation traversal
//!
//! Tree and flat traversals yield atoms and argument positions in reading order.
//! Printing and source-location calculation consume these items.

use super::AtomPhrase;

/// An atom or argument position encountered in notation order.
#[derive(Clone, Copy, Debug)]
pub(crate) enum VisitItem<'a> {
    /// A literal atom.
    Atom(&'a AtomPhrase),
    /// The zero-based argument position.
    Arg(usize),
}
