//! Identifiers shared by the language representations
//!
//! An identifier may carry a suffix after `_` or `'` (`x_1`, `x'`)
//! that distinguishes occurrences of the same base name;
//! `strip_suffix` recovers the base.

use std::rc::Rc;

use crate::lang::{
    common::source::Phrase,
    traits::print::{Print, Printer},
};

/// Source-annotated identifier, sharing its name between copies.
pub type Id = Phrase<Rc<str>>;

impl Print for Id {
    fn print(&self, printer: &mut Printer<'_>) -> std::fmt::Result {
        printer.write(&self.node)
    }
}

impl Id {
    /// Strips identifier suffixes while preserving the source span.
    pub fn strip_suffix(&self) -> Self {
        // Share the name when there is no suffix to strip
        let name = strip_suffix(&self.node);
        let node = if name.len() == self.node.len() { Rc::clone(&self.node) } else { name.into() };
        crate::phrase! { node: node, span: self.span }
    }
}

/// Strips identifier suffixes while preserving all-underscore tails.
pub fn strip_suffix(id: &str) -> &str {
    // The suffix starts at the first underscore or apostrophe
    let underscore = id.find('_');
    let apostrophe = id.find('\'');
    let suffix_index = match (underscore, apostrophe) {
        // No suffix
        (None, None) => return id,
        // A tail of only underscores is part of the name
        (Some(index), None) if id[index..].bytes().all(|byte| byte == b'_') => {
            return id;
        }
        // One marker present
        (Some(index), None) | (None, Some(index)) => index,
        // Both present: the earlier one starts the suffix
        (Some(index_l), Some(index_r)) => index_l.min(index_r),
    };
    &id[..suffix_index]
}
