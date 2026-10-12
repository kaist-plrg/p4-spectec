//! Cached parsing of notation trees
//!
//! `mixop` parses a mixop from its text once and caches it.

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::frontend;

use super::Mixop;

// = Mixop parsing

thread_local! {
    /// Parsed mixops by their source text.
    static MIXOP_CACHE: RefCell<HashMap<Rc<str>, Rc<Mixop>>> = RefCell::new(HashMap::new());
}

/// Parses a mixop from its text, reusing an earlier parse of the same text.
pub(crate) fn mixop(mixop_text: &str) -> Rc<Mixop> {
    MIXOP_CACHE.with(|cache| {
        // Cached: share it
        if let Some(mixop_tree) = cache.borrow().get(mixop_text).cloned() {
            return mixop_tree;
        }

        // First use: parse and remember
        let mixop_tree = frontend::parse::parse_mixop(mixop_text)
            .expect("value constructor contains a valid SpecTec mixop");
        let mixop_tree = Rc::new(mixop_tree);
        cache
            .borrow_mut()
            .insert(Rc::from(mixop_text), Rc::clone(&mixop_tree));
        mixop_tree
    })
}
