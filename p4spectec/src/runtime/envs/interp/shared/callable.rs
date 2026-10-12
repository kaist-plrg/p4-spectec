//! Prepared callable syntax and its local frame layout
//!
//! `Callable` stores a definition beside the layout followed by its frames.
//! Each interpreter's top-level preparation constructs its callables.

use std::rc::Rc;

use super::frame::FrameLayout;

/// Callable syntax paired with its interpreter-owned local layout.
#[derive(Clone, Debug, PartialEq)]
pub struct Callable<T> {
    /// The prepared definition.
    pub def: T,
    /// Slot layout of the definition's frames.
    pub layout: Rc<FrameLayout>,
}
