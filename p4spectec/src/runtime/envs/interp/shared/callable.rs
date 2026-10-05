//! Prepared callable syntax and its local frame layout
//!
//! `Callable::prepare` runs the `Prepare` traversal once,
//! resolving every name to a slot and recording the layout its frames follow.

use std::{fmt, rc::Rc};

use crate::lang::traits::{
    eq::SyntaxEq,
    print::{Print, Printer},
};

use crate::interp::shared::prepare::Prepare;

use super::frame::FrameLayout;

/// Callable syntax paired with its interpreter-owned local layout.
#[derive(Clone, Debug, PartialEq)]
pub struct Callable<T> {
    /// The prepared definition.
    pub def: T,
    /// Slot layout of the definition's frames.
    pub layout: Rc<FrameLayout>,
}

impl<T> Callable<T> {
    /// Prepares a definition, collecting its slot layout.
    pub fn prepare<S: Prepare<Output = T>>(source: S) -> Self {
        // The traversal fills the layout as it resolves names
        let mut layout = FrameLayout::default();
        let def = source.prepare(&mut layout);
        Self { def, layout: Rc::new(layout) }
    }
}

impl<T: Print> Print for Callable<T> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        self.def.print(printer)
    }
}

impl<T: SyntaxEq> SyntaxEq for Callable<T> {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.def.syntax_eq(&other.def)
    }
}
