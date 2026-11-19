//! Prepared syntax that runs in its own frame, with that frame's layout
//!
//! `Callable` stores prepared syntax beside the layout its frames follow.
//! Preparation constructs one for each unit that binds its own variables:
//! a defined relation or function in SL and PL,
//! and a rule group, table row, or clause in AL.

use std::rc::Rc;

use super::frame::FrameLayout;

/// Prepared syntax paired with the layout of the frames it runs in.
#[derive(Clone, Debug, PartialEq)]
pub struct Callable<T> {
    /// The prepared syntax.
    pub def: T,
    /// Slot layout of the definition's frames.
    pub layout: Rc<FrameLayout>,
}
