//! Calls contained in language syntax
//!
//! `HasCall` collects located call expressions in preorder.
//! A call precedes calls in its arguments; siblings retain their syntax order.

/// Collects call expressions contained in syntax.
pub trait HasCall {
    /// The located expression type of the language.
    type Exp;

    /// Returns calls in preorder, including the receiver when it is a call.
    fn nested_call(&self) -> Vec<&Self::Exp>;

    /// Reports whether the syntax contains a call expression.
    fn has_call(&self) -> bool {
        !self.nested_call().is_empty()
    }
}
