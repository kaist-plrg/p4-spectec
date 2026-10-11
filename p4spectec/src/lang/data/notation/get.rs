//! Projections of generic mixfix forms
//!
//! Accessors borrow or consume the parts without changing their pairing.

use super::Mixfix;

// - Parts

impl<M, T> Mixfix<M, T> {
    /// The form, with argument positions.
    pub fn mixop(&self) -> &M {
        &self.mixop
    }

    /// The arguments in notation order.
    pub fn args(&self) -> &[T] {
        &self.args
    }

    /// The arguments in notation order, for rewriting in place.
    pub fn args_mut(&mut self) -> &mut [T] {
        &mut self.args
    }

    /// The number of argument positions.
    pub fn arity(&self) -> usize {
        self.args.len()
    }

    /// The arguments, dropping the mixop.
    pub fn into_args(self) -> Vec<T> {
        self.args
    }

    /// The mixop and the arguments.
    pub fn into_parts(self) -> (M, Vec<T>) {
        (self.mixop, self.args)
    }
}
