//! Argument mapping for mixfix forms
//!
//! `map` and `try_map` preserve the mixop and argument order.
//! The mapped form keeps one argument per position.

use super::Mixfix;

// - Arguments

impl<M, T> Mixfix<M, T> {
    /// Maps each argument in order, keeping the mixop.
    pub fn map<U>(&self, map_arg: impl FnMut(&T) -> U) -> Mixfix<M, U>
    where
        M: Clone,
    {
        Mixfix { mixop: self.mixop.clone(), args: self.args.iter().map(map_arg).collect() }
    }

    /// Maps each argument in order, stopping at the first error.
    pub fn try_map<U, E>(&self, map_arg: impl FnMut(&T) -> Result<U, E>) -> Result<Mixfix<M, U>, E>
    where
        M: Clone,
    {
        let args = self.args.iter().map(map_arg).collect::<Result<_, _>>()?;
        Ok(Mixfix { mixop: self.mixop.clone(), args })
    }
}
