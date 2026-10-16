//! Projections of tree values
//!
//! Decomposes a filled case into its mixop and argument values.

use crate::lang::data::notation::tree::Mixop;

use super::{Value, ValueCase};

// - Cases

impl ValueCase {
    /// Splits a filled tree into its mixop and arguments in notation order.
    pub fn into_parts(self) -> (Mixop, Vec<Value>) {
        let mut values = Vec::new();
        let mixop = self.into_parts_inner(&mut values);
        (mixop, values)
    }

    /// Moves each argument into the output as its position is visited.
    fn into_parts_inner(self, values: &mut Vec<Value>) -> Mixop {
        match self {
            Self::Arg(value) => {
                values.push(*value);
                Mixop::Arg
            }
            Self::Atom(atom) => Mixop::Atom(atom),
            Self::Brack(atom_l, value_case, atom_r) => {
                Mixop::Brack(atom_l, Box::new(value_case.into_parts_inner(values)), atom_r)
            }
            Self::Infix(value_case_l, atom, value_case_r) => {
                let mixop_l = value_case_l.into_parts_inner(values);
                let mixop_r = value_case_r.into_parts_inner(values);
                Mixop::Infix(Box::new(mixop_l), atom, Box::new(mixop_r))
            }
            Self::Seq(value_cases) => Mixop::Seq(
                value_cases
                    .into_iter()
                    .map(|value_case| value_case.into_parts_inner(values))
                    .collect(),
            ),
        }
    }
}
