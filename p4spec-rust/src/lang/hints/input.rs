//! Input hints for relations
//!
//! `hint(input %0 %2)` says which notation positions of a relation are inputs;
//! the rest are outputs the relation computes.
//! `split` and `combine` move between source order and the input/output lists.

use crate::lang::{
    common::source::Phrase,
    el::ast::{Exp, ExpKind, Hole},
    traits::eq::SyntaxEq,
};
use thiserror::Error;

/// Relation input positions in source order
///
/// `new` does not validate indices;
/// call `validate` when the relation arity is known;
/// operations such as `split` validate before consuming items.
#[derive(Clone, Debug, Eq)]
pub struct InputHint {
    /// Input positions, in the order the hint lists them.
    indices: Vec<Phrase<usize>>,
}

/// An invalid hint or a list that does not fit it.
#[derive(Clone, Debug, Error, PartialEq, Eq)]
pub enum InputError {
    /// No input position at all.
    #[error("input hint is empty")]
    Empty,

    /// A position listed twice.
    #[error("input hint contains duplicate index {}", idx.node)]
    DuplicateIndex { idx: Box<Phrase<usize>>, idx_previous: Box<Phrase<usize>> },

    /// A position past the relation's arity.
    #[error("input hint index {} is out of bounds for arity {arity}", idx.node)]
    IndexOutOfBounds { idx: Box<Phrase<usize>>, arity: usize },

    /// The input list has the wrong length.
    #[error("input hint expects {expected} input items, but got {actual}")]
    InputCountMismatch { expected: usize, actual: usize },

    /// The output list has the wrong length.
    #[error("input hint expects {expected} output items, but got {actual}")]
    OutputCountMismatch { expected: usize, actual: usize },
}

impl InputHint {
    /// Stores located indices without validation.
    pub fn new(indices: Vec<Phrase<usize>>) -> Self {
        Self { indices }
    }

    /// Borrows located positions in the order the hint lists them.
    pub fn indices(&self) -> &[Phrase<usize>] {
        &self.indices
    }

    /// Returns located positions in the order the hint lists them.
    pub fn into_indices(self) -> Vec<Phrase<usize>> {
        self.indices
    }
}

// Syntax equivalence of hints

impl PartialEq for InputHint {
    fn eq(&self, other: &Self) -> bool {
        self.indices
            .iter()
            .map(|idx| idx.node)
            .eq(other.indices.iter().map(|idx| idx.node))
    }
}

impl SyntaxEq for InputHint {
    fn syntax_eq(&self, other: &Self) -> bool {
        self == other
    }
}

// Creating hints

/// Reads a hint from `%N` holes, one or a sequence; anything else is no hint.
pub fn init(hint_exp: &Exp) -> Option<InputHint> {
    let indices = match &hint_exp.node {
        // A sequence of `%N` holes, all of which must be holes
        ExpKind::Seq(hint_exps) => hint_exps
            .iter()
            .map(|hint_exp| match hint_exp.node {
                ExpKind::Hole(Hole::Num(idx)) => {
                    Some(crate::phrase!(node: idx, span: hint_exp.span.clone()))
                }
                _ => None,
            })
            .collect(),
        // A single hole
        ExpKind::Hole(Hole::Num(idx)) => {
            Some(vec![crate::phrase!(node: *idx, span: hint_exp.span.clone())])
        }
        // Anything else is not an input hint
        _ => None,
    }?;
    Some(InputHint::new(indices))
}

// Validating hints

/// Validates non-empty, unique positions within `arity`
pub fn validate(hint: &InputHint, arity: usize) -> Result<(), InputError> {
    if hint.indices.is_empty() {
        return Err(InputError::Empty);
    }
    // Each position at most once
    for (idx_hint, idx) in hint.indices.iter().enumerate() {
        if let Some(idx_previous) = hint.indices[..idx_hint]
            .iter()
            .find(|idx_previous| idx_previous.node == idx.node)
        {
            return Err(InputError::DuplicateIndex {
                idx: Box::new(idx.clone()),
                idx_previous: Box::new(idx_previous.clone()),
            });
        }
    }
    // Every position within the arity
    if let Some(idx) = hint.indices.iter().find(|idx| idx.node >= arity) {
        return Err(InputError::IndexOutOfBounds { idx: Box::new(idx.clone()), arity });
    }
    Ok(())
}

/// Validates operational positions, including a zero-arity relation's default.
fn validate_items(hint: &InputHint, arity: usize) -> Result<(), InputError> {
    if arity == 0 && hint.indices.is_empty() { Ok(()) } else { validate(hint, arity) }
}

// Splitting and combining expressions based on input hints

/// Splits items into input and output positions
///
/// Validates the hint against `items.len()`
pub fn split<Item>(
    hint: &InputHint,
    items: Vec<Item>,
) -> Result<(Vec<Item>, Vec<Item>), InputError> {
    validate_items(hint, items.len())?;
    let mut items_input = Vec::new();
    let mut items_output = Vec::new();
    // Inputs and outputs each keep source order
    for (idx, item) in items.into_iter().enumerate() {
        if hint.indices.iter().any(|idx_input| idx_input.node == idx) {
            items_input.push(item);
        } else {
            items_output.push(item);
        }
    }
    Ok((items_input, items_output))
}

/// Reconstructs source-order items from input and output positions
///
/// Validates the hint and both item counts.
pub fn combine<Item>(
    hint: &InputHint,
    items_input: Vec<Item>,
    items_output: Vec<Item>,
) -> Result<Vec<Item>, InputError> {
    // The hint must fit the combined length
    let input_actual = items_input.len();
    let output_actual = items_output.len();
    let items_len = input_actual + output_actual;
    validate_items(hint, items_len)?;
    let input_expected = hint.indices.len();
    let output_expected = items_len - input_expected;
    // Both lists must have the lengths the hint implies
    if input_actual != input_expected {
        return Err(InputError::InputCountMismatch {
            expected: input_expected,
            actual: input_actual,
        });
    }
    if output_actual != output_expected {
        return Err(InputError::OutputCountMismatch {
            expected: output_expected,
            actual: output_actual,
        });
    }

    // Walk source positions, drawing from whichever list owns each
    let mut items_input = items_input.into_iter();
    let mut items_output = items_output.into_iter();
    let mut items = Vec::with_capacity(items_len);
    // Refill source positions from the two lists in turn
    for idx in 0..items_len {
        let item = if hint.indices.iter().any(|idx_input| idx_input.node == idx) {
            items_input.next().ok_or(InputError::InputCountMismatch {
                expected: input_expected,
                actual: input_actual,
            })?
        } else {
            items_output.next().ok_or(InputError::OutputCountMismatch {
                expected: output_expected,
                actual: output_actual,
            })?
        };
        items.push(item);
    }
    Ok(items)
}

// Checking if a hint is conditional

/// Reports whether every item is an input
///
/// Validates the hint against `items.len()`
pub fn is_conditional<Item>(hint: &InputHint, items: &[Item]) -> Result<bool, InputError> {
    validate_items(hint, items.len())?;
    Ok(items
        .iter()
        .enumerate()
        .all(|(idx, _)| hint.indices.iter().any(|idx_input| idx_input.node == idx)))
}
