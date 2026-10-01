//! Pattern-set operations for table exclusiveness and exhaustiveness
//!
//! `PatternSet` contains the notation-type alternatives for one argument,
//! while `PatternSets` is their Cartesian product across one table row.
//! Rows are exclusive when no two products overlap
//! and exhaustive when subtracting them all from the type's product
//! leaves nothing.

use crate::lang::common::{ds::set::PhraseSet, source::Span};

use crate::lang::il::ast;

use super::super::{AlgoError, error};

// == Pattern sets

// - Single argument

/// A notation-type set compared by syntax.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PatternSet(PhraseSet<ast::NotTyp>);

impl PatternSet {
    /// Iterates over notation alternatives in syntax order.
    pub fn iter(&self) -> impl Iterator<Item = &ast::NotTyp> {
        self.0.iter()
    }

    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    fn intersection(&self, other: &Self) -> Self {
        let not_typs = self.0.intersection(&other.0);
        Self(not_typs)
    }

    fn difference(&self, other: &Self) -> Self {
        let not_typs = self.0.difference(&other.0);
        Self(not_typs)
    }
}

impl FromIterator<ast::NotTyp> for PatternSet {
    fn from_iter<T: IntoIterator<Item = ast::NotTyp>>(not_typs: T) -> Self {
        let not_typs = not_typs.into_iter().collect();
        Self(not_typs)
    }
}

// - Table row

/// Ordered pattern sets for the arguments of one table row.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PatternSets(Vec<PatternSet>);

impl PatternSets {
    /// Iterates over pattern sets in argument order.
    pub fn iter(&self) -> impl Iterator<Item = &PatternSet> {
        self.0.iter()
    }
}

impl FromIterator<PatternSet> for PatternSets {
    fn from_iter<T: IntoIterator<Item = PatternSet>>(pattern_sets: T) -> Self {
        let pattern_sets = pattern_sets.into_iter().collect();
        Self(pattern_sets)
    }
}

// == Exclusiveness checks

/// Rejects rows whose argument counts differ.
fn check_arity(
    span: &Span,
    pattern_sets_l: &PatternSets,
    pattern_sets_r: &PatternSets,
) -> Result<(), AlgoError> {
    let expected = pattern_sets_l.0.len();
    let actual = pattern_sets_r.0.len();
    if expected == actual {
        return Ok(());
    }
    Err(error::table::table_pattern_arity_mismatch(span, expected, actual))
}

/// Checks whether two rows can match the same input.
pub fn has_overlap(
    span: &Span,
    pattern_sets_l: &PatternSets,
    pattern_sets_r: &PatternSets,
) -> Result<bool, AlgoError> {
    check_arity(span, pattern_sets_l, pattern_sets_r)?;
    // Rows overlap only if every position shares an alternative
    let has_overlap =
        pattern_sets_l
            .0
            .iter()
            .zip(&pattern_sets_r.0)
            .all(|(pattern_set_l, pattern_set_r)| {
                let intersection = pattern_set_l.intersection(pattern_set_r);
                !intersection.is_empty()
            });
    Ok(has_overlap)
}

/// Finds the first overlapping pair of row indices in source order.
pub fn find_overlap(
    span: &Span,
    pattern_sets_by_row: &[PatternSets],
) -> Result<Option<(usize, usize)>, AlgoError> {
    // Keep the earlier row outermost so the selected pair remains deterministic
    for (idx, pattern_sets) in pattern_sets_by_row.iter().enumerate() {
        for (idx_other, pattern_sets_other) in pattern_sets_by_row.iter().enumerate().skip(idx + 1)
        {
            // Return both identities so callers can locate the original rows
            if has_overlap(span, pattern_sets, pattern_sets_other)? {
                return Ok(Some((idx, idx_other)));
            }
        }
    }
    Ok(None)
}

// == Exhaustiveness checks

/// Removes one row's patterns from a total, returning the remaining fragments.
pub fn subtract(
    span: &Span,
    pattern_sets_total: &PatternSets,
    pattern_sets: &PatternSets,
) -> Result<Vec<PatternSets>, AlgoError> {
    if !has_overlap(span, pattern_sets_total, pattern_sets)? {
        let pattern_sets_total = pattern_sets_total.clone();
        return Ok(vec![pattern_sets_total]);
    }

    // F × F' − W × W' = (F − W) × F' ∪ (F ∩ W) × (F' − W')
    let mut pattern_sets_rows_fragment = Vec::new();
    let pattern_sets_prefix = Vec::new();
    let mut pattern_sets_prefix = PatternSets(pattern_sets_prefix);
    for (index, (pattern_set_total, pattern_set)) in
        pattern_sets_total.0.iter().zip(&pattern_sets.0).enumerate()
    {
        let pattern_set_diff = pattern_set_total.difference(pattern_set);
        let pattern_set_inter = pattern_set_total.intersection(pattern_set);

        // (F − W) × F'
        if !pattern_set_diff.is_empty() {
            let mut pattern_sets_fragment = pattern_sets_prefix.clone();
            pattern_sets_fragment.0.push(pattern_set_diff);
            pattern_sets_fragment
                .0
                .extend_from_slice(&pattern_sets_total.0[index + 1..]);
            pattern_sets_rows_fragment.push(pattern_sets_fragment);
        }

        // (F ∩ W) × (F' − W')
        if pattern_set_inter.is_empty() {
            break;
        }
        pattern_sets_prefix.0.push(pattern_set_inter);
    }
    Ok(pattern_sets_rows_fragment)
}

/// Subtracts every row from the total pattern space; what remains is uncovered.
pub fn find_missing(
    span: &Span,
    pattern_sets_total: &PatternSets,
    pattern_sets_by_row: &[PatternSets],
) -> Result<Vec<PatternSets>, AlgoError> {
    let mut pattern_sets_rows_missing = vec![pattern_sets_total.clone()];
    // Each row carves fragments out of what the previous rows left
    for pattern_sets in pattern_sets_by_row {
        let mut pattern_sets_rows_remaining = Vec::new();
        for pattern_sets_total in &pattern_sets_rows_missing {
            let pattern_sets_rows_fragment = subtract(span, pattern_sets_total, pattern_sets)?;
            pattern_sets_rows_remaining.extend(pattern_sets_rows_fragment);
        }
        pattern_sets_rows_missing = pattern_sets_rows_remaining;
    }
    Ok(pattern_sets_rows_missing)
}
