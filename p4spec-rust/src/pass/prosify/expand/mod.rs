//! Lift nested calls into explicit SL let instructions

mod lift;
mod transform;

use crate::lang::sl::ast as sl;

/// Lifts nested calls in a structured-language specification.
pub(super) fn expand_spec(spec_sl: sl::Spec) -> sl::Spec {
    transform::expand_spec(spec_sl)
}
