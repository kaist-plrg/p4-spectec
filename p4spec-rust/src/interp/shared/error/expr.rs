//! Expr diagnostics for interpreter operations
//!
//! Builds diagnostics; callers choose whether to stop or try another candidate.

use super::diagnostic;
use crate::diagnostic::Diagnostic;
use num_bigint::BigInt;

const TEXT_SLICE_BOUNDARY_MISMATCH: &str = "runtime/text-slice-boundary-mismatch";

/// Reports text slice boundary mismatch.
pub fn text_slice_boundary_mismatch() -> Diagnostic {
    diagnostic(
        TEXT_SLICE_BOUNDARY_MISMATCH,
        "text byte slice is not on UTF-8 boundaries",
        Vec::new(),
    )
}

const CHARACTER_UPDATE_LENGTH_MISMATCH: &str = "runtime/character-update-length-mismatch";

/// Reports character update length mismatch.
pub fn character_update_length_mismatch() -> Diagnostic {
    diagnostic(
        CHARACTER_UPDATE_LENGTH_MISMATCH,
        "updating a character requires a single-character text",
        Vec::new(),
    )
}

const INDEX_OUT_OF_BOUNDS: &str = "runtime/index-out-of-bounds";

/// Reports index out of bounds.
pub fn index_out_of_bounds(idx: BigInt, len: usize) -> Diagnostic {
    diagnostic(INDEX_OUT_OF_BOUNDS, format!("index {idx} out of bounds [0, {len})"), Vec::new())
}

const SLICE_OUT_OF_BOUNDS: &str = "runtime/slice-out-of-bounds";

/// Reports slice out of bounds.
pub fn slice_out_of_bounds(idx: BigInt, end: BigInt, size: usize) -> Diagnostic {
    diagnostic(
        SLICE_OUT_OF_BOUNDS,
        format!("slice [{idx}, {end}) out of bounds [0, {size})"),
        Vec::new(),
    )
}

const TEXT_SLICE_UPDATE_LENGTH_MISMATCH: &str = "runtime/text-slice-update-length-mismatch";

/// Reports text slice update length mismatch.
pub fn text_slice_update_length_mismatch(len: usize, actual: usize) -> Diagnostic {
    diagnostic(
        TEXT_SLICE_UPDATE_LENGTH_MISMATCH,
        format!(
            "updating a slice of length {len} requires a text of length {len}, but got length {actual}"
        ),
        Vec::new(),
    )
}

const LIST_SLICE_UPDATE_LENGTH_MISMATCH: &str = "runtime/list-slice-update-length-mismatch";

/// Reports list slice update length mismatch.
pub fn list_slice_update_length_mismatch(len: usize, actual: usize) -> Diagnostic {
    diagnostic(
        LIST_SLICE_UPDATE_LENGTH_MISMATCH,
        format!(
            "updating a slice of length {len} requires a list of length {len}, but got length {actual}"
        ),
        Vec::new(),
    )
}
