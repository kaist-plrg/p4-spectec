//! Expr diagnostics for interpreter operations
//!
//! Constructors identify runtime checks without deciding whether callers retry.

use super::diagnostic;
use crate::diagnostic::Diagnostic;
use num_bigint::BigInt;

const CONCATENATION_OPERAND_MISMATCH: &str = "runtime/concatenation-operand-mismatch";

/// Reports concatenation operand mismatch.
pub fn concatenation_operand_mismatch() -> Diagnostic {
    diagnostic(
        CONCATENATION_OPERAND_MISMATCH,
        "concatenation expects either two texts or two lists",
        Vec::new(),
    )
}

const LENGTH_OPERAND_MISMATCH: &str = "runtime/length-operand-mismatch";

/// Reports length operand mismatch.
pub fn length_operand_mismatch() -> Diagnostic {
    diagnostic(
        LENGTH_OPERAND_MISMATCH,
        "length operation expects either a text or a list",
        Vec::new(),
    )
}

const FIELD_UNDEFINED: &str = "runtime/field-undefined";

/// Reports undefined field.
pub fn field_undefined() -> Diagnostic {
    diagnostic(FIELD_UNDEFINED, "undefined structure field", Vec::new())
}

const TEXT_SLICE_BOUNDARY_MISMATCH: &str = "runtime/text-slice-boundary-mismatch";

/// Reports text slice boundary mismatch.
pub fn text_slice_boundary_mismatch() -> Diagnostic {
    diagnostic(
        TEXT_SLICE_BOUNDARY_MISMATCH,
        "text byte slice is not on UTF-8 boundaries",
        Vec::new(),
    )
}

const INDEX_OPERAND_MISMATCH: &str = "runtime/index-operand-mismatch";

/// Reports index operand mismatch.
pub fn index_operand_mismatch() -> Diagnostic {
    diagnostic(INDEX_OPERAND_MISMATCH, "indexing expects either a text or a list", Vec::new())
}

const SLICE_OPERAND_MISMATCH: &str = "runtime/slice-operand-mismatch";

/// Reports slice operand mismatch.
pub fn slice_operand_mismatch() -> Diagnostic {
    diagnostic(SLICE_OPERAND_MISMATCH, "slicing expects either a text or a list", Vec::new())
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

const TUPLE_CAST_ARITY_MISMATCH: &str = "runtime/tuple-cast-arity-mismatch";

/// Reports tuple cast arity mismatch.
pub fn tuple_cast_arity_mismatch(expected: usize, actual: usize) -> Diagnostic {
    diagnostic(
        TUPLE_CAST_ARITY_MISMATCH,
        "tuple cast arity mismatch",
        vec![format!("expected: {expected}, actual: {actual}")],
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
