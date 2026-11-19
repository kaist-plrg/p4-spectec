//! Diagnostics for simulator operations and STF execution
//!
//! Constructors return reports; extern callers supply the fatal host wrapper.
//! Incoming reports and low-level value failures retain their own payloads.

use crate::lang::common::source::Span;

use crate::diagnostic::{Diagnostic, Report, Severity};

use super::io::{Expectation, Tx};

/// A diagnostic from simulator construction or execution.
pub type SimError = Box<Report>;

fn report(code: &str, message: impl Into<String>) -> SimError {
    Box::new(
        Diagnostic::new("sim", Severity::Error, Some(code.to_owned()), message, vec![], vec![])
            .into(),
    )
}

// = Architecture and control operations

const ARCHITECTURE_UNSUPPORTED: &str = "sim/architecture-unsupported";

/// Reports an architecture rejected by simulator selection.
pub(super) fn architecture_unsupported(arch: &str) -> SimError {
    report(ARCHITECTURE_UNSUPPORTED, format!("architecture {arch} is not supported"))
}

const CONTROL_OPERATION_UNSUPPORTED: &str = "sim/control-operation-unsupported";

/// Reports a control operation that is unsupported.
pub(super) fn control_operation_unsupported(message: impl Into<String>) -> SimError {
    report(CONTROL_OPERATION_UNSUPPORTED, message)
}

// = Assertions

const ASSERTION_UNMET: &str = "sim/assertion-unmet";

/// Reports an assertion that is unmet.
pub(super) fn assertion_unmet(message: impl Into<String>) -> SimError {
    report(ASSERTION_UNMET, message)
}

// = Extern dispatch and arguments

const EXTERN_ARGUMENT_ARITY_MISMATCH: &str = "sim/extern-argument-arity-mismatch";

/// Reports an extern argument arity that is mismatch.
pub(super) fn extern_argument_arity_mismatch(message: impl Into<String>) -> SimError {
    report(EXTERN_ARGUMENT_ARITY_MISMATCH, message)
}

const EXTERN_FUNCTION_UNSUPPORTED: &str = "sim/extern-function-unsupported";

/// Reports an extern function that is unsupported.
pub(super) fn extern_function_unsupported(message: impl Into<String>) -> SimError {
    report(EXTERN_FUNCTION_UNSUPPORTED, message)
}

const EXTERN_METHOD_UNSUPPORTED: &str = "sim/extern-method-unsupported";

/// Reports an extern method that is unsupported.
pub(super) fn extern_method_unsupported(message: impl Into<String>) -> SimError {
    report(EXTERN_METHOD_UNSUPPORTED, message)
}

const EXTERN_OBJECT_UNDEFINED: &str = "sim/extern-object-undefined";

/// Reports an extern object that is undefined.
pub(super) fn extern_object_undefined(message: impl Into<String>) -> SimError {
    report(EXTERN_OBJECT_UNDEFINED, message)
}

const EXTERN_RELATION_UNSUPPORTED: &str = "sim/extern-relation-unsupported";

/// Reports an extern relation that is unsupported.
pub(super) fn extern_relation_unsupported(message: impl Into<String>) -> SimError {
    report(EXTERN_RELATION_UNSUPPORTED, message)
}

const FUNCTION_ARGUMENT_UNDEFINED: &str = "sim/function-argument-undefined";

/// Reports a function argument that is undefined.
pub(super) fn function_argument_undefined(message: impl Into<String>) -> SimError {
    report(FUNCTION_ARGUMENT_UNDEFINED, message)
}

// = Packet input and sizes

const PACKET_CURSOR_INVALID: &str = "sim/packet-cursor-invalid";

/// Reports a packet cursor that is invalid.
pub(super) fn packet_cursor_invalid(message: impl Into<String>) -> SimError {
    report(PACKET_CURSOR_INVALID, message)
}

const PACKET_DIGIT_INVALID: &str = "sim/packet-digit-invalid";

/// Reports a packet digit that is invalid.
pub(super) fn packet_digit_invalid(message: impl Into<String>) -> SimError {
    report(PACKET_DIGIT_INVALID, message)
}

const PACKET_SIZE_INVALID: &str = "sim/packet-size-invalid";

/// Reports a packet size that is invalid.
pub(super) fn packet_size_invalid(message: impl Into<String>) -> SimError {
    report(PACKET_SIZE_INVALID, message)
}

const PACKET_SIZE_OUT_OF_BOUNDS: &str = "sim/packet-size-out-of-bounds";

/// Reports a packet size out of that is bounds.
pub(super) fn packet_size_out_of_bounds(message: impl Into<String>) -> SimError {
    report(PACKET_SIZE_OUT_OF_BOUNDS, message)
}

// = Hashing

const HASH_ALGORITHM_INVALID: &str = "sim/hash-algorithm-invalid";

/// Reports a hash algorithm that is invalid.
pub(super) fn hash_algorithm_invalid(message: impl Into<String>) -> SimError {
    report(HASH_ALGORITHM_INVALID, message)
}

const HASH_ALGORITHM_UNSUPPORTED: &str = "sim/hash-algorithm-unsupported";

/// Reports a hash algorithm that is unsupported.
pub(super) fn hash_algorithm_unsupported(message: impl Into<String>) -> SimError {
    report(HASH_ALGORITHM_UNSUPPORTED, message)
}

const HASH_RANGE_INVALID: &str = "sim/hash-range-invalid";

/// Reports a hash range that is invalid.
pub(super) fn hash_range_invalid(message: impl Into<String>) -> SimError {
    report(HASH_RANGE_INVALID, message)
}

const HASH_WIDTH_INVALID: &str = "sim/hash-width-invalid";

/// Reports a hash width that is invalid.
pub(super) fn hash_width_invalid(message: impl Into<String>) -> SimError {
    report(HASH_WIDTH_INVALID, message)
}

// = Counters, meters, and registers

const CLONE_DIRECTION_INVALID: &str = "sim/clone-direction-invalid";

/// Reports a clone direction that is invalid.
pub(super) fn clone_direction_invalid(message: impl Into<String>) -> SimError {
    report(CLONE_DIRECTION_INVALID, message)
}

const COUNTER_TYPE_INVALID: &str = "sim/counter-type-invalid";

/// Reports a counter type that is invalid.
pub(super) fn counter_type_invalid(message: impl Into<String>) -> SimError {
    report(COUNTER_TYPE_INVALID, message)
}

const COUNTER_TYPE_UNSUPPORTED: &str = "sim/counter-type-unsupported";

/// Reports a counter type that is unsupported.
pub(super) fn counter_type_unsupported(message: impl Into<String>) -> SimError {
    report(COUNTER_TYPE_UNSUPPORTED, message)
}

const COUNTER_VALUE_OUT_OF_BOUNDS: &str = "sim/counter-value-out-of-bounds";

/// Reports a counter value out of that is bounds.
pub(super) fn counter_value_out_of_bounds(message: impl Into<String>) -> SimError {
    report(COUNTER_VALUE_OUT_OF_BOUNDS, message)
}

const METER_TYPE_INVALID: &str = "sim/meter-type-invalid";

/// Reports a meter type that is invalid.
pub(super) fn meter_type_invalid(message: impl Into<String>) -> SimError {
    report(METER_TYPE_INVALID, message)
}

const REGISTER_TYPE_ARGUMENT_ARITY_MISMATCH: &str = "sim/register-type-argument-arity-mismatch";

/// Reports a register type argument arity that is mismatch.
pub(super) fn register_type_argument_arity_mismatch(message: impl Into<String>) -> SimError {
    report(REGISTER_TYPE_ARGUMENT_ARITY_MISMATCH, message)
}

// = Values, formatting, and lookups

const PROGRAM_OUTPUT_FAILED: &str = "sim/program-output-failed";

/// Reports a failure to write a program log message.
pub(super) fn program_output_failed(message: impl Into<String>) -> SimError {
    report(PROGRAM_OUTPUT_FAILED, message)
}

const FORMAT_ARGUMENT_ARITY_MISMATCH: &str = "sim/format-argument-arity-mismatch";

/// Reports a format argument arity that is mismatch.
pub(super) fn format_argument_arity_mismatch(message: impl Into<String>) -> SimError {
    report(FORMAT_ARGUMENT_ARITY_MISMATCH, message)
}

const INTEGER_INVALID: &str = "sim/integer-invalid";

/// Reports an integer that is invalid.
pub(super) fn integer_invalid(message: impl Into<String>) -> SimError {
    report(INTEGER_INVALID, message)
}

const OBJECT_STATE_UNDEFINED: &str = "sim/object-state-undefined";

/// Reports an object state that is undefined.
pub(super) fn object_state_undefined(message: impl Into<String>) -> SimError {
    report(OBJECT_STATE_UNDEFINED, message)
}

const TABLE_ENTRY_INVALID: &str = "sim/table-entry-invalid";

/// Reports a table entry that is invalid.
pub(super) fn table_entry_invalid(message: impl Into<String>) -> SimError {
    report(TABLE_ENTRY_INVALID, message)
}

const TABLE_UNDEFINED: &str = "sim/table-undefined";

/// Reports a table that is undefined.
pub(super) fn table_undefined(message: impl Into<String>) -> SimError {
    report(TABLE_UNDEFINED, message)
}

const TYPE_UNDEFINED: &str = "sim/type-undefined";

/// Reports a type that is undefined.
pub(super) fn type_undefined(message: impl Into<String>) -> SimError {
    report(TYPE_UNDEFINED, message)
}

const VALUE_INVALID: &str = "sim/value-invalid";

/// Reports a value that is invalid.
pub(super) fn value_invalid(message: impl Into<String>) -> SimError {
    report(VALUE_INVALID, message)
}

// = STF execution

const PACKET_MISMATCH: &str = "sim/packet-mismatch";
const STATEMENT_UNSUPPORTED: &str = "sim/statement-unsupported";
const PACKET_EXPECTATION_INCOMPLETE: &str = "sim/packet-expectation-incomplete";

/// Reports an output packet that differs from its expectation.
pub(super) fn packet_mismatch(expect: &Tx, tx: &Tx) -> SimError {
    report(PACKET_MISMATCH, format!("expected {expect} but got {tx}"))
}

/// Reports an STF statement the simulator cannot execute.
pub(super) fn statement_unsupported(stmt: String) -> SimError {
    report(STATEMENT_UNSUPPORTED, format!("not yet supported: {stmt}"))
}

/// Reports unmatched outputs and expectations when an STF test ends.
pub(super) fn packet_expectation_incomplete(txs: &[Tx], expects: &[Expectation]) -> SimError {
    report(
        PACKET_EXPECTATION_INCOMPLETE,
        format!("{}{}", remaining_outputs(txs), remaining_expects(expects)),
    )
}

/// Adds the STF execution context without changing the underlying failure.
pub(super) fn statement_execution_failure(span: &Span, report: Box<Report>) -> SimError {
    if *span == Span::default() {
        report
    } else {
        Box::new(Report::frame(*span, "while executing STF statement", vec![*report]))
    }
}

/// Lists unmatched outputs, or nothing.
fn remaining_outputs(txs: &[Tx]) -> String {
    // Nothing to report when all outputs matched
    if txs.is_empty() {
        String::new()
    } else {
        format!(
            "[FAIL] Remaining packets to be matched:\n{}",
            txs.iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}

/// Lists unmet expectations, or nothing.
fn remaining_expects(expects: &[Expectation]) -> String {
    // Nothing to report when all expectations were met
    if expects.is_empty() {
        String::new()
    } else {
        format!(
            "[FAIL] Expected packets to be output:\n{}",
            expects
                .iter()
                .map(|expect| expect.tx.to_string())
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}
