//! Unpacks Rust values from IL values representing P4 values
//!
//! Each projection matches the specification's `value` case
//! for one P4 value form
//! and fails with an extern error otherwise.

use num_bigint::BigInt;

use crate::lang::{
    common::prim::num,
    data::{
        arena::Arena,
        value::{
            ValueError,
            flat::{self as value, Value},
        },
    },
};

use crate::runner::ExternError;

use crate::sim_plugin::error;

// == P4 values

/// The boolean in a `_B bool` value.
pub fn p4_bool(arena: &Arena, value: &Value) -> Result<bool, ExternError> {
    value::get::matches! { arena,
        value,
        "_B bool" => |values| {
            // Exactly one argument
            let [value] = values else {
                return Err(ValueError::CountMismatch {
                    expected: 1,
                    actual: values.len(),
                }.into());
            };
            value::get::bool(arena, value).map_err(ExternError::from)
        },
        _ => Err(error::value_invalid("expected P4 bool value".to_owned()).into()),
    }
}

/// The text in a `"text"` value.
pub fn p4_string(arena: &Arena, value: &Value) -> Result<String, ExternError> {
    value::get::matches! { arena,
        value,
        "'\"' text '\"'" => |values| {
            // Exactly one argument
            let [value] = values else {
                return Err(ValueError::CountMismatch {
                    expected: 1,
                    actual: values.len(),
                }.into());
            };
            value::get::text(arena, value).map(str::to_owned).map_err(ExternError::from)
        },
        _ => Err(error::value_invalid("expected P4 string value".to_owned()).into()),
    }
}

/// The type and member names of a `tid . id` value.
pub fn p4_enum(arena: &Arena, value: &Value) -> Result<(String, String), ExternError> {
    value::get::matches! { arena, value,
        "tid '.' id" => |values| {
            // Type name, then member
            let [value_enum, value_id] = values else {
                return Err(ValueError::CountMismatch { expected: 2, actual: values.len() }.into());
            };
            Ok((value::get::text(arena, value_enum)?.to_owned(), value::get::text(arena, value_id)?.to_owned()))
        },
        _ => Err(error::value_invalid("expected P4 enum value".to_owned()).into()),
    }
}

/// The components of a `TUPLE (...)` value.
pub fn p4_tuple(arena: &Arena, value: &Value) -> Result<Vec<Value>, ExternError> {
    value::get::matches! { arena, value,
        "TUPLE `( value* `)" => |values| {
            // One list of components
            let [value_list] = values else {
                return Err(ValueError::CountMismatch { expected: 1, actual: values.len() }.into());
            };
            Ok(value::get::list(arena, value_list)?.to_vec())
        },
        _ => Err(error::value_invalid("expected P4 tuple value".to_owned()).into()),
    }
}

// - Numbers

/// Width and value of a `nat W int` bit string.
pub fn p4_fixed_bit(arena: &Arena, value: &Value) -> Result<(BigInt, BigInt), ExternError> {
    value::get::matches! { arena, value,
        "nat W int" => |values| {
            // Width, then value
            let [value_width, value_int] = values else {
                return Err(ValueError::CountMismatch {
                    expected: 2,
                    actual: values.len(),
                }.into());
            };
            Ok((
                num::to_int(value::get::num(arena, value_width)?).clone(),
                num::to_int(value::get::num(arena, value_int)?).clone(),
            ))
        },
        _ => Err(error::value_invalid("expected P4 fixed-bit value".to_owned()).into()),
    }
}

/// Width and value of any fixed-width number: `W`, `S`, or varbit `V`.
pub fn p4_precision_number(arena: &Arena, value: &Value) -> Result<(BigInt, BigInt), ExternError> {
    value::get::matches! { arena, value,
        "nat W int" | "nat S int" => |values| {
            // Width, then value
            let [value_width, value_int] = values else {
                return Err(ValueError::CountMismatch { expected: 2, actual: values.len() }.into());
            };
            Ok((
                num::to_int(value::get::num(arena, value_width)?).clone(),
                num::to_int(value::get::num(arena, value_int)?).clone(),
            ))
        },
        // A varbit carries its maximum width first; only the actual one matters
        "nat '.' nat V int" => |values| {
            let [value_width_max, value_width, value_int] = values else {
                return Err(ValueError::CountMismatch { expected: 3, actual: values.len() }.into());
            };
            value::get::num(arena, value_width_max)?;
            Ok((
                num::to_int(value::get::num(arena, value_width)?).clone(),
                num::to_int(value::get::num(arena, value_int)?).clone(),
            ))
        },
        _ => Err(error::value_invalid("expected P4 precision number value".to_owned()).into()),
    }
}
