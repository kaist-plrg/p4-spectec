//! Unpacks Rust values from IL values representing P4 values
//!
//! Each projection matches the specification's `value` case
//! for one P4 value form
//! and fails with an extern error otherwise.

use num_bigint::BigInt;

use crate::{
    lang::{
        common::prim::num,
        data::value::{Value, ValueArena, ValueError, get},
    },
    runner::ExternError,
};

// == P4 values

/// The boolean in a `_B bool` value.
pub fn p4_bool(arena: &ValueArena, value: &Value) -> Result<bool, ExternError> {
    get::matches! { arena,
        value,
        "_B bool" => |values| {
            // Exactly one argument
            let [value] = values.as_slice() else {
                return Err(ValueError::ExpectedCount {
                    expected: 1,
                    actual: values.len(),
                }.into());
            };
            get::bool(arena, value).map_err(ExternError::from)
        },
        _ => Err(crate::sim_plugin::error::value_invalid("expected P4 bool value".to_owned())),
    }
}

/// The text in a `"text"` value.
pub fn p4_string(arena: &ValueArena, value: &Value) -> Result<String, ExternError> {
    get::matches! { arena,
        value,
        "'\"' text '\"'" => |values| {
            // Exactly one argument
            let [value] = values.as_slice() else {
                return Err(ValueError::ExpectedCount {
                    expected: 1,
                    actual: values.len(),
                }.into());
            };
            get::text(arena, value).map(str::to_owned).map_err(ExternError::from)
        },
        _ => Err(crate::sim_plugin::error::value_invalid("expected P4 string value".to_owned())),
    }
}

/// The type and member names of a `tid . id` value.
pub fn p4_enum(arena: &ValueArena, value: &Value) -> Result<(String, String), ExternError> {
    get::matches! { arena, value,
        "tid '.' id" => |values| {
            // Type name, then member
            let [value_enum, value_id] = values.as_slice() else {
                return Err(ValueError::ExpectedCount { expected: 2, actual: values.len() }.into());
            };
            Ok((get::text(arena, value_enum)?.to_owned(), get::text(arena, value_id)?.to_owned()))
        },
        _ => Err(crate::sim_plugin::error::value_invalid("expected P4 enum value".to_owned())),
    }
}

/// The components of a `TUPLE (...)` value.
pub fn p4_tuple(arena: &ValueArena, value: &Value) -> Result<Vec<Value>, ExternError> {
    get::matches! { arena, value,
        "TUPLE `( value* `)" => |values| {
            // One list of components
            let [value_list] = values.as_slice() else {
                return Err(ValueError::ExpectedCount { expected: 1, actual: values.len() }.into());
            };
            Ok(get::list(arena, value_list)?.to_vec())
        },
        _ => Err(crate::sim_plugin::error::value_invalid("expected P4 tuple value".to_owned())),
    }
}

// - Numbers

/// Width and value of a `nat W int` bit string.
pub fn p4_fixed_bit(arena: &ValueArena, value: &Value) -> Result<(BigInt, BigInt), ExternError> {
    get::matches! { arena, value,
        "nat W int" => |values| {
            // Width, then value
            let [value_width, value_int] = values.as_slice() else {
                return Err(ValueError::ExpectedCount {
                    expected: 2,
                    actual: values.len(),
                }.into());
            };
            Ok((
                num::to_int(get::num(arena, value_width)?).clone(),
                num::to_int(get::num(arena, value_int)?).clone(),
            ))
        },
        _ => Err(crate::sim_plugin::error::value_invalid("expected P4 fixed-bit value".to_owned())),
    }
}

/// Width and value of any fixed-width number: `W`, `S`, or varbit `V`.
pub fn p4_precision_number(
    arena: &ValueArena,
    value: &Value,
) -> Result<(BigInt, BigInt), ExternError> {
    get::matches! { arena, value,
        "nat W int" | "nat S int" => |values| {
            // Width, then value
            let [value_width, value_int] = values.as_slice() else {
                return Err(ValueError::ExpectedCount { expected: 2, actual: values.len() }.into());
            };
            Ok((
                num::to_int(get::num(arena, value_width)?).clone(),
                num::to_int(get::num(arena, value_int)?).clone(),
            ))
        },
        // A varbit carries its maximum width first; only the actual one matters
        "nat '.' nat V int" => |values| {
            let [value_width_max, value_width, value_int] = values.as_slice() else {
                return Err(ValueError::ExpectedCount { expected: 3, actual: values.len() }.into());
            };
            get::num(arena, value_width_max)?;
            Ok((
                num::to_int(get::num(arena, value_width)?).clone(),
                num::to_int(get::num(arena, value_int)?).clone(),
            ))
        },
        _ => Err(crate::sim_plugin::error::value_invalid("expected P4 precision number value".to_owned())),
    }
}
