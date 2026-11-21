//! Extern arguments by name
//!
//! The specification passes parameter names and argument values as two lists;
//! `assoc` zips them.

use p4spectec::lang::data::{
    arena::Arena,
    value::{ValueError, flat::Value},
};

use p4spectec::runner::ExternError;

use crate::spec::bridge::FromSpec;

// == Arguments

/// Pairs parameter names with argument values; the counts must match.
pub fn assoc(
    arena: &mut Arena,
    value_ids: Value,
    value_args: Value,
) -> Result<Vec<(String, Value)>, ExternError> {
    // Names are texts, arguments any values
    let names = Vec::<String>::from_spec(arena, &value_ids)?;
    let values = Vec::<Value>::from_spec(arena, &value_args)?;
    // One value per name
    if names.len() != values.len() {
        return Err(
            ValueError::CountMismatch { expected: names.len(), actual: values.len() }.into()
        );
    }
    Ok(names.into_iter().zip(values).collect())
}
