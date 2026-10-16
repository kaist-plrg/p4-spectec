//! Extern arguments by name
//!
//! The specification passes parameter names and argument values as two lists;
//! `assoc` zips them and `find` looks one up.

use crate::lang::data::{
    arena::Arena,
    value::{
        ValueError,
        flat::{self as value, Value},
    },
};

use crate::runner::ExternError;

use crate::sim_plugin::error;

// == Arguments

/// Pairs parameter names with argument values; the counts must match.
pub fn assoc(
    arena: &Arena,
    value_ids: Value,
    value_args: Value,
) -> Result<Vec<(String, Value)>, ExternError> {
    // Names are texts, arguments any values
    let names = value::get::list(arena, &value_ids)?
        .iter()
        .map(|value_id| value::get::text(arena, value_id).map(str::to_owned))
        .collect::<Result<Vec<_>, _>>()?;
    let values = value::get::list(arena, &value_args)?;
    // One value per name
    if names.len() != values.len() {
        return Err(
            ValueError::CountMismatch { expected: names.len(), actual: values.len() }.into()
        );
    }
    Ok(names.into_iter().zip(values.iter().copied()).collect())
}

/// Finds the first argument with the requested name.
pub fn find(args: &[(String, Value)], name: &str) -> Result<Value, ExternError> {
    args.iter()
        .find(|(name_arg, _)| name_arg == name)
        .map(|(_, value)| *value)
        .ok_or_else(|| {
            error::function_argument_undefined(format!("argument not found: {name}")).into()
        })
}
