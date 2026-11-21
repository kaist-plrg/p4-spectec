//! Helpers for invoking functions in the spec
//!
//! Each wrapper builds the argument values, calls the function by name,
//! and unwraps the result; the names are the specification's.

use p4spectec::lang::{
    common::source::Span,
    data::{
        arena::Arena,
        value::flat::{self as value, Value},
    },
};

use p4spectec::runner::{ExternError, InterpreterError};

use crate::spec::{
    bridge::{FromSpec, ToSpec},
    call::rel::Cursor,
    host::SpecHost,
};

use crate::error;

// == Names

/// An unqualified `prefixedNameIR`, `_BARE nameIR`.
pub struct BareName<'a>(pub &'a str);

impl ToSpec for BareName<'_> {
    fn to_spec(&self, arena: &mut Arena) -> Result<Value, ExternError> {
        let value_name = self.0.to_spec(arena)?;
        Ok(value::make::case_string! {
            arena: arena,
            shape: "_BARE nameIR",
            args: vec![value_name],
            typ: "prefixedNameIR",
            span: Span::default(),
        }?)
    }
}

// == Variables

/// Looks a variable's value up at a cursor with `find_var_value_t`.
pub fn find_var_value_t(
    host: &mut dyn SpecHost,
    value_cursor: &Value,
    value_ctx: &Value,
    name: &str,
) -> Result<Value, InterpreterError> {
    let value_name = BareName(name).to_spec(host.arena_mut())?;
    host.call_func("find_var_value_t", &[], &[value_name, *value_cursor, *value_ctx])
        .result
}

/// Looks a local variable's value up.
pub fn find_var_value_t_local(
    host: &mut dyn SpecHost,
    value_ctx: &Value,
    name: &str,
) -> Result<Value, InterpreterError> {
    // Same lookup at the `LOCAL` cursor
    let value_cursor = Cursor::Local.to_spec(host.arena_mut())?;
    find_var_value_t(host, &value_cursor, value_ctx, name)
}

/// Looks a local variable up with `find_var_e`.
pub fn find_var_e_local(
    host: &mut dyn SpecHost,
    value_ctx: Value,
    name: &str,
) -> Result<Value, InterpreterError> {
    let value_cursor = Cursor::Local.to_spec(host.arena_mut())?;
    let value_name = BareName(name).to_spec(host.arena_mut())?;
    host.call_func("find_var_e", &[], &[value_name, value_cursor, value_ctx])
        .result
}

// == Types

/// Looks a local type up with `find_type_e`; a missing type is an error.
pub fn find_type_e_local(
    host: &mut dyn SpecHost,
    value_ctx: Value,
    name: &str,
) -> Result<Value, InterpreterError> {
    let value_cursor = Cursor::Local.to_spec(host.arena_mut())?;
    let value_name = name.to_spec(host.arena_mut())?;
    let value_opt = host
        .call_func("find_type_e", &[], &[value_cursor, value_ctx, value_name])
        .result?;
    Option::<Value>::from_spec(host.arena_mut(), &value_opt)?
        .ok_or_else(|| error::type_undefined(format!("type not found: {name}")).into())
}

/// Substitutes local type arguments into a type with `subst_type_e`.
pub fn subst_type_e_local(
    host: &mut dyn SpecHost,
    value_ctx: Value,
    value_typ: Value,
) -> Result<Value, InterpreterError> {
    let value_cursor = Cursor::Local.to_spec(host.arena_mut())?;
    host.call_func("subst_type_e", &[], &[value_cursor, value_ctx, value_typ])
        .result
}

/// The default value of a type.
pub fn default(host: &mut dyn SpecHost, value_typ: Value) -> Result<Value, InterpreterError> {
    host.call_func("default", &[], &[value_typ]).result
}

/// The minimum size of a type in bits.
pub fn sizeof_min_size_in_bits(
    host: &mut dyn SpecHost,
    value_typ: Value,
) -> Result<num_bigint::BigInt, InterpreterError> {
    let value_size = host
        .call_func("sizeof_minSizeInBits'", &[], &[value_typ])
        .result?;
    Ok(num_bigint::BigInt::from_spec(host.arena_mut(), &value_size)?)
}

/// The maximum size of a type in bits.
pub fn sizeof_max_size_in_bits(
    host: &mut dyn SpecHost,
    value_typ: Value,
) -> Result<num_bigint::BigInt, InterpreterError> {
    let value_size = host
        .call_func("sizeof_maxSizeInBits'", &[], &[value_typ])
        .result?;
    Ok(num_bigint::BigInt::from_spec(host.arena_mut(), &value_size)?)
}

/// Casts a value to a type.
pub fn cast_op(
    host: &mut dyn SpecHost,
    value_typ: Value,
    value: Value,
) -> Result<Value, InterpreterError> {
    host.call_func("cast_op", &[], &[value_typ, value]).result
}

// == Bits

/// Serializes a value to its bits.
pub fn write_bits_from_value(
    host: &mut dyn SpecHost,
    value_source: Value,
) -> Result<Value, InterpreterError> {
    host.call_func("write_bits_from_value", &[], &[value_source])
        .result
}

/// Fills a value from bits, sizing its variable field to `size_varsize`.
pub fn write_value_from_bits(
    host: &mut dyn SpecHost,
    value_target: Value,
    size_varsize: usize,
    bits: &[bool],
) -> Result<Value, InterpreterError> {
    let value_varsize = size_varsize.to_spec(host.arena_mut())?;
    let value_bits = bits.to_spec(host.arena_mut())?;
    host.call_func("write_value_from_bits", &[], &[value_target, value_varsize, value_bits])
        .result
}

// == Tables

/// The keys of a table: name, match kind, and type each.
pub fn key_interface_of_table_object(
    host: &mut dyn SpecHost,
    value_table: Value,
) -> Result<Vec<(Value, Value, Value)>, InterpreterError> {
    // Each key is a (name, match kind, type) triple
    let value_keys = host
        .call_func("key_interface_of_tableObject", &[], &[value_table])
        .result?;
    Ok(Vec::<(Value, Value, Value)>::from_spec(host.arena_mut(), &value_keys)?)
}

// == Objects

// - Lookup

/// Finds a table object by qualified name.
pub fn find_table_qualified_e(
    host: &mut dyn SpecHost,
    value_arch: Value,
    value_id: Value,
) -> Result<Option<Value>, InterpreterError> {
    let value_opt = host
        .call_func("find_table_qualified_e", &[], &[value_arch, value_id])
        .result?;
    Ok(Option::<Value>::from_spec(host.arena_mut(), &value_opt)?)
}

/// Finds a table object by bare name.
pub fn find_table_unqualified_e(
    host: &mut dyn SpecHost,
    value_arch: Value,
    value_id: Value,
) -> Result<Option<Value>, InterpreterError> {
    let value_opt = host
        .call_func("find_table_unqualified_e", &[], &[value_arch, value_id])
        .result?;
    Ok(Option::<Value>::from_spec(host.arena_mut(), &value_opt)?)
}

// - Update

/// Replaces an object found by qualified name.
pub fn update_object_qualified_e(
    host: &mut dyn SpecHost,
    value_arch: Value,
    value_id: Value,
    value_object: Value,
) -> Result<Value, InterpreterError> {
    host.call_func("update_object_qualified_e", &[], &[value_arch, value_id, value_object])
        .result
}

/// Replaces a table object found by bare name.
pub fn update_table_unqualified_e(
    host: &mut dyn SpecHost,
    value_arch: Value,
    value_id: Value,
    value_object: Value,
) -> Result<Value, InterpreterError> {
    host.call_func("update_table_unqualified_e", &[], &[value_arch, value_id, value_object])
        .result
}

// == Object state

/// The state of an extern object; missing state is an error.
pub fn find_object_state_e(
    host: &mut dyn SpecHost,
    value_arch: Value,
    value_id: Value,
) -> Result<Value, InterpreterError> {
    let value_opt = host
        .call_func("find_objectState_e", &[], &[value_arch, value_id])
        .result?;
    Option::<Value>::from_spec(host.arena_mut(), &value_opt)?
        .ok_or_else(|| error::object_state_undefined("object state not found".to_owned()).into())
}

/// Replaces the state of an extern object; missing state is an error.
pub fn update_object_state_e(
    host: &mut dyn SpecHost,
    value_arch: Value,
    value_id: Value,
    value_state: Value,
) -> Result<Value, InterpreterError> {
    let value_opt = host
        .call_func("update_objectState_e", &[], &[value_arch, value_id, value_state])
        .result?;
    Option::<Value>::from_spec(host.arena_mut(), &value_opt)?
        .ok_or_else(|| error::object_state_undefined("object state not found".to_owned()).into())
}

// == Architecture state

/// The architecture state.
pub fn find_arch_state_e(
    host: &mut dyn SpecHost,
    value_arch: Value,
) -> Result<Value, InterpreterError> {
    host.call_func("find_archState_e", &[], &[value_arch])
        .result
}

/// Replaces the architecture state.
pub fn update_arch_state_e(
    host: &mut dyn SpecHost,
    value_arch: Value,
    value_state: Value,
) -> Result<Value, InterpreterError> {
    host.call_func("update_archState_e", &[], &[value_arch, value_state])
        .result
}
