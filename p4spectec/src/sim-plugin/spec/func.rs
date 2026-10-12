//! Helpers for invoking functions in the spec
//!
//! Each wrapper builds the argument values, calls the function by name,
//! and unwraps the result; the names are the specification's.

use crate::lang::{
    common::source::Span,
    data::value::flat::{self as value, Value},
};

use crate::runner::{Extern, ExternError, Interface, Interpreter, InterpreterError, RunnerContext};

use crate::sim_plugin::error;

// == Names and cursors

/// The `LOCAL` cursor, selecting the current call's scope.
pub(crate) fn local_cursor(
    arena: &mut crate::lang::data::arena::Arena,
) -> Result<Value, ExternError> {
    Ok(value::make::case_shaped! {
        arena: arena,
        shape: "LOCAL",
        args: Vec::new(),
        typ: "cursor",
        span: Span::default(),
    }?)
}

/// An unqualified `prefixedNameIR`.
pub(crate) fn bare_name(
    arena: &mut crate::lang::data::arena::Arena,
    name: &str,
) -> Result<Value, ExternError> {
    let value_name = value::make::text(arena, name.to_owned(), Span::default())?;
    Ok(value::make::case_shaped! {
        arena: arena,
        shape: "_BARE nameIR",
        args: vec![value_name],
        typ: "prefixedNameIR",
        span: Span::default(),
    }?)
}

// == Variables

/// Looks a variable's value up at a cursor with `find_var_value_t`.
pub fn find_var_value_t<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_cursor: &Value,
    value_ctx: &Value,
    name: &str,
) -> Result<Value, InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    // The name as a bare `prefixedNameIR`
    let value_name = value::make::text(ctx.arena_mut(), name.to_owned(), Span::default())
        .map_err(ExternError::from)?;
    let value_name = value::make::case_shaped! {
        arena: ctx.arena_mut(),
        shape: "_BARE nameIR",
        args: vec![value_name],
        typ: "prefixedNameIR",
        span: Span::default(),
    }
    .map_err(ExternError::from)?;
    ctx.call_func("find_var_value_t", &[], &[value_name, *value_cursor, *value_ctx])
}

/// Looks a local variable's value up.
pub fn find_var_value_t_local<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: &Value,
    name: &str,
) -> Result<Value, InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    // Same lookup at the `LOCAL` cursor
    let value_cursor = value::make::case_shaped! {
        arena: ctx.arena_mut(),
        shape: "LOCAL",
        args: Vec::new(),
        typ: "cursor",
        span: Span::default(),
    }
    .map_err(ExternError::from)?;
    find_var_value_t(ctx, &value_cursor, value_ctx, name)
}

/// Looks a local variable up with `find_var_e`.
pub fn find_var_e_local<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    name: &str,
) -> Result<Value, InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let value_cursor = local_cursor(ctx.arena_mut())?;
    let value_name = bare_name(ctx.arena_mut(), name)?;
    ctx.call_func("find_var_e", &[], &[value_name, value_cursor, value_ctx])
}

// == Types

/// Looks a local type up with `find_type_e`; a missing type is an error.
pub fn find_type_e_local<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    name: &str,
) -> Result<Value, InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let value_cursor = local_cursor(ctx.arena_mut())?;
    let value_name = value::make::text(ctx.arena_mut(), name.to_owned(), Span::default())
        .map_err(ExternError::from)?;
    let value_opt = ctx.call_func("find_type_e", &[], &[value_cursor, value_ctx, value_name])?;
    value::get::opt(ctx.arena(), &value_opt)
        .map_err(ExternError::from)?
        .ok_or_else(|| error::type_undefined(format!("type not found: {name}")).into())
}

/// Substitutes local type arguments into a type with `subst_type_e`.
pub fn subst_type_e_local<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_typ: Value,
) -> Result<Value, InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let value_cursor = local_cursor(ctx.arena_mut())?;
    ctx.call_func("subst_type_e", &[], &[value_cursor, value_ctx, value_typ])
}

/// The default value of a type.
pub fn default<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_typ: Value,
) -> Result<Value, InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    ctx.call_func("default", &[], &[value_typ])
}

/// The minimum size of a type in bits.
pub fn sizeof_min_size_in_bits<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_typ: Value,
) -> Result<num_bigint::BigInt, InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let value_size = ctx.call_func("sizeof_minSizeInBits'", &[], &[value_typ])?;
    Ok(crate::lang::common::prim::num::to_int(
        value::get::num(ctx.arena(), &value_size).map_err(ExternError::from)?,
    )
    .clone())
}

/// The maximum size of a type in bits.
pub fn sizeof_max_size_in_bits<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_typ: Value,
) -> Result<num_bigint::BigInt, InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let value_size = ctx.call_func("sizeof_maxSizeInBits'", &[], &[value_typ])?;
    Ok(crate::lang::common::prim::num::to_int(
        value::get::num(ctx.arena(), &value_size).map_err(ExternError::from)?,
    )
    .clone())
}

/// Casts a value to a type.
pub fn cast_op<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_typ: Value,
    value: Value,
) -> Result<Value, InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    ctx.call_func("cast_op", &[], &[value_typ, value])
}

// == Bits

/// Serializes a value to its bits.
pub fn write_bits_from_value<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_source: Value,
) -> Result<Value, InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    ctx.call_func("write_bits_from_value", &[], &[value_source])
}

/// Fills a value from bits, sizing its variable field to `size_varsize`.
pub fn write_value_from_bits<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_target: Value,
    size_varsize: usize,
    bits: &[bool],
) -> Result<Value, InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    // The variable field size must be a natural
    let value_varsize = value::make::nat(
        ctx.arena_mut(),
        crate::lang::common::prim::num::Natural::try_from(num_bigint::BigInt::from(size_varsize))
            .expect("packet size is nonnegative"),
        Span::default(),
    )
    .map_err(ExternError::from)?;
    // Bits travel as a `bit*` list of booleans
    let values_bits = bits
        .iter()
        .map(|bit| value::make::bool(ctx.arena_mut(), *bit, Span::default()))
        .collect::<Result<Vec<_>, _>>()
        .map_err(ExternError::from)?;
    let typ_bits = crate::lang::data::typ::make::list(crate::lang::data::typ::make::var(
        crate::phrase!(node: "bit".to_owned(), span: Span::default()),
        Vec::new(),
    ));
    let value_bits =
        value::make::list(ctx.arena_mut(), typ_bits.node.into(), values_bits, Span::default())
            .map_err(ExternError::from)?;
    ctx.call_func("write_value_from_bits", &[], &[value_target, value_varsize, value_bits])
}

/// Extracts the bit range `hi..lo` of a value.
pub fn bitacc_range_op<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_base: Value,
    value_hi: Value,
    value_lo: Value,
) -> Result<Value, InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    ctx.call_func("bitacc_range_op", &[], &[value_base, value_hi, value_lo])
}

// == Tables

/// The keys of a table: name, match kind, and type each.
pub fn key_interface_of_table_object<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_table: Value,
) -> Result<Vec<(Value, Value, Value)>, InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    // Each key is a (name, match kind, type) triple
    let value_keys = ctx.call_func("key_interface_of_tableObject", &[], &[value_table])?;
    value::get::list(ctx.arena(), &value_keys)
        .map_err(ExternError::from)?
        .iter()
        .map(|value_key| {
            let values = value::get::tuple(ctx.arena(), value_key).map_err(ExternError::from)?;
            let (value_name, value_match_kind, value_typ) =
                value::get::three(values).map_err(ExternError::from)?;
            Ok((*value_name, *value_match_kind, *value_typ))
        })
        .collect()
}

/// Adds an entry to a table object; `None` when the entry was rejected.
pub fn table_object_add_entry<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_table: Value,
    value_priority: Value,
    value_keys: Value,
    value_action: Value,
) -> Result<Option<Value>, InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let value_opt = ctx.call_func(
        "tableObject_add_entry",
        &[],
        &[value_ctx, value_table, value_priority, value_keys, value_action],
    )?;
    Ok(value::get::opt(ctx.arena(), &value_opt).map_err(ExternError::from)?)
}

/// Sets a table object's default action.
pub fn table_object_add_default_action<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_table: Value,
    value_action: Value,
) -> Result<Value, InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    ctx.call_func("tableObject_add_default_action", &[], &[value_ctx, value_table, value_action])
}

// == Objects

// - Lookup

/// Finds an object by qualified name.
pub fn find_object_qualified_e<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_arch: Value,
    value_id: Value,
) -> Result<Option<Value>, InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let value_opt = ctx.call_func("find_object_qualified_e", &[], &[value_arch, value_id])?;
    Ok(value::get::opt(ctx.arena(), &value_opt).map_err(ExternError::from)?)
}

/// Finds an object by bare name.
pub fn find_object_unqualified_e<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_arch: Value,
    value_id: Value,
) -> Result<Option<Value>, InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let value_opt = ctx.call_func("find_object_unqualified_e", &[], &[value_arch, value_id])?;
    Ok(value::get::opt(ctx.arena(), &value_opt).map_err(ExternError::from)?)
}

// - Update

/// Replaces an object found by qualified name.
pub fn update_object_qualified_e<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_arch: Value,
    value_id: Value,
    value_object: Value,
) -> Result<Value, InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    ctx.call_func("update_object_qualified_e", &[], &[value_arch, value_id, value_object])
}

/// Replaces an object found by bare name.
pub fn update_object_unqualified_e<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_arch: Value,
    value_id: Value,
    value_object: Value,
) -> Result<Value, InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    ctx.call_func("update_object_unqualified_e", &[], &[value_arch, value_id, value_object])
}

// == Object state

/// The state of an extern object; missing state is an error.
pub fn find_object_state_e<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_arch: Value,
    value_id: Value,
) -> Result<Value, InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let value_opt = ctx.call_func("find_objectState_e", &[], &[value_arch, value_id])?;
    value::get::opt(ctx.arena(), &value_opt)
        .map_err(ExternError::from)?
        .ok_or_else(|| error::object_state_undefined("object state not found".to_owned()).into())
}

/// Replaces the state of an extern object; missing state is an error.
pub fn update_object_state_e<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_arch: Value,
    value_id: Value,
    value_state: Value,
) -> Result<Value, InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let value_opt =
        ctx.call_func("update_objectState_e", &[], &[value_arch, value_id, value_state])?;
    value::get::opt(ctx.arena(), &value_opt)
        .map_err(ExternError::from)?
        .ok_or_else(|| error::object_state_undefined("object state not found".to_owned()).into())
}

// == Architecture state

/// The architecture state.
pub fn find_arch_state_e<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_arch: Value,
) -> Result<Value, InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    ctx.call_func("find_archState_e", &[], &[value_arch])
}

/// Replaces the architecture state.
pub fn update_arch_state_e<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_arch: Value,
    value_state: Value,
) -> Result<Value, InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    ctx.call_func("update_archState_e", &[], &[value_arch, value_state])
}
