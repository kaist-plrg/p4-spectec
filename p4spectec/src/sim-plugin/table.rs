//! Match-action table interface
//!
//! STF names a table qualified, `MyIngress.t`, or bare, `t`;
//! `find_table` tries the qualified object first.
//! Adding an entry goes through the specification's `tableObject_add_entry`,
//! retrying with the table's own key names when the STF key names do not match.

use crate::lang::{
    common::source::Span,
    data::{
        arena::Arena,
        typ,
        value::{
            ValueError,
            flat::{self as value, Value},
        },
    },
};

use crate::runner::{Extern, ExternError, Interface, Interpreter, RunnerContext};

use crate::sim_plugin::error;

use super::spec::func;

// == Table names

/// Splits a dotted name into its last segment and, if dotted, its full path.
fn table_name(arena: &mut Arena, value_name: Value) -> Result<(Value, Option<Value>), ExternError> {
    // The last segment is the bare name
    let name = value::get::text(arena, &value_name)?.to_owned();
    let names: Vec<_> = name.split('.').collect();
    let value_unqualified = value::make::text(
        arena,
        names
            .last()
            .expect("split always has a segment")
            .to_string(),
        Span::default(),
    )?;
    // A bare name has no qualified form
    let value_qualified = if names.len() == 1 {
        None
    } else {
        let values_name = names
            .into_iter()
            .map(|name| value::make::text(arena, name.to_owned(), Span::default()))
            .collect::<Result<Vec<_>, _>>()?;
        let typ_id = typ::make::list(typ::make::var(
            crate::phrase!(node: "nameIR".to_owned(), span: Span::default()),
            Vec::new(),
        ));
        Some(value::make::list(arena, typ_id.node.into(), values_name, Span::default())?)
    };
    Ok((value_unqualified, value_qualified))
}

// == Table lookup and update

/// Finds a table object by qualified name, falling back to the bare name.
pub fn find_table<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_arch: Value,
    value_name: Value,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let (value_unqualified, value_qualified) = table_name(ctx.arena_mut(), value_name)?;
    if let Some(value_id) = value_qualified
        && let Some(value_table) = func::find_object_qualified_e(ctx, value_arch, value_id)?
    {
        return Ok(value_table);
    }
    func::find_object_unqualified_e(ctx, value_arch, value_unqualified)?
        .ok_or_else(|| error::table_undefined("table not found".to_owned()).into())
}

/// Stores a table object back under the name it was found by.
pub fn update_table<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_arch: Value,
    value_name: Value,
    value_table: Value,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let (value_unqualified, value_qualified) = table_name(ctx.arena_mut(), value_name)?;
    if let Some(value_id) = value_qualified
        && func::find_object_qualified_e(ctx, value_arch, value_id)?.is_some()
    {
        return func::update_object_qualified_e(ctx, value_arch, value_id, value_table)
            .map_err(ExternError::from);
    }
    func::update_object_unqualified_e(ctx, value_arch, value_unqualified, value_table)
        .map_err(ExternError::from)
}

// == Table entries

/// Adds an entry to a table and writes the table back into the architecture.
pub fn add_entry<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
    value_name: Value,
    value_priority: Value,
    value_keys: Value,
    value_action: Value,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    // Lookup table object
    let value_table = find_table(ctx, value_arch, value_name)?;
    // Add entry to table object
    let value_table = match func::table_object_add_entry(
        ctx,
        value_ctx,
        value_table,
        value_priority,
        value_keys,
        value_action,
    )? {
        Some(value_table) => value_table,
        None => {
            // Rejected: retry with the table's own key names, in order
            let keys = func::key_interface_of_table_object(ctx, value_table)?;
            let mut values_name = Vec::new();
            for (value_name, value_match_kind, _) in keys {
                // Selector keys take no STF value
                if value::get::text(ctx.arena(), &value_match_kind)? != "selector" {
                    values_name.push(value_name);
                }
            }
            let values_key = value::get::list(ctx.arena(), &value_keys)?
                .iter()
                .map(|value_key| value::get::tuple(ctx.arena(), value_key))
                .collect::<Result<Vec<_>, ValueError>>()?;
            let values_key = values_key
                .into_iter()
                .map(|values| value::get::nth(values, 1).copied())
                .collect::<Result<Vec<_>, ValueError>>()?;
            // Key count must then agree
            if values_name.len() != values_key.len() {
                return Err(ExternError::from(ValueError::CountMismatch {
                    expected: values_name.len(),
                    actual: values_key.len(),
                }));
            }
            let typ_key = typ::make::var(
                crate::phrase!(node: "tableKeyInterface".to_owned(), span: Span::default()),
                Vec::new(),
            );
            let values_key = values_name
                .into_iter()
                .zip(values_key)
                .map(|(value_name, value_key)| {
                    value::make::tuple(
                        ctx.arena_mut(),
                        typ_key.node.clone().into(),
                        vec![value_name, value_key],
                        Span::default(),
                    )
                })
                .collect::<Result<Vec<_>, _>>()?;
            let value_keys = value::make::list(
                ctx.arena_mut(),
                typ::make::list(typ_key).node.into(),
                values_key,
                Span::default(),
            )?;
            // A second rejection is final
            func::table_object_add_entry(
                ctx,
                value_ctx,
                value_table,
                value_priority,
                value_keys,
                value_action,
            )?
            .ok_or_else(|| error::table_entry_invalid("table entry rejected".to_owned()))?
        }
    };
    // Update arch with modified table object
    update_table(ctx, value_arch, value_name, value_table)
}

/// Sets a table's default action and writes the table back.
pub fn add_default_action<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
    value_name: Value,
    value_action: Value,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    // Lookup table object
    let value_table = find_table(ctx, value_arch, value_name)?;
    let value_table =
        func::table_object_add_default_action(ctx, value_ctx, value_table, value_action)?;
    // Update arch with modified table object
    update_table(ctx, value_arch, value_name, value_table)
}
