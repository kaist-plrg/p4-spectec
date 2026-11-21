//! Match-action tables in the specification's state
//!
//! A control plane names a table qualified, `MyIngress.t`, or bare, `t`;
//! `find_table` tries the qualified object first.
//! Lookups consider only table objects,
//! since a table may share its control-plane name with an extern.
//! `add_entry` and `set_default_action` encode a `p4::TableEntry`
//! or `p4::ActionCall` as the specification's table interface values with `ToSpec`
//! and apply `TableObject_add_entry` or `TableObject_add_default_action`;
//! `get_keys` reads a table's key names and match kinds.

use num_bigint::BigInt;

use p4spectec::lang::{
    common::source::Span,
    data::{
        arena::Arena,
        typ,
        value::flat::{self as value, Value},
    },
};

use p4spectec::runner::ExternError;

use crate::spec::{
    SpecValue,
    bridge::{FromSpec, ToSpec},
    host::SpecHost,
};

use crate::error;

use crate::p4::{
    ActionCall, Architecture, EntryStatus, MatchKey, MatchKeyParam, Radix, Sim, TableEntry,
    TableKey,
};

use crate::spec::call::{func, rel};

// == Table names

/// Splits a dotted name into its last segment and, if dotted, its full path.
fn table_name(arena: &mut Arena, value_name: Value) -> Result<(Value, Option<Value>), ExternError> {
    // The last segment is the bare name
    let name = String::from_spec(arena, &value_name)?;
    let names: Vec<_> = name.split('.').collect();
    let value_unqualified = names
        .last()
        .expect("split always has a segment")
        .to_spec(arena)?;
    // A bare name has no qualified form
    let value_qualified = if names.len() == 1 {
        None
    } else {
        let values_name = names
            .into_iter()
            .map(|name| name.to_spec(arena))
            .collect::<Result<Vec<_>, _>>()?;
        let typ_id = typ::make::list(typ_var("nameIR"));
        Some(value::make::list(arena, typ_id.node.into(), values_name, Span::default())?)
    };
    Ok((value_unqualified, value_qualified))
}

// == Table lookup and update

/// Finds a table object by qualified name, falling back to the bare name.
fn find_table(
    host: &mut dyn SpecHost,
    value_arch: Value,
    value_name: Value,
) -> Result<Value, ExternError> {
    let (value_unqualified, value_qualified) = table_name(host.arena_mut(), value_name)?;
    if let Some(value_id) = value_qualified
        && let Some(value_table) = func::find_table_qualified_e(host, value_arch, value_id)?
    {
        return Ok(value_table);
    }
    func::find_table_unqualified_e(host, value_arch, value_unqualified)?
        .ok_or_else(|| error::table_undefined("table not found".to_owned()).into())
}

/// Stores a table object back under the name it was found by.
fn update_table(
    host: &mut dyn SpecHost,
    value_arch: Value,
    value_name: Value,
    value_table: Value,
) -> Result<Value, ExternError> {
    let (value_unqualified, value_qualified) = table_name(host.arena_mut(), value_name)?;
    if let Some(value_id) = value_qualified
        && func::find_table_qualified_e(host, value_arch, value_id)?.is_some()
    {
        return func::update_object_qualified_e(host, value_arch, value_id, value_table)
            .map_err(ExternError::from);
    }
    func::update_table_unqualified_e(host, value_arch, value_unqualified, value_table)
        .map_err(ExternError::from)
}

// == Table entries

impl<Arch: Architecture> Sim<'_, Arch> {
    /// Adds `entry` to its table; whether the table took it.
    pub fn add_entry(&mut self, entry: &TableEntry) -> Result<EntryStatus, ExternError> {
        let host: &mut dyn SpecHost = &mut *self.host;
        let (value_ctx, value_arch) = (self.state.value_ctx.0, self.state.value_arch.0);
        let value_name = entry.table.to_spec(host.arena_mut())?;
        let value_table = find_table(host, value_arch, value_name)?;
        // Priority is optional
        let value_priority = entry
            .priority
            .map(|priority| BigInt::from(priority).to_spec(host.arena_mut()))
            .transpose()?;
        let value_priority = value::make::opt(
            host.arena_mut(),
            typ::make::opt(typ::make::int()).node.into(),
            value_priority,
            Span::default(),
        )?;
        let value_keys = entry.keys.to_spec(host.arena_mut())?;
        let value_action = entry.action.to_spec(host.arena_mut())?;
        // The relation returns the updated table, or nothing on rejection
        match rel::table_object_add_entry(
            host,
            value_ctx,
            value_table,
            value_priority,
            value_keys,
            value_action,
        )? {
            Some(value_table) => {
                let value_arch = update_table(host, value_arch, value_name, value_table)?;
                self.state.value_arch = SpecValue(value_arch);
                Ok(EntryStatus::Added)
            }
            None => Ok(EntryStatus::Rejected),
        }
    }

    /// Sets the action `table` runs on a miss.
    pub fn set_default_action(
        &mut self,
        table: &str,
        action: &ActionCall,
    ) -> Result<(), ExternError> {
        let host: &mut dyn SpecHost = &mut *self.host;
        let (value_ctx, value_arch) = (self.state.value_ctx.0, self.state.value_arch.0);
        let value_name = table.to_spec(host.arena_mut())?;
        let value_table = find_table(host, value_arch, value_name)?;
        let value_action = action.to_spec(host.arena_mut())?;
        let value_table =
            rel::table_object_add_default_action(host, value_ctx, value_table, value_action)?;
        let value_arch = update_table(host, value_arch, value_name, value_table)?;
        self.state.value_arch = SpecValue(value_arch);
        Ok(())
    }

    /// The keys of `table` in order: name and match kind.
    pub fn get_keys(&mut self, table: &str) -> Result<Vec<TableKey>, ExternError> {
        let host: &mut dyn SpecHost = &mut *self.host;
        let value_name = table.to_spec(host.arena_mut())?;
        let value_table = find_table(host, self.state.value_arch.0, value_name)?;
        func::key_interface_of_table_object(host, value_table)?
            .into_iter()
            .map(|(value_name, value_match_kind, _)| {
                Ok(TableKey {
                    name: String::from_spec(host.arena_mut(), &value_name)?,
                    match_kind: String::from_spec(host.arena_mut(), &value_match_kind)?,
                })
            })
            .collect()
    }
}

// == Encoding

/// The type variable `id`.
fn typ_var(id: &str) -> typ::Typ {
    typ::make::var(p4spectec::phrase!(node: id.into(), span: Span::default()), Vec::new())
}

/// A key value as a `tableKeyValueInterface`.
impl ToSpec for MatchKeyParam {
    fn to_spec(&self, arena: &mut Arena) -> Result<Value, ExternError> {
        match self {
            // Numbers keep their radix spelling as a tagged text
            MatchKeyParam::Number { radix, digits } => {
                let shape = match radix {
                    Radix::Bin => "_BIN text",
                    Radix::Dec => "_DEC text",
                    Radix::Hex => "_HEX text",
                };
                let value_digits = digits.to_spec(arena)?;
                Ok(value::make::case_string! {
                    arena: arena,
                    shape: shape,
                    args: vec![value_digits],
                    typ: "tableKeyValueInterface",
                    span: Span::default(),
                }?)
            }
            // `value/len` becomes a text and a natural
            MatchKeyParam::Prefix { value, len } => {
                let value_prefix = value.to_spec(arena)?;
                let value_len = len.to_spec(arena)?;
                Ok(value::make::case_string! {
                    arena: arena,
                    shape: "text _SLASH nat",
                    args: vec![value_prefix, value_len],
                    typ: "tableKeyValueInterface",
                    span: Span::default(),
                }?)
            }
        }
    }
}

/// A key as a `tableKeyInterface`: its name and value.
impl ToSpec for MatchKey {
    fn to_spec(&self, arena: &mut Arena) -> Result<Value, ExternError> {
        let value_name = self.name.to_spec(arena)?;
        let value_param = self.param.to_spec(arena)?;
        Ok(value::make::tuple(
            arena,
            typ_var("tableKeyInterface").node.into(),
            vec![value_name, value_param],
            Span::default(),
        )?)
    }
}

/// The keys as a `tableKeyInterface` list.
impl ToSpec for Vec<MatchKey> {
    fn to_spec(&self, arena: &mut Arena) -> Result<Value, ExternError> {
        let values_key = self
            .iter()
            .map(|key| key.to_spec(arena))
            .collect::<Result<Vec<_>, _>>()?;
        let typ_keys = typ::make::list(typ_var("tableKeyInterface"));
        Ok(value::make::list(arena, typ_keys.node.into(), values_key, Span::default())?)
    }
}

/// The action as a `tableActionInterface`: its name and named integer arguments.
impl ToSpec for ActionCall {
    fn to_spec(&self, arena: &mut Arena) -> Result<Value, ExternError> {
        let value_name = self.name.to_spec(arena)?;
        let typ_arg = typ_var("tableActionArgumentInterface");
        let values_arg = self
            .args
            .iter()
            .map(|(id, int)| {
                let value_id = id.to_spec(arena)?;
                let value_int = int.to_spec(arena)?;
                Ok(value::make::tuple(
                    arena,
                    typ_arg.node.clone().into(),
                    vec![value_id, value_int],
                    Span::default(),
                )?)
            })
            .collect::<Result<Vec<_>, ExternError>>()?;
        let value_args = value::make::list(
            arena,
            typ::make::list(typ_arg).node.into(),
            values_arg,
            Span::default(),
        )?;
        Ok(value::make::tuple(
            arena,
            typ_var("tableActionInterface").node.into(),
            vec![value_name, value_args],
            Span::default(),
        )?)
    }
}
