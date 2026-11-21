//! Helpers for invoking relations in the spec
//!
//! Each wrapper calls a relation by name and unpacks its outputs.
//! `Lvalue_read` and `Lvalue_write` take a `Cursor` (`LOCAL` or `GLOBAL`)
//! and a `StorageReference` built from a dotted path,
//! `StorageReference::from_path("standard_metadata.egress_spec")`;
//! `TableObject_add_entry` and `TableObject_add_default_action`
//! fill a table from the STF control plane.

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
    call::func::BareName,
    host::SpecHost,
};

// == Lvalues

/// The scope an lvalue is looked up in.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Cursor {
    /// Program-wide variables, such as `standard_metadata` in a scheduler.
    Global,
    /// The current call's variables, such as an extern's parameters.
    Local,
}

/// The cursor value `GLOBAL` or `LOCAL`.
impl ToSpec for Cursor {
    fn to_spec(&self, arena: &mut Arena) -> Result<Value, ExternError> {
        let shape = match self {
            Cursor::Global => "GLOBAL",
            Cursor::Local => "LOCAL",
        };
        Ok(value::make::case_string! {
            arena: arena,
            shape: shape,
            args: vec![],
            typ: "cursor",
            span: Span::default(),
        }?)
    }
}

/// A `storageReference` to a variable or a field of one,
/// such as `standard_metadata.egress_spec`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StorageReference {
    /// A variable, `_BARE nameIR`.
    Name(String),
    /// A field of a reference, `storageReference '.' nameIR`.
    Member(Box<StorageReference>, String),
}

impl StorageReference {
    /// The reference to the dotted `path`: its first segment,
    /// then one field per further segment.
    pub fn from_path(path: &str) -> Self {
        match path.rsplit_once('.') {
            // The last segment is a field of the reference before it
            Some((path_base, id)) => {
                Self::Member(Box::new(Self::from_path(path_base)), id.to_owned())
            }
            // A single segment is the variable itself
            None => Self::Name(path.to_owned()),
        }
    }
}

/// The reference value: a bare name, then one `.` member per field.
impl ToSpec for StorageReference {
    fn to_spec(&self, arena: &mut Arena) -> Result<Value, ExternError> {
        match self {
            // A variable is a bare `prefixedNameIR`
            Self::Name(id) => BareName(id).to_spec(arena),
            // A field wraps the reference to its base
            Self::Member(reference_base, id) => {
                let value_base = reference_base.to_spec(arena)?;
                let value_id = id.to_spec(arena)?;
                Ok(value::make::case_string! {
                    arena: arena,
                    shape: "storageReference '.' nameIR",
                    args: vec![value_base, value_id],
                    typ: "storageReference",
                    span: Span::default(),
                }?)
            }
        }
    }
}

/// Reads the variable or field `reference`, such as `standard_metadata.egress_spec`.
pub fn lvalue_read(
    host: &mut dyn SpecHost,
    cursor: Cursor,
    value_ctx: Value,
    value_arch: Value,
    reference: &StorageReference,
) -> Result<Value, InterpreterError> {
    let value_cursor = cursor.to_spec(host.arena_mut())?;
    let value_ref = reference.to_spec(host.arena_mut())?;
    // The relation returns the single value read
    let values = host
        .call_rel("Lvalue_read", &[value_cursor, value_ctx, value_arch, value_ref])
        .result?;
    Ok(*value::get::one(&values).map_err(ExternError::from)?)
}

/// Writes `value` to the variable or field `reference`; returns the new context.
pub fn lvalue_write(
    host: &mut dyn SpecHost,
    cursor: Cursor,
    value_ctx: Value,
    value_arch: Value,
    reference: &StorageReference,
    value: Value,
) -> Result<Value, InterpreterError> {
    let value_cursor = cursor.to_spec(host.arena_mut())?;
    let value_ref = reference.to_spec(host.arena_mut())?;
    // Writing returns the updated context
    let values = host
        .call_rel("Lvalue_write", &[value_cursor, value_ctx, value_arch, value_ref, value])
        .result?;
    Ok(*value::get::one(&values).map_err(ExternError::from)?)
}

// == Tables

/// Adds an entry to a table object; `None` when the entry was rejected.
pub fn table_object_add_entry(
    host: &mut dyn SpecHost,
    value_ctx: Value,
    value_table: Value,
    value_priority: Value,
    value_keys: Value,
    value_action: Value,
) -> Result<Option<Value>, InterpreterError> {
    // The relation returns the updated table, or `eps` on rejection
    let values = host
        .call_rel(
            "TableObject_add_entry",
            &[value_ctx, value_table, value_priority, value_keys, value_action],
        )
        .result?;
    let value_opt = value::get::one(&values).map_err(ExternError::from)?;
    Ok(Option::<Value>::from_spec(host.arena_mut(), value_opt)?)
}

/// Sets a table object's default action.
pub fn table_object_add_default_action(
    host: &mut dyn SpecHost,
    value_ctx: Value,
    value_table: Value,
    value_action: Value,
) -> Result<Value, InterpreterError> {
    let values = host
        .call_rel("TableObject_add_default_action", &[value_ctx, value_table, value_action])
        .result?;
    Ok(*value::get::one(&values).map_err(ExternError::from)?)
}
