//! Host payloads of extern syntax types
//!
//! A host defines one `TypDef<T>` per `extern syntax` declaration
//! in an arena and keeps it;
//! `make` wraps a native payload of type `T` into a value,
//! and `get` reads it back only through the definition that made it.
//! The arena issues every identity:
//! two definitions with one name and payload type still differ,
//! and each value made gets its own serial.
//! Exact identity is that serial; canonical identity is the payload's `Ord`.

use std::{
    any::{Any, TypeId},
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    marker::PhantomData,
};

use serde::{Deserializer, Serializer};
use serde_state::{DeserializeState, SerializeState};

use crate::lang::{
    common::source::Span,
    data::{arena::Arena, typ},
};

use super::{
    ValueError, ValueTag,
    flat::{
        Value, ValueKind,
        external::{DecodeContext, EncodeContext},
    },
};

// = Definition identities

/// A definition's index, valid only in the arena that issued it.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypDefId(u32);

/// A defined name and the payload type its values carry.
#[derive(Debug)]
struct TypDefEntry {
    name: String,
    type_id: TypeId,
}

/// Extern type definitions issued in one arena.
///
/// Definitions outlive `Arena::reset_values`,
/// since hosts and runners keep them across programs.
#[derive(Debug, Default)]
pub struct TypDefArena {
    entries: Vec<TypDefEntry>,
}

impl TypDefArena {
    /// Issues a definition distinct from every earlier one.
    fn define(&mut self, name: &str, type_id: TypeId) -> Result<TypDefId, ValueError> {
        let index = u32::try_from(self.entries.len())?;
        self.entries
            .push(TypDefEntry { name: name.to_owned(), type_id });
        Ok(TypDefId(index))
    }

    /// The declaration name of a definition issued here.
    pub fn name(&self, id: TypDefId) -> &str {
        &self.entries[id.0 as usize].name
    }

    /// Whether this arena issued `id` for payloads of `type_id`.
    fn carries(&self, id: TypDefId, type_id: TypeId) -> bool {
        self.entries
            .get(id.0 as usize)
            .is_some_and(|entry| entry.type_id == type_id)
    }

    /// Names a definition and its index, telling equally named ones apart.
    fn describe(&self, id: TypDefId) -> String {
        format!("{} (definition {})", self.name(id), id.0)
    }
}

// = Typed definitions

/// A host's definition of one extern syntax type, with payloads of type `T`.
///
/// Payloads must not change their order or hash once made:
/// the arena stores them for the rest of the program.
/// Payloads that compare equal must be interchangeable in host operations,
/// since equal values share cached results.
pub struct TypDef<T> {
    id: TypDefId,
    marker: PhantomData<fn() -> T>,
}

impl<T> Clone for TypDef<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for TypDef<T> {}

impl<T> fmt::Debug for TypDef<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_tuple("TypDef").field(&self.id).finish()
    }
}

impl<T: Ord + Hash + fmt::Debug + 'static> TypDef<T> {
    /// Defines a new type named `name` in `arena`, distinct from every other.
    pub fn new(arena: &mut Arena, name: &str) -> Result<Self, ValueError> {
        let id = arena.typdef.define(name, TypeId::of::<T>())?;
        Ok(Self { id, marker: PhantomData })
    }

    /// The identity of this definition in its arena.
    pub fn id(self) -> TypDefId {
        self.id
    }

    /// Allocates a value of this definition, typed by its declaration name.
    pub fn make(self, arena: &mut Arena, payload: T, span: Span) -> Result<Value, ValueError> {
        // Reject a definition that another arena issued
        if !arena.typdef.carries(self.id, TypeId::of::<T>()) {
            return Err(ValueError::TypDefForeign);
        }
        // A fresh serial keeps equal payloads as distinct stored values
        let serial = arena.value.issue_extern_serial()?;
        let value_extern = ExternValue { typdef: self.id, serial, payload: Box::new(payload) };
        let id_typ = crate::phrase!(node: arena.typdef.name(self.id).into(), span: Span::default());
        let typ = typ::make::var(id_typ, Vec::new());
        Value::new(arena, ValueKind::Extern(value_extern), typ.node.into(), span)
    }

    /// Borrows the payload of `value` if this definition made it.
    pub fn get<'a>(self, arena: &'a Arena, value: &Value) -> Result<&'a T, ValueError> {
        // Only extern bodies carry payloads
        let kind = arena.value.kind(value.node);
        let ValueKind::Extern(value_extern) = kind else {
            return Err(ValueError::KindMismatch {
                expected: ValueTag::Extern,
                actual: kind.tag(),
            });
        };
        // Another definition is a mismatch, even with the same name and type
        if value_extern.typdef != self.id {
            return Err(ValueError::TypDefMismatch {
                expected: arena.typdef.describe(self.id),
                actual: arena.typdef.describe(value_extern.typdef),
            });
        }
        // `make` admitted only payloads of `T` under this definition
        Ok(value_extern
            .payload
            .as_any()
            .downcast_ref::<T>()
            .expect("a definition carries one payload type"))
    }
}

// = Values

/// A payload with the definition that made it; the payload type is erased.
#[derive(Debug)]
pub struct ExternValue {
    /// The definition that made this value.
    typdef: TypDefId,
    /// Issued once per made value by its arena.
    serial: u32,
    payload: Box<dyn Payload>,
}

// Exact identity is the serial issued by `make`
impl PartialEq for ExternValue {
    fn eq(&self, value_other: &Self) -> bool {
        self.serial == value_other.serial
    }
}

impl Eq for ExternValue {}

impl Hash for ExternValue {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.serial.hash(hasher);
    }
}

impl ExternValue {
    /// The definition that made this value.
    pub fn typdef(&self) -> TypDefId {
        self.typdef
    }

    /// Whether two values mean the same: one definition and equal payloads.
    pub(in crate::lang::data) fn canon_eq(&self, value_other: &Self) -> bool {
        self.typdef == value_other.typdef && self.payload.cmp_dyn(&*value_other.payload).is_eq()
    }

    /// Hashes the meaning, consistently with `canon_eq`.
    pub(in crate::lang::data) fn canon_hash<H: Hasher>(&self, hasher: &mut H) {
        self.typdef.hash(hasher);
        self.payload.hash_dyn(hasher);
    }

    /// Orders by definition, then by payload within one definition.
    pub(in crate::lang::data) fn syntax_cmp(&self, value_other: &Self) -> Ordering {
        self.typdef
            .cmp(&value_other.typdef)
            .then_with(|| self.payload.cmp_dyn(&*value_other.payload))
    }
}

// - Encoding

// Payloads have no codec; encoding a value that contains one fails
impl SerializeState<EncodeContext<'_>> for ExternValue {
    fn serialize_state<S: Serializer>(
        &self,
        _serializer: S,
        _ctx: &EncodeContext<'_>,
    ) -> Result<S::Ok, S::Error> {
        Err(serde::ser::Error::custom(ValueError::CodecMissing))
    }
}

impl<'de> DeserializeState<'de, DecodeContext<'_>> for ExternValue {
    fn deserialize_state<D: Deserializer<'de>>(
        _ctx: &mut DecodeContext<'_>,
        _deserializer: D,
    ) -> Result<Self, D::Error> {
        Err(serde::de::Error::custom(ValueError::CodecMissing))
    }
}

// = Erased payloads

/// What a payload supplies once its type is erased.
trait Payload: fmt::Debug {
    fn as_any(&self) -> &dyn Any;

    /// Orders two payloads of one definition.
    fn cmp_dyn(&self, payload_other: &dyn Payload) -> Ordering;

    fn hash_dyn(&self, hasher: &mut dyn Hasher);
}

impl<T: Ord + Hash + fmt::Debug + 'static> Payload for T {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn cmp_dyn(&self, payload_other: &dyn Payload) -> Ordering {
        let payload_other = payload_other
            .as_any()
            .downcast_ref::<T>()
            .expect("payloads of one definition share a type");
        self.cmp(payload_other)
    }

    fn hash_dyn(&self, mut hasher: &mut dyn Hasher) {
        self.hash(&mut hasher);
    }
}
