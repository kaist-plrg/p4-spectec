//! Append-only storage for value bodies, types, and spans
//!
//! `ValueArena` is the value half of an `Arena`;
//! handles belong to one arena; annotation changes preserve the stored body.
//! Bodies are interned canonically, types by `Rc` identity, spans exactly;
//! the default span is interned first so generated values share it.

use std::rc::Rc;

use crate::lang::{
    common::source::Span,
    data::{
        intern::{CanonId, CanonInterner, Interned, Interner, RcInterner},
        notation::MixopArena,
        typ::TypKind,
    },
};

use super::super::ValueError;
use super::{Value, ValueKind};

// = Arena storage

/// Storage for value bodies, types, and spans.
#[derive(Debug)]
pub struct ValueArena {
    /// Bodies, with canonical identities; a case reads its mixop's.
    values: CanonInterner<ValueKind>,
    /// Types, shared by allocation.
    types: RcInterner<TypKind>,
    /// Spans, shared by equality.
    spans: Interner<Span>,
}

impl Default for ValueArena {
    fn default() -> Self {
        let mut spans = Interner::new();
        spans
            .intern_default()
            .expect("the first span fits in an interner index");
        Self { values: CanonInterner::new(), types: RcInterner::new(), spans }
    }
}

impl ValueArena {
    // - Interning

    /// Interns a value body and its annotations in allocation order.
    pub(in crate::lang::data) fn alloc(
        &mut self,
        arena_mixop: &MixopArena,
        value_kind: ValueKind,
        typ: Rc<TypKind>,
        span: Span,
    ) -> Result<Value, ValueError> {
        let node = self.intern_kind(arena_mixop, value_kind)?;
        let note = self.intern_typ(typ)?;
        let span = self.intern_span(span)?;
        Ok(Value { node, note, span })
    }

    /// Interns a body using the canonical identities of its mixops.
    pub(in crate::lang::data) fn intern_kind(
        &mut self,
        arena_mixop: &MixopArena,
        value_kind: ValueKind,
    ) -> Result<Interned<ValueKind>, std::num::TryFromIntError> {
        self.values.intern(arena_mixop, value_kind)
    }

    /// Interns a shared type annotation by allocation identity.
    pub(in crate::lang::data) fn intern_typ(
        &mut self,
        typ: Rc<TypKind>,
    ) -> Result<Interned<TypKind>, std::num::TryFromIntError> {
        self.types.intern(typ)
    }

    /// Interns an exact source location.
    pub(in crate::lang::data) fn intern_span(
        &mut self,
        span: Span,
    ) -> Result<Interned<Span>, std::num::TryFromIntError> {
        self.spans.intern(span)
    }

    // - Lookup

    /// Reads the body behind its handle.
    pub fn kind(&self, value: Interned<ValueKind>) -> &ValueKind {
        self.values.get(value)
    }

    /// Reads a type annotation.
    pub fn typ(&self, typ: Interned<TypKind>) -> &Rc<TypKind> {
        self.types.get(typ)
    }

    /// Reads a source location.
    pub fn span(&self, span: Interned<Span>) -> &Span {
        self.spans.get(span)
    }

    /// Returns the canonical identity of a body.
    pub fn canon_id(&self, value: Interned<ValueKind>) -> CanonId<ValueKind> {
        self.values.canon_id(value)
    }
}
