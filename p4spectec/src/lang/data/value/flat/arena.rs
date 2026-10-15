//! Append-only storage for value bodies, types, and spans
//!
//! `ValueArena` is the value half of an `Arena`;
//! handles belong to one arena; annotation changes preserve the stored body.
//! Bodies are interned canonically, types by exact contents, spans exactly;
//! the default span is interned first so generated values share it.

use std::rc::Rc;

use crate::lang::{
    common::source::Span,
    data::{
        intern::{CanonId, CanonLookup, Interned, Interner},
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
    pub(super) bools: [Option<Value>; 2],
    /// Bodies, with canonical identities; a case reads its mixop's.
    values: super::lazy::Values,
    /// Types, shared by exact contents including nested source spans.
    types: Interner<Rc<TypKind>>,
    /// Spans, shared by equality.
    spans: Interner<Span>,
}

impl Default for ValueArena {
    fn default() -> Self {
        let mut spans = Interner::new();
        let span_default = spans
            .intern_default()
            .expect("the first span fits in an interner index");
        debug_assert_eq!(span_default, Self::span_default());
        Self {
            bools: [None; 2],
            values: super::lazy::Values::default(),
            types: Interner::new(),
            spans,
        }
    }
}

impl ValueArena {
    /// The handle of the default location, which `Default` interns first.
    pub(super) fn span_default() -> Interned<Span> {
        Interned::from_index(0)
    }

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

    /// Interns a shared type annotation by exact contents.
    #[inline]
    pub(in crate::lang::data) fn intern_typ(
        &mut self,
        typ: Rc<TypKind>,
    ) -> Result<Interned<Rc<TypKind>>, std::num::TryFromIntError> {
        self.types.intern(typ)
    }

    /// Interns an exact source location.
    pub(in crate::lang::data) fn intern_span(
        &mut self,
        span: Span,
    ) -> Result<Interned<Span>, std::num::TryFromIntError> {
        if span == Span::default() { Ok(Self::span_default()) } else { self.spans.intern(span) }
    }

    // - Lookup

    /// Reads the body behind its handle.
    #[inline]
    pub fn kind(&self, value: Interned<ValueKind>) -> &ValueKind {
        self.values.get(value)
    }

    /// Reads a type annotation.
    #[inline]
    pub fn typ(&self, typ: Interned<Rc<TypKind>>) -> &Rc<TypKind> {
        self.types.get(typ)
    }

    /// Reads a source location.
    #[inline]
    pub fn span(&self, span: Interned<Span>) -> &Span {
        self.spans.get(span)
    }

    /// Returns the canonical identity of a body.
    #[inline]
    pub fn canon_id(&self, value: Interned<ValueKind>) -> CanonId<ValueKind> {
        self.values.canon_id(value)
    }
}
