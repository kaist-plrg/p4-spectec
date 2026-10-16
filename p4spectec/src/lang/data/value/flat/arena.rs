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

/// Annotation allocations remembered before interning by contents.
const TYPES_RECENT_LEN: usize = 64;

/// A retained annotation allocation and its interned handle.
type TypRecent = (Rc<TypKind>, Interned<Rc<TypKind>>);

/// Storage for value bodies, types, and spans.
#[derive(Debug)]
pub struct ValueArena {
    generation: Rc<()>,
    pub(super) bools: [Option<Value>; 2],
    /// Bodies, with canonical identities; a case reads its mixop's.
    values: super::lazy::Values,
    /// Tail bodies already derived from immutable non-empty lists.
    tails:
        hashbrown::HashMap<Interned<ValueKind>, Interned<ValueKind>, foldhash::fast::RandomState>,
    /// Exact structural columns, invalidated with their source bodies.
    pub(super) columns: super::projection::Columns,
    /// Types, shared by exact contents including nested source spans.
    types: Interner<Rc<TypKind>>,
    /// Recently interned annotations by allocation, each retained.
    types_recent: [Option<TypRecent>; TYPES_RECENT_LEN],
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
            generation: Rc::new(()),
            bools: [None; 2],
            values: super::lazy::Values::default(),
            tails: Default::default(),
            columns: Default::default(),
            types: Interner::new(),
            types_recent: std::array::from_fn(|_| None),
            spans,
        }
    }
}

impl ValueArena {
    /// The handle of the default location, which `Default` interns first.
    pub(super) fn span_default() -> Interned<Span> {
        Interned::from_index(0)
    }

    /// Identifies this value generation without retaining its stored bodies.
    pub(crate) fn generation(&self) -> &Rc<()> {
        &self.generation
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

    /// Constructs a composite, allocating children only for a new exact body.
    pub(crate) fn composite(
        &mut self,
        arena_mixop: &MixopArena,
        shape: super::projection::Shape,
        typ: Rc<TypKind>,
        values: &[Value],
    ) -> Result<Value, ValueError> {
        let node = self.values.composite(arena_mixop, shape, values)?;
        let note = self.intern_typ(typ)?;
        Ok(Value { node, note, span: Self::span_default() })
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
        // A retained allocation cannot be reused, so its address identifies it
        let slot = (Rc::as_ptr(&typ) as usize >> 4) % TYPES_RECENT_LEN;
        if let Some((typ_recent, id)) = &self.types_recent[slot]
            && Rc::ptr_eq(typ_recent, &typ)
        {
            return Ok(*id);
        }
        let id = self.types.intern(Rc::clone(&typ))?;
        self.types_recent[slot] = Some((typ, id));
        Ok(id)
    }

    /// Interns an exact source location.
    pub(in crate::lang::data) fn intern_span(
        &mut self,
        span: Span,
    ) -> Result<Interned<Span>, std::num::TryFromIntError> {
        if span == Span::default() { Ok(Self::span_default()) } else { self.spans.intern(span) }
    }

    /// Reuses a list tail without copying and hashing the same suffix again.
    pub(crate) fn list_tail(
        &mut self,
        arena_mixop: &MixopArena,
        value: Value,
    ) -> Result<Value, ValueError> {
        let node = if let Some(node) = self.tails.get(&value.node) {
            *node
        } else {
            let ValueKind::List(values) = self.kind(value.node) else { unreachable!("list tail") };
            let values = values
                .get(1..)
                .expect("cons pattern must match a non-empty list")
                .to_vec();
            let node = self.intern_kind(arena_mixop, ValueKind::List(values))?;
            self.tails.insert(value.node, node);
            node
        };
        Ok(Value { node, note: value.note, span: Self::span_default() })
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
