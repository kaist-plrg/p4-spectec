//! Append-only storage for value bodies, types, and spans
//!
//! Handles belong to one arena; annotation changes preserve the stored body.
//! Bodies are interned canonically, types by `Rc` identity, spans exactly;
//! the default span is interned first so generated values share it.

use std::rc::Rc;

use crate::lang::{common::source::Span, data::typ::TypKind};

use super::{
    intern::{CanonId, CanonInterner, Interner, RcInterner},
    value::{Value, ValueError, ValueKind, ValueRef},
};

// = Arena storage

/// Storage for every value of one run.
#[derive(Debug)]
pub struct ValueArena {
    /// Bodies, with canonical identities.
    pub(super) values: CanonInterner<ValueKind>,
    /// Types, shared by allocation.
    pub(super) types: RcInterner<TypKind>,
    /// Spans, shared by equality.
    pub(super) spans: Interner<Span>,
}

impl Default for ValueArena {
    fn default() -> Self {
        Self::new()
    }
}

impl ValueArena {
    // - Construction

    /// An empty arena with the default span pre-interned.
    pub fn new() -> Self {
        let mut spans = Interner::new();
        spans
            .intern_default()
            .expect("the first span fits in an interner index");
        Self { values: CanonInterner::new(), types: RcInterner::new(), spans }
    }

    // - Interning

    /// Interns the three parts and returns their handles as a value.
    pub(super) fn alloc(
        &mut self,
        kind: ValueKind,
        typ: Rc<TypKind>,
        span: Span,
    ) -> Result<Value, ValueError> {
        let node = self.values.intern(kind)?;
        let note = self.types.intern(typ)?;
        let span = self.spans.intern(span)?;
        Ok(Value { node, note, span })
    }

    // - Lookup

    /// The body of a value.
    pub fn kind(&self, value: &Value) -> &ValueKind {
        self.values.get(value.node)
    }

    /// The canonical identity of a value's body.
    pub fn canon_id(&self, value: &Value) -> CanonId<ValueKind> {
        self.values.canon_id(value.node)
    }

    /// The type of a value.
    pub fn typ(&self, value: &Value) -> &Rc<TypKind> {
        self.types.get(value.note)
    }

    /// The span of a value.
    pub fn span(&self, value: &Value) -> &Span {
        self.spans.get(value.span)
    }

    /// Borrows a value issued by this arena for syntax comparisons.
    pub fn view(&self, value: Value) -> ValueRef<'_> {
        ValueRef { arena: self, value }
    }

    // - Printing

    /// Prints a value in full through the IL printer.
    pub fn to_string(&self, value: &Value) -> String {
        let mut output = String::new();
        let mut printer = crate::lang::traits::print::Printer::new(&mut output);
        crate::lang::il::print::print_value(self, value, &mut printer)
            .expect("writing to a String cannot fail");
        output
    }
}
