//! Storage for prepared specifications' notation shapes and a run's values
//!
//! An `Arena` holds a `MixopArena` and extern type definitions
//! beside the value storage.
//! The shapes are shared by every specification prepared in the arena,
//! and outlive `reset_values`; a case body's notation is one of them.
//! Extern type definitions outlive it too, as hosts keep them across programs.
//! `value` allocates values into the arena and reads them back.

use super::{
    notation::MixopArena,
    value::{ValueArena, extern_value::TypDefArena},
};

// = Arena storage

/// Storage for prepared specifications' notation shapes and the values of one run.
///
/// Shapes and extern type definitions survive `reset_values`;
/// value, type, and span handles do not.
#[derive(Debug, Default)]
pub struct Arena {
    /// Notation shapes of prepared syntax and case bodies.
    pub mixop: MixopArena,
    /// Extern type definitions issued to hosts.
    pub typdef: TypDefArena,
    /// Value bodies, types, and spans.
    pub value: ValueArena,
}

impl Arena {
    // - Construction

    /// An empty arena with the default span pre-interned.
    pub fn new() -> Self {
        Self::default()
    }

    /// An empty value store over shapes interned earlier.
    pub fn with_arena_mixop(arena_mixop: MixopArena) -> Self {
        Self { mixop: arena_mixop, typdef: TypDefArena::default(), value: ValueArena::default() }
    }

    /// Drops every value, type, and span, keeping the shapes and definitions.
    ///
    /// Value, type, and span handles issued before become invalid
    /// and numbering starts over;
    /// shape handles and extern type definitions stay valid.
    /// Clear every sharing runner's caches and host state first.
    pub fn reset_values(&mut self) {
        self.value = ValueArena::default();
    }
}
