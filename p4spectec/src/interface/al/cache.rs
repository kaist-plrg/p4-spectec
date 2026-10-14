//! Successful AL representation conversions within one arena
//!
//! Each host boundary owns its cache and passes it through recursive conversions.
//! Keys compare payloads without type notes or source locations;
//! a hit retains the annotations of the first converted value.
//! The two directions are independent and retain entries until cleared.

use std::collections::HashMap;

use foldhash::fast::RandomState;

use crate::lang::data::{
    intern::CanonId,
    notation::flat::{Mixop, MixopKind},
    typ::Typ,
    value::flat::{Value, ValueKind},
};

/// Conversion results owned by one host boundary in one arena.
///
/// Clear before resetting or replacing the arena; canonical ids and stored
/// handles are only valid in the arena that produced them.
/// Tables have no capacity limit or eviction policy.
pub struct Cache {
    pub(super) enabled: bool,
    pub(super) values_encode: HashMap<CanonId<ValueKind>, Value, RandomState>,
    pub(super) values_decode: HashMap<CanonId<ValueKind>, Value, RandomState>,
    pub(super) mixops_encode: HashMap<CanonId<MixopKind>, Value, RandomState>,
    pub(super) mixops_decode: HashMap<CanonId<ValueKind>, Mixop, RandomState>,
    pub(super) typs_decode: HashMap<CanonId<ValueKind>, Typ, RandomState>,
}

impl Cache {
    /// Creates empty tables, bypassing lookup and insertion when disabled.
    pub fn new(enabled: bool) -> Self {
        Self {
            enabled,
            values_encode: HashMap::default(),
            values_decode: HashMap::default(),
            mixops_encode: HashMap::default(),
            mixops_decode: HashMap::default(),
            typs_decode: HashMap::default(),
        }
    }

    /// Drops all retained results while preserving the enabled setting.
    pub fn clear(&mut self) {
        self.values_encode.clear();
        self.values_decode.clear();
        self.mixops_encode.clear();
        self.mixops_decode.clear();
        self.typs_decode.clear();
    }
}
