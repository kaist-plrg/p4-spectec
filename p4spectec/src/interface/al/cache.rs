//! Successful AL representation conversions within one arena
//!
//! Each host boundary owns its cache and passes it through recursive conversions.
//! Keys compare payloads without type notes or source locations;
//! a hit reuses the annotations of the retained conversion.
//! Each independent table uses clock replacement at a fixed entry limit.

use crate::lang::data::{
    intern::CanonId,
    notation::flat::{Mixop, MixopKind},
    typ::Typ,
    value::flat::{Value, ValueKind},
};

use crate::util::cache::ClockCache;

const NUM_ENTRIES_MAX: usize = 65_536;

/// Conversion results owned by one host boundary in one arena.
///
/// Clear before resetting or replacing the arena; canonical ids and stored
/// handles are only valid in the arena that produced them.
pub struct Cache {
    pub(super) values_encode: ClockCache<CanonId<ValueKind>, Value>,
    pub(super) values_decode: ClockCache<CanonId<ValueKind>, Value>,
    pub(super) mixops_encode: ClockCache<CanonId<MixopKind>, Value>,
    pub(super) mixops_decode: ClockCache<CanonId<ValueKind>, Mixop>,
    pub(super) typs_decode: ClockCache<CanonId<ValueKind>, Typ>,
}

impl Cache {
    /// Creates empty tables with the requested caching mode.
    pub fn new(enabled: bool) -> Self {
        let capacity = if enabled { NUM_ENTRIES_MAX } else { 0 };
        Self {
            values_encode: ClockCache::new(capacity),
            values_decode: ClockCache::new(capacity),
            mixops_encode: ClockCache::new(capacity),
            mixops_decode: ClockCache::new(capacity),
            typs_decode: ClockCache::new(capacity),
        }
    }

    /// Drops all retained results while preserving the caching mode.
    pub fn clear(&mut self) {
        self.values_encode.clear();
        self.values_decode.clear();
        self.mixops_encode.clear();
        self.mixops_decode.clear();
        self.typs_decode.clear();
    }
}
