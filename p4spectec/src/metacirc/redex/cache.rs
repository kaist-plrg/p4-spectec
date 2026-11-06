//! The codec state of the boundary above spec-meta-redex

use crate::lang::data::notation::{AtomPhrase, flat::Mixop};
use std::collections::HashMap;

/// The codec state of the boundary above spec-meta-redex.
///
/// Notation is converted once per spelling, and a pure host call that succeeded
/// is answered once per request text, if enabled;
/// every reentry depth shares it, and `clear` runs between programs.
pub(super) struct Cache {
    pub(super) enabled: bool,
    /// A value's notation, as the s-expression of its atom rows.
    pub(super) mixops_out: HashMap<Mixop, String>,
    /// A notation by the s-expression of its atom rows, as Racket's `write` prints it.
    pub(super) mixops_in: HashMap<String, Mixop>,
    /// A struct field's atom by its spelling.
    pub(super) atoms_in: HashMap<String, AtomPhrase>,
    /// The response to a host request of the base, by the request's text.
    responses: HashMap<String, String>,
}

impl Cache {
    pub(super) fn new(enabled: bool) -> Self {
        Self {
            enabled,
            mixops_out: HashMap::new(),
            mixops_in: HashMap::new(),
            atoms_in: HashMap::new(),
            responses: HashMap::new(),
        }
    }

    /// Clears the memoized conversions and responses between programs.
    pub(super) fn clear(&mut self) {
        self.mixops_out.clear();
        self.mixops_in.clear();
        self.atoms_in.clear();
        self.responses.clear();
    }

    /// The response cached for host request `request`.
    pub(super) fn response(&self, request: &str) -> Option<&str> {
        self.responses.get(request).map(String::as_str)
    }

    /// Caches `response` to host request `request`, if enabled.
    pub(super) fn cache_response(&mut self, request: String, response: &str) {
        if self.enabled {
            self.responses.insert(request, response.to_owned());
        }
    }
}
