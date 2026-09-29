//! Typed splice stores and the common marker rendering procedure
//!
//! `func::Prose` implements `Kind`: it selects and renders function definitions.
//! `Splicer<func::Prose>` owns that kind's store, usage flags, and parsed requests.
//! The driver keeps it beside other kinds as `Box<dyn Splice>`.
//! For `${func-prose: f f}`, one request retains both keys;
//! rendering selects the same definition twice with distinct original positions.

use super::super::anchor::AnchorContext;
use super::{
    anchor::{Decls, Targets},
    error::{Error, warn},
    parser,
    source::Source,
};
use crate::lang::{
    common::source::{Phrase, Span},
    el::ast as el,
    pl::ast as pl,
};
use std::collections::BTreeMap;

// == Splice key and values

/// Parses and labels the keys accepted by a marker.
pub(super) trait Key: Ord {
    fn to_string(&self) -> String;

    fn to_anchor(&self) -> String {
        self.to_string()
    }

    fn parse(source: &mut Source<'_>) -> Result<Vec<Phrase<Self>>, Error>
    where
        Self: Sized;
}

impl Key for String {
    fn to_string(&self) -> String {
        self.clone()
    }

    fn parse(source: &mut Source<'_>) -> Result<Vec<Phrase<Self>>, Error> {
        parser::parse_ids(source)
    }
}

/// Borrows one selected definition with its key and original request position.
///
/// In `${func-prose: missing f}`, the selection for `f` retains index 1.
/// Repeated keys stay separate even when they borrow the same data.
pub(super) struct Selection<'a, Key, Value> {
    /// Retains the key's position before missing definitions are skipped.
    pub idx_key: usize,
    /// Identifies the selected definition within this marker kind.
    pub key: &'a Key,
    /// Borrows the definition selected from the store.
    pub data: &'a Value,
}

/// Supplies kind-specific operations while `Splicer` owns the mutable state.
pub(super) trait Kind<'spec> {
    type Key: Key;
    type Value;
    const NAME: &'static str;
    const PREFIX: &'static str;
    const SUFFIX: &'static str;

    fn init(spec_el: &'spec el::Spec, spec_pl: &'spec pl::Spec)
    -> BTreeMap<Self::Key, Self::Value>;

    fn render(
        anchor_ctx: &mut AnchorContext<'_>,
        idx_request: usize,
        values: &[Selection<'_, Self::Key, Self::Value>],
    ) -> Result<String, Error>;

    fn anchor(_anchor_ctx: &AnchorContext<'_>, _name: &str) -> Option<String> {
        None
    }

    fn collect_link_targets(_keys: &[Phrase<Self::Key>], _decls: &Decls, _targets: &mut Targets) {}
}

// == Splice lookups

struct Entry<Value> {
    used: bool,
    data: Value,
}
struct Store<Key, Value> {
    entries: BTreeMap<Key, Entry<Value>>,
}

impl<Key: Ord, Value> Store<Key, Value> {
    fn init(values: BTreeMap<Key, Value>) -> Self {
        let entries = values
            .into_iter()
            .map(|(key, data)| (key, Entry { used: false, data }))
            .collect();
        Self { entries }
    }

    fn cardinal(&self) -> usize {
        self.entries.len()
    }

    fn unused(&self) -> Vec<&Key> {
        self.entries
            .iter()
            .filter_map(|(key, entry)| (!entry.used).then_some(key))
            .collect()
    }
}

// == Splicer

/// Lets the driver invoke different kinds through one `dyn Splice` interface.
///
/// `parse` returns an index into this splicer's requests; the driver retains it
/// in a marker and supplies it to target collection and rendering.
pub(super) trait Splice {
    fn name(&self) -> &'static str;

    fn parse(&mut self, source: &mut Source<'_>) -> Result<usize, Error>;

    fn render(
        &mut self,
        anchor_ctx: &mut AnchorContext<'_>,
        idx_request: usize,
    ) -> Result<String, Error>;

    fn collect_link_targets(&self, idx_request: usize, decls: &Decls, targets: &mut Targets);

    fn warn_unused(&self);
}

/// Retains the parsed keys for one marker occurrence.
///
/// `${func-prose: f g}` stores both located keys here; the skeleton holds only
/// the request index, so target collection and rendering reuse the same parse.
struct Request<Key> {
    keys: Vec<Phrase<Key>>,
}

/// Owns a kind-specific store and its usage flags.
pub(super) struct Splicer<'spec, SpliceKind: Kind<'spec>> {
    store: Store<SpliceKind::Key, SpliceKind::Value>,
    requests: Vec<Request<SpliceKind::Key>>,
}

impl<'spec, SpliceKind: Kind<'spec>> Splicer<'spec, SpliceKind> {
    /// Builds a store from the EL and PL specifications.
    pub(super) fn new(spec_el: &'spec el::Spec, spec_pl: &'spec pl::Spec) -> Self {
        Self { store: Store::init(SpliceKind::init(spec_el, spec_pl)), requests: Vec::new() }
    }
}

impl<'spec, SpliceKind: Kind<'spec>> Splice for Splicer<'spec, SpliceKind> {
    fn name(&self) -> &'static str {
        SpliceKind::NAME
    }

    fn parse(&mut self, source: &mut Source<'_>) -> Result<usize, Error> {
        let keys = SpliceKind::Key::parse(source)?;
        let idx_request = self.requests.len();
        self.requests.push(Request { keys });
        Ok(idx_request)
    }

    fn render(
        &mut self,
        anchor_ctx: &mut AnchorContext<'_>,
        idx_request: usize,
    ) -> Result<String, Error> {
        let keys = &self.requests[idx_request].keys;
        // Finish mutable usage updates before selections borrow definition data
        for key in keys {
            if let Some(entry) = self.store.entries.get_mut(&key.node) {
                entry.used = true;
            } else {
                warn(
                    &key.span,
                    format!("{} splice key not found: {}", SpliceKind::NAME, key.node.to_string()),
                );
            }
        }
        let mut headers = String::new();
        let mut values = Vec::with_capacity(keys.len());
        // Preserve repetitions while declaring each destination once
        for (idx_key, key) in keys.iter().enumerate() {
            let Some(entry) = self.store.entries.get(&key.node) else {
                continue;
            };
            values.push(Selection { idx_key, key: &key.node, data: &entry.data });
            if let Some(anchor) = SpliceKind::anchor(anchor_ctx, &key.node.to_anchor())
                && anchor_ctx.claim_anchor(&anchor)
            {
                if headers.is_empty() {
                    headers.push_str("++++\n");
                }
                headers.push_str(&format!("<span id=\"{anchor}\"></span>\n"));
            }
        }
        if !headers.is_empty() {
            headers.push_str("++++\n");
        }
        let text = SpliceKind::render(anchor_ctx, idx_request, &values)?;
        Ok(format!("{headers}{}{text}{}", SpliceKind::PREFIX, SpliceKind::SUFFIX))
    }

    fn collect_link_targets(&self, idx_request: usize, decls: &Decls, targets: &mut Targets) {
        SpliceKind::collect_link_targets(&self.requests[idx_request].keys, decls, targets);
    }

    /// Reports sorted unused keys in groups of five, including empty stores.
    fn warn_unused(&self) {
        // Report unused totals even when the store is empty
        let keys = self.store.unused();
        let num_unused = keys.len();
        let total = self.store.cardinal();
        let percentage = if total == 0 { 0.0 } else { num_unused as f64 / total as f64 * 100.0 };
        warn(
            &Span::default(),
            format!(
                "unused {num_unused} {} splices out of {total} ({percentage:.2}%)",
                SpliceKind::NAME
            ),
        );
        // List sorted keys in groups of five
        if keys.is_empty() {
            warn(&Span::default(), "\t".to_owned());
        }
        for keys in keys.chunks(5) {
            warn(
                &Span::default(),
                format!(
                    "\t{}",
                    keys.iter()
                        .map(|key| key.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
            );
        }
    }
}
