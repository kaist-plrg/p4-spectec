//! Typed splice stores and the common marker rendering procedure
//!
//! `Kind` supplies key/value types and marker-specific operations.
//! `Splicer` owns usage flags; `Run` erases types only at the driver boundary.

use super::{
    super::adoc::pl::fallthrough,
    context::Context,
    error::{Error, warn},
    parser,
    source::Source,
};
use crate::{
    diagnostic::Report,
    lang::{common::source::Span, el::ast as el, pl::ast as pl},
};
use std::collections::BTreeMap;

// == Splice key and values

/// Parses and labels the keys accepted by a marker.
pub(super) trait Key: Ord {
    fn to_string(&self) -> String;
    fn to_anchor(&self) -> String {
        self.to_string()
    }
    fn parse(source: &mut Source<'_>) -> Result<Vec<Self>, Error>
    where
        Self: Sized;
}

impl Key for String {
    fn to_string(&self) -> String {
        self.clone()
    }
    fn parse(source: &mut Source<'_>) -> Result<Vec<Self>, Error> {
        parser::parse_ids(source)
    }
}

impl Key for (String, String) {
    fn to_string(&self) -> String {
        format!("{}/{}", self.0, self.1)
    }
    fn to_anchor(&self) -> String {
        fallthrough::anchor_of_group(&self.0, &self.1)
    }
    fn parse(source: &mut Source<'_>) -> Result<Vec<Self>, Error> {
        Ok(vec![parser::parse_id_with_sub(source)?])
    }
}

/// Supplies the typed data and rendering policy of one marker.
pub(super) trait Kind<'spec> {
    type Key: Key;
    type Value;
    const NAME: &'static str;
    const PREFIX: &'static str;
    const SUFFIX: &'static str;
    fn init(spec_el: &'spec el::Spec, spec_pl: &'spec pl::Spec) -> Vec<(Self::Key, Self::Value)>;
    fn render(ctx: &mut Context<'_>, values: &[&Self::Value]) -> Result<String, Error>;
    fn anchor(_ctx: &Context<'_>, _name: &str) -> Option<String> {
        None
    }
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
    fn cardinal(&self) -> usize {
        self.entries.len()
    }
    fn add(&mut self, key: Key, data: Value) {
        self.entries.insert(key, Entry { used: false, data });
    }
    fn find(&self, key: &Key) -> Option<&Value> {
        self.entries.get(key).map(|entry| &entry.data)
    }
    fn use_key(&mut self, key: &Key) {
        if let Some(entry) = self.entries.get_mut(key) {
            entry.used = true;
        }
    }
    fn unused(&self) -> Vec<&Key> {
        self.entries
            .iter()
            .filter_map(|(key, entry)| (!entry.used).then_some(key))
            .collect()
    }
    fn init(pairs: Vec<(Key, Value)>) -> Self {
        let mut store = Self { entries: BTreeMap::new() };
        for (key, data) in pairs {
            store.add(key, data);
        }
        store
    }
}

// == Splice configuration

pub(super) const PREFIX_SOURCE: &str = "ifdef::backend-html5[]\n.Click to view the specification source\n[%collapsible]\n====\n[source,watsup]\n----\n";
pub(super) const SUFFIX_SOURCE: &str = "\n----\n====\n\n[.empty]\n--\n\n\n--\n\nendif::[]";
pub(super) const PREFIX_LATEX: &str = "ifdef::backend-html5[]\n[latexmath]\n++++\n";
pub(super) const SUFFIX_LATEX: &str = "\n++++\nendif::[]";
pub(super) const PREFIX_PROSE: &str = "****\n";
pub(super) const SUFFIX_PROSE: &str = "\n****";

// == Splicer

/// Exposes marker operations without exposing their key and value types.
pub(super) trait Run {
    fn name(&self) -> &'static str;
    fn splice(&mut self, source: &mut Source<'_>, ctx: &mut Context<'_>) -> Result<String, Error>;
    fn warn_unused(&self, warnings: &mut Vec<Report>);
}

/// Owns a kind-specific store and its usage flags.
pub(super) struct Splicer<'spec, SpliceKind: Kind<'spec>> {
    store: Store<SpliceKind::Key, SpliceKind::Value>,
}

impl<'spec, SpliceKind: Kind<'spec>> Splicer<'spec, SpliceKind> {
    /// Builds a store from the EL and PL specifications.
    pub(super) fn new(spec_el: &'spec el::Spec, spec_pl: &'spec pl::Spec) -> Self {
        Self { store: Store::init(SpliceKind::init(spec_el, spec_pl)) }
    }

    /// Resolves keys, claims anchors, records usage, and wraps rendered values.
    fn render(
        &mut self,
        keys: Vec<SpliceKind::Key>,
        ctx: &mut Context<'_>,
    ) -> Result<String, Error> {
        // Retain only keys available in this marker's store
        let keys: Vec<_> = keys
            .into_iter()
            .filter(|key| {
                if self.store.find(key).is_some() {
                    return true;
                }
                warn(
                    ctx.warnings,
                    &Span::default(),
                    format!("{} splice key not found: {}", SpliceKind::NAME, key.to_string()),
                );
                false
            })
            .collect();
        // Declare each destination once across the input batch
        let anchors: Vec<_> = keys
            .iter()
            .filter_map(|key| SpliceKind::anchor(ctx, &key.to_anchor()))
            .collect();
        let anchors: Vec<_> = anchors
            .into_iter()
            .filter(|anchor| ctx.claim_anchor(anchor))
            .collect();
        let headers = if anchors.is_empty() {
            String::new()
        } else {
            format!(
                "++++\n{}\n++++\n",
                anchors
                    .iter()
                    .map(|anchor| format!("<span id=\"{anchor}\"></span>"))
                    .collect::<Vec<_>>()
                    .join("\n")
            )
        };
        // Record usage independently for each splice kind
        for key in &keys {
            self.store.use_key(key);
        }
        let values: Vec<_> = keys.iter().filter_map(|key| self.store.find(key)).collect();
        // Render the selected values between this marker's wrappers
        let text = SpliceKind::render(ctx, &values)?;
        Ok(format!("{headers}{}{text}{}", SpliceKind::PREFIX, SpliceKind::SUFFIX))
    }
}

impl<'spec, SpliceKind: Kind<'spec>> Run for Splicer<'spec, SpliceKind> {
    fn name(&self) -> &'static str {
        SpliceKind::NAME
    }
    fn splice(&mut self, source: &mut Source<'_>, ctx: &mut Context<'_>) -> Result<String, Error> {
        self.render(SpliceKind::Key::parse(source)?, ctx)
    }
    /// Reports sorted unused keys in groups of five, including empty stores.
    fn warn_unused(&self, warnings: &mut Vec<Report>) {
        // Report unused totals even when the store is empty
        let keys = self.store.unused();
        let num_unused = keys.len();
        let total = self.store.cardinal();
        let percentage = if total == 0 { 0.0 } else { num_unused as f64 / total as f64 * 100.0 };
        warn(
            warnings,
            &Span::default(),
            format!(
                "unused {num_unused} {} splices out of {total} ({percentage:.2}%)",
                SpliceKind::NAME
            ),
        );
        // Keep sorted keys and the OCaml grouping of five keys per line
        if keys.is_empty() {
            warn(warnings, &Span::default(), "\t".to_owned());
        }
        for keys in keys.chunks(5) {
            warn(
                warnings,
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
