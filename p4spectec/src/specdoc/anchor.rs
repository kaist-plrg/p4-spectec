//! Link resolution and declared destinations for a document batch
//!
//! Callers supply function and relation lookups after collecting document titles
//! Group and otherwise destinations are derived from definition IDs
//! One context tracks emitted destinations across all fragments in the batch

use std::collections::BTreeSet;

// == Definitions

/// Identifies a function or relation destination.
#[derive(Clone, Copy)]
pub enum DefKind {
    /// Identifies a function.
    Func,
    /// Identifies a relation.
    Rel,
}

// == Presentations

/// Selects the presentation of a function or relation title.
#[derive(Clone, Copy)]
pub enum Presentation {
    /// Selects a prose title.
    Prose,
    /// Selects a mathematical title.
    Latex,
}

impl Presentation {
    /// Returns the title namespace component.
    pub fn name(self) -> &'static str {
        match self {
            Self::Prose => "prose",
            Self::Latex => "latex",
        }
    }
}

// == Anchor context

/// Resolves document links and tracks destinations already declared.
pub struct AnchorContext<'a> {
    func: &'a dyn Fn(Presentation, &str) -> Option<String>,
    rel: &'a dyn Fn(Presentation, &str) -> Option<String>,
    emitted: BTreeSet<String>,
}

impl Default for AnchorContext<'_> {
    /// Starts a document batch with no resolved links or declared destinations.
    fn default() -> Self {
        Self::new(&|_, _| None, &|_, _| None)
    }
}

impl<'a> AnchorContext<'a> {
    /// Starts a document batch with supplied lookups and no declared destinations.
    pub fn new(
        func: &'a dyn Fn(Presentation, &str) -> Option<String>,
        rel: &'a dyn Fn(Presentation, &str) -> Option<String>,
    ) -> Self {
        Self { func, rel, emitted: BTreeSet::new() }
    }

    /// Resolves a function title without a leading hash.
    pub fn func(&self, presentation: Presentation, id: &str) -> Option<String> {
        (self.func)(presentation, id)
    }

    /// Resolves a relation title without a leading hash.
    pub fn rel(&self, presentation: Presentation, id: &str) -> Option<String> {
        (self.rel)(presentation, id)
    }

    /// Returns a rule-group anchor, sanitizing path separators.
    pub fn group(&self, id_rel: &str, id_group: &str) -> String {
        let id_rel_sanitized = id_rel.replace('/', "-");
        let id_group_sanitized = id_group.replace('/', "-");
        format!("{id_rel_sanitized}-{id_group_sanitized}")
    }

    /// Returns the anchor of an otherwise block.
    pub fn otherwise(&self, def_kind: DefKind, id_def: &str) -> String {
        let prefix = match def_kind {
            DefKind::Func => "func",
            DefKind::Rel => "rel",
        };
        format!("{prefix}:{id_def}-else")
    }

    /// Registers a destination and reports whether this is its first declaration.
    pub fn claim_anchor(&mut self, anchor: &str) -> bool {
        self.emitted.insert(anchor.to_owned())
    }
}
