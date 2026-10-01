//! Link resolution and declared destinations for a document batch
//!
//! Callers supply function and relation lookups after collecting document titles.
//! One context tracks emitted destinations across all fragments in the batch.
//! Body namespaces and block numbering belong to each renderer invocation.

use std::collections::BTreeSet;

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

    /// Registers a destination and reports whether this is its first declaration.
    pub fn claim_anchor(&mut self, anchor: &str) -> bool {
        self.emitted.insert(anchor.to_owned())
    }
}
