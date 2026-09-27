//! Rendering state shared by the splicers of one document batch
//!
//! Targets stay immutable after the skeleton prepass.
//! Emitted anchors, PL counters, and warnings belong to one splice invocation.

use super::{
    super::adoc::pl::{Renderer, doc::doc::Subject},
    anchor::{Presentation, Targets},
};
use crate::diagnostic::Report;
use std::collections::BTreeSet;

/// Owns mutable rendering state for one splice invocation.
pub(super) struct Context<'a> {
    /// Resolves immutable declarations collected from all input files.
    pub targets: &'a Targets,
    anchors_emitted: BTreeSet<String>,
    /// Shares PL arm counters between fragments.
    pub renderer: Renderer<'a>,
    /// Collects diagnostics without choosing an output stream.
    pub warnings: &'a mut Vec<Report>,
}

impl<'a> Context<'a> {
    /// Starts an invocation with fresh anchors and counters.
    pub(super) fn new(
        targets: &'a Targets,
        anchor: &'a dyn Fn(&Subject) -> Option<String>,
        warnings: &'a mut Vec<Report>,
    ) -> Self {
        Self {
            targets,
            anchors_emitted: BTreeSet::new(),
            renderer: Renderer::new(anchor),
            warnings,
        }
    }
    /// Claims an anchor only on its first rendered occurrence.
    pub(super) fn claim_anchor(&mut self, anchor: &str) -> bool {
        self.anchors_emitted.insert(anchor.to_owned())
    }
}

/// Resolves a PL subject through the skeleton title targets.
pub(super) fn resolve_prose(targets: &Targets, subject: &Subject) -> Option<String> {
    match subject {
        Subject::Function(name) => targets.func(Presentation::Prose, name),
        Subject::Relation(name) => targets.rel(Presentation::Prose, name),
    }
}
