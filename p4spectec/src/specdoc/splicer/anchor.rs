//! Declared function and relation targets in skeleton documents
//!
//! Target collection precedes rendering across every input file.
//! Only title markers for declared EL identifiers create reference destinations.

use std::collections::BTreeSet;

use crate::lang::common::source::Phrase;

use crate::lang::el::ast as el;

use crate::diagnostic::Report;

use super::super::anchor::Presentation;

use super::error;

// == Anchor targets

#[derive(Default)]
pub(super) struct Decls {
    funcs: BTreeSet<String>,
    rels: BTreeSet<String>,
}

#[derive(Default)]
/// Holds declared title targets for both presentations.
pub(super) struct Targets {
    prose: Decls,
    latex: Decls,
}

// == EL declarations

impl Decls {
    /// Collects declarations that may own a title marker.
    pub(super) fn collect_from_el(spec_el: &el::Spec) -> Self {
        let mut decls = Self::default();
        // Collect title-bearing declarations in source order
        for def in spec_el {
            match &def.node {
                // Relation declarations supply relation title targets
                el::DefKind::ExternRel(rel) => {
                    decls.rels.insert(rel.id.node.to_string());
                }
                // Defined relations use the same reference namespace
                el::DefKind::Rel(rel) => {
                    decls.rels.insert(rel.id.node.to_string());
                }
                // Function declarations supply function title targets
                el::DefKind::ExternDec(func) => {
                    decls.funcs.insert(func.id.node.to_string());
                }
                // Builtin functions may also receive document titles
                el::DefKind::BuiltinDec(func) => {
                    decls.funcs.insert(func.id.node.to_string());
                }
                // Defined functions are indexed by their declaration
                el::DefKind::FuncDec(func) => {
                    decls.funcs.insert(func.id.node.to_string());
                }
                // Other definitions do not declare title targets
                _ => {}
            }
        }
        decls
    }
}

// == Skeleton targets and renderer lookups

impl Targets {
    /// Adds declared targets and warns about duplicate title occurrences.
    fn add_ids(
        ids_target: &mut BTreeSet<String>,
        warnings: &mut Vec<Report>,
        name: &str,
        ids_declared: &BTreeSet<String>,
        ids: &[Phrase<String>],
    ) {
        // Ignore undeclared identifiers, retaining duplicate-target diagnostics
        for id in ids {
            if ids_declared.contains(&id.node) && !ids_target.insert(id.node.clone()) {
                warnings.push(error::target_duplicate(&id.span, name, &id.node));
            }
        }
    }

    fn decls(&self, presentation: Presentation) -> &Decls {
        match presentation {
            Presentation::Prose => &self.prose,
            Presentation::Latex => &self.latex,
        }
    }

    fn decls_mut(&mut self, presentation: Presentation) -> &mut Decls {
        match presentation {
            Presentation::Prose => &mut self.prose,
            Presentation::Latex => &mut self.latex,
        }
    }

    /// Registers declared function titles and reports repeated destinations.
    pub(super) fn add_funcs(
        &mut self,
        warnings: &mut Vec<Report>,
        presentation: Presentation,
        name: &str,
        decls: &Decls,
        keys: &[Phrase<String>],
    ) {
        Self::add_ids(&mut self.decls_mut(presentation).funcs, warnings, name, &decls.funcs, keys);
    }

    /// Registers declared relation titles and reports repeated destinations.
    pub(super) fn add_rels(
        &mut self,
        warnings: &mut Vec<Report>,
        presentation: Presentation,
        name: &str,
        decls: &Decls,
        keys: &[Phrase<String>],
    ) {
        Self::add_ids(&mut self.decls_mut(presentation).rels, warnings, name, &decls.rels, keys);
    }

    /// Resolves a declared function title in one presentation.
    pub(super) fn func(&self, presentation: Presentation, name: &str) -> Option<String> {
        self.decls(presentation)
            .funcs
            .contains(name)
            .then(|| format!("function_{}_{name}", presentation.name()))
    }

    /// Resolves a declared relation title in one presentation.
    pub(super) fn rel(&self, presentation: Presentation, name: &str) -> Option<String> {
        self.decls(presentation)
            .rels
            .contains(name)
            .then(|| format!("relation_{}_{name}", presentation.name()))
    }
}
