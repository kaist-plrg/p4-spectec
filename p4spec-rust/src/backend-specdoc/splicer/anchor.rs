//! Declared function and relation targets in skeleton documents
//!
//! Target collection precedes rendering across every input file.
//! Only title markers for declared EL identifiers create reference destinations.

use super::{
    error::{Error, warn},
    parser,
    source::Source,
};
use crate::{
    diagnostic::Report,
    lang::{common::source::Span, el::ast as el},
};
use std::collections::BTreeSet;

// == Anchor targets

#[derive(Clone, Copy)]
/// Selects the reference namespace of a title.
pub(super) enum Presentation {
    Prose,
    Latex,
}

#[derive(Default)]
struct Decls {
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

/// Collects declarations that may own a title marker.
fn collect_decls(spec_el: &el::Spec) -> Decls {
    let mut decls = Decls::default();
    // Collect title-bearing declarations in source order
    for def in spec_el {
        match &def.node {
            // Relation declarations supply relation title targets
            el::DefKind::ExternRel(rel) => {
                decls.rels.insert(rel.id.node.clone());
            }
            // Defined relations use the same reference namespace
            el::DefKind::Rel(rel) => {
                decls.rels.insert(rel.id.node.clone());
            }
            // Function declarations supply function title targets
            el::DefKind::ExternDec(func) => {
                decls.funcs.insert(func.id.node.clone());
            }
            // Builtin functions may also receive document titles
            el::DefKind::BuiltinDec(func) => {
                decls.funcs.insert(func.id.node.clone());
            }
            // Defined functions are indexed by their declaration
            el::DefKind::FuncDec(func) => {
                decls.funcs.insert(func.id.node.clone());
            }
            // Other definitions do not declare title targets
            _ => {}
        }
    }
    decls
}

// == Skeleton targets

/// Adds declared targets and warns about duplicate title occurrences.
fn add_ids(
    name: &str,
    ids_declared: &BTreeSet<String>,
    ids: Vec<String>,
    ids_target: &mut BTreeSet<String>,
    warnings: &mut Vec<Report>,
) {
    // Ignore undeclared identifiers, retaining duplicate-target diagnostics
    for id in ids {
        if ids_declared.contains(&id) && !ids_target.insert(id.clone()) {
            warn(warnings, &Span::default(), format!("duplicate {name} target: {id}"));
        }
    }
}

/// Scans a skeleton for the four title marker kinds.
fn collect_targets(
    decls: &Decls,
    targets: &mut Targets,
    source: &mut Source<'_>,
    warnings: &mut Vec<Report>,
) -> Result<(), Error> {
    // Match the OCaml prepass order independently of rendering order
    while !source.eos() {
        let mut parsed = false;
        for (name, ids_declared, ids_target) in [
            ("func-title-prose", &decls.funcs, &mut targets.prose.funcs),
            ("func-title-latex", &decls.funcs, &mut targets.latex.funcs),
            ("relation-title-prose", &decls.rels, &mut targets.prose.rels),
            ("relation-title-latex", &decls.rels, &mut targets.latex.rels),
        ] {
            // Parse only recognized title markers
            if parser::parse_splice_start(source, name) {
                let ids = parser::parse_ids(source)?;
                add_ids(name, ids_declared, ids, ids_target, warnings);
                parsed = true;
                break;
            }
        }
        // Advance through text and other markers unchanged
        if !parsed {
            source.adv();
        }
    }
    Ok(())
}

// == Renderer lookups

impl Presentation {
    fn name(self) -> &'static str {
        match self {
            Self::Prose => "prose",
            Self::Latex => "latex",
        }
    }
}

impl Targets {
    fn decls(&self, presentation: Presentation) -> &Decls {
        match presentation {
            Presentation::Prose => &self.prose,
            Presentation::Latex => &self.latex,
        }
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

// == Entry point

/// Collects targets across all skeletons before rendering starts.
pub(super) fn collect(
    spec_el: &el::Spec,
    sources: &[(&str, &str)],
    warnings: &mut Vec<Report>,
) -> Result<Targets, Error> {
    let decls = collect_decls(spec_el);
    let mut targets = Targets::default();
    // Resolve forward references across the whole input batch
    for (file, text) in sources {
        collect_targets(&decls, &mut targets, &mut Source::new(file, text), warnings)?;
    }
    Ok(targets)
}
