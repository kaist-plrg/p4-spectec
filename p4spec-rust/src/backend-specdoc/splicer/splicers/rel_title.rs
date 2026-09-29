//! Relation title splices
//!
//! Initialization selects definitions in source order.
//! The generic splicer owns wrappers, anchors, and usage accounting.

use super::super::super::anchor::{AnchorContext, Presentation};
use super::super::super::{adoc, latex};
use crate::diagnostic::Report;
use std::collections::BTreeMap;

use super::super::{
    anchor::{Decls, Targets},
    config::{PREFIX_LATEX, PREFIX_SOURCE, SUFFIX_LATEX, SUFFIX_PROSE, SUFFIX_SOURCE},
    error::Error,
    splicer::{Kind, Selection},
};
use crate::lang::common::source::Phrase;
use crate::lang::{el::ast as el, pl::ast as pl};

// == Splice initialization

/// Selects the EL definitions indexed by this marker.
fn init_from_el(spec_el: &el::Spec) -> BTreeMap<String, &el::Def> {
    spec_el
        .iter()
        .filter_map(|def_el| match &def_el.node {
            el::DefKind::ExternRel(def) => Some((def.id.node.clone(), def_el)),
            el::DefKind::Rel(def) => Some((def.id.node.clone(), def_el)),
            _ => None,
        })
        .collect()
}

/// Selects the annotated PL definitions indexed by this marker.
fn init_from_pl(spec_pl: &pl::Spec) -> BTreeMap<String, &pl::Def> {
    spec_pl
        .iter()
        .filter_map(|def_pl| match &def_pl.node.node {
            pl::DefKind::Rel(pl::RelDef::Extern(rel)) => Some((rel.id.node.clone(), def_pl)),
            pl::DefKind::Rel(pl::RelDef::Defined(rel)) => Some((rel.id.node.clone(), def_pl)),
            _ => None,
        })
        .collect()
}

// == Source splicer

/// Renders source fragments for this definition kind.
pub(in super::super) struct Source;

impl<'spec> Kind<'spec> for Source {
    type Key = String;
    type Value = &'spec el::Def;
    const NAME: &'static str = "relation-title-source";
    const PREFIX: &'static str = PREFIX_SOURCE;
    const SUFFIX: &'static str = SUFFIX_SOURCE;

    fn init(
        spec_el: &'spec el::Spec,
        _spec_pl: &'spec pl::Spec,
    ) -> BTreeMap<Self::Key, Self::Value> {
        init_from_el(spec_el)
    }

    fn render(
        _anchor_ctx: &mut AnchorContext<'_>,
        _warnings: &mut Vec<Report>,
        _idx_request: usize,
        values: &[Selection<'_, Self::Key, Self::Value>],
    ) -> Result<String, Error> {
        Ok(values
            .iter()
            .map(|selection| adoc::el::render_def(selection.data))
            .collect::<Vec<_>>()
            .join("\n\n"))
    }
}

// == LaTeX splicer

/// Renders LaTeX fragments for this definition kind.
pub(in super::super) struct Latex;

impl<'spec> Kind<'spec> for Latex {
    type Key = String;
    type Value = &'spec el::Def;
    const NAME: &'static str = "relation-title-latex";
    const PREFIX: &'static str = PREFIX_LATEX;
    const SUFFIX: &'static str = SUFFIX_LATEX;

    fn init(
        spec_el: &'spec el::Spec,
        _spec_pl: &'spec pl::Spec,
    ) -> BTreeMap<Self::Key, Self::Value> {
        init_from_el(spec_el)
    }

    fn render(
        anchor_ctx: &mut AnchorContext<'_>,
        _warnings: &mut Vec<Report>,
        _idx_request: usize,
        values: &[Selection<'_, Self::Key, Self::Value>],
    ) -> Result<String, Error> {
        latex::render_defs(anchor_ctx, values.iter().map(|selection| *selection.data))
    }

    fn anchor(anchor_ctx: &AnchorContext<'_>, name: &str) -> Option<String> {
        anchor_ctx.rel(Presentation::Latex, name)
    }

    fn collect_link_targets(
        targets: &mut Targets,
        warnings: &mut Vec<Report>,
        decls: &Decls,
        keys: &[Phrase<Self::Key>],
    ) {
        targets.add_rels(warnings, Presentation::Latex, Self::NAME, decls, keys);
    }
}

// == Prose splicer

/// Renders prose fragments for this definition kind.
pub(in super::super) struct Prose;

impl<'spec> Kind<'spec> for Prose {
    type Key = String;
    type Value = &'spec pl::Def;
    const NAME: &'static str = "relation-title-prose";
    const PREFIX: &'static str = "[.sidebar-title]\n****\n";
    const SUFFIX: &'static str = SUFFIX_PROSE;

    fn init(
        _spec_el: &'spec el::Spec,
        spec_pl: &'spec pl::Spec,
    ) -> BTreeMap<Self::Key, Self::Value> {
        init_from_pl(spec_pl)
    }

    fn render(
        anchor_ctx: &mut AnchorContext<'_>,
        warnings: &mut Vec<Report>,
        _idx_request: usize,
        values: &[Selection<'_, Self::Key, Self::Value>],
    ) -> Result<String, Error> {
        Ok(values
            .iter()
            .filter_map(|selection| {
                adoc::pl::render_def_title(anchor_ctx, warnings, selection.data)
            })
            .collect::<Vec<_>>()
            .join("\n\n"))
    }

    fn anchor(anchor_ctx: &AnchorContext<'_>, name: &str) -> Option<String> {
        anchor_ctx.rel(Presentation::Prose, name)
    }

    fn collect_link_targets(
        targets: &mut Targets,
        warnings: &mut Vec<Report>,
        decls: &Decls,
        keys: &[Phrase<Self::Key>],
    ) {
        targets.add_rels(warnings, Presentation::Prose, Self::NAME, decls, keys);
    }
}
