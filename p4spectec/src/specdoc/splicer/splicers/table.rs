//! Table splices
//!
//! Initialization selects definitions in source order.
//! The generic splicer owns wrappers, anchors, and usage accounting.

use std::collections::BTreeMap;

use crate::lang::el::ast as el;

use crate::lang::pl::ast as pl;

use crate::diagnostic::Report;

use super::super::super::{adoc, anchor::AnchorContext, latex};

use super::super::{
    config::{PREFIX_LATEX, SUFFIX_LATEX},
    error::Error,
    splicer::{Kind, Selection},
};

// == Splice initialization

/// Selects the EL definitions indexed by this marker.
fn init_from_el(spec_el: &el::Spec) -> BTreeMap<String, &el::Def> {
    spec_el
        .iter()
        .filter_map(|def_el| match &def_el.node {
            el::DefKind::TableDef(def) => Some((def.id.node.to_string(), def_el)),
            _ => None,
        })
        .collect()
}

/// Selects the annotated PL definitions indexed by this marker.
fn init_from_pl(spec_pl: &pl::Spec) -> BTreeMap<String, &pl::Def> {
    spec_pl
        .iter()
        .filter_map(|def_pl| match &def_pl.node.node {
            pl::DefKind::MetaFunc(pl::MetaFuncDef::Table(func)) => {
                Some((func.id.node.to_string(), def_pl))
            }
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
    const NAME: &'static str = "table-source";
    const PREFIX: &'static str = "";
    const SUFFIX: &'static str = "\n";

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
    const NAME: &'static str = "table-latex";
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
}

// == Prose splicer

/// Renders prose fragments for this definition kind.
pub(in super::super) struct Prose;

impl<'spec> Kind<'spec> for Prose {
    type Key = String;
    type Value = &'spec pl::Def;
    const NAME: &'static str = "table-prose";
    const PREFIX: &'static str = "";
    const SUFFIX: &'static str = "\n";

    fn init(
        _spec_el: &'spec el::Spec,
        spec_pl: &'spec pl::Spec,
    ) -> BTreeMap<Self::Key, Self::Value> {
        init_from_pl(spec_pl)
    }

    fn render(
        anchor_ctx: &mut AnchorContext<'_>,
        warnings: &mut Vec<Report>,
        idx_request: usize,
        values: &[Selection<'_, Self::Key, Self::Value>],
    ) -> Result<String, Error> {
        Ok(values
            .iter()
            .filter_map(|selection| {
                let anchor_prefix = format!(
                    "{}:{}:{idx_request}:{}",
                    Self::NAME,
                    selection.key,
                    selection.idx_key,
                );
                adoc::pl::render_def(anchor_ctx, warnings, &anchor_prefix, selection.data)
            })
            .collect::<Vec<_>>()
            .join("\n\n"))
    }
}
