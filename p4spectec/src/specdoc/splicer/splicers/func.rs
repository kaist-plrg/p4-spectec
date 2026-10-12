//! Function body splices
//!
//! Initialization selects definitions in source order.
//! The generic splicer owns wrappers, anchors, and usage accounting.

use std::collections::BTreeMap;

use crate::lang::el::ast as el;

use crate::lang::pl::ast as pl;

use crate::diagnostic::Report;

use super::super::super::{adoc, anchor::AnchorContext, latex};

use super::super::{
    config::{
        PREFIX_LATEX, PREFIX_PROSE, PREFIX_SOURCE, SUFFIX_LATEX, SUFFIX_PROSE, SUFFIX_SOURCE,
    },
    error::Error,
    splicer::{Kind, Selection},
};

// == Splice initialization

/// Collects clauses for each function while preserving their source order.
fn init_from_el(spec_el: &el::Spec) -> BTreeMap<String, Vec<&el::Def>> {
    let mut defs: BTreeMap<String, Vec<&el::Def>> = BTreeMap::new();
    // Append clauses to the function selected by their declaration identifier
    for def_el in spec_el {
        if let el::DefKind::FuncDef(def) = &def_el.node {
            defs.entry(def.id.node.to_string())
                .or_default()
                .push(def_el);
        }
    }
    defs
}

/// Selects the annotated PL definitions indexed by this marker.
fn init_from_pl(spec_pl: &pl::Spec) -> BTreeMap<String, &pl::Def> {
    spec_pl
        .iter()
        .filter_map(|def_pl| match &def_pl.node.node {
            pl::DefKind::MetaFunc(pl::MetaFuncDef::Defined(func)) => {
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
    type Value = Vec<&'spec el::Def>;
    const NAME: &'static str = "func-source";
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
            .flat_map(|selection| selection.data.iter().copied())
            .map(adoc::el::render_def)
            .collect::<Vec<_>>()
            .join("\n\n"))
    }
}

// == LaTeX splicer

/// Renders LaTeX fragments for this definition kind.
pub(in super::super) struct Latex;

impl<'spec> Kind<'spec> for Latex {
    type Key = String;
    type Value = Vec<&'spec el::Def>;
    const NAME: &'static str = "func-latex";
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
        latex::render_defs(
            anchor_ctx,
            values
                .iter()
                .flat_map(|selection| selection.data.iter().copied()),
        )
    }
}

// == Prose splicer

/// Renders prose fragments for this definition kind.
pub(in super::super) struct Prose;

impl<'spec> Kind<'spec> for Prose {
    type Key = String;
    type Value = &'spec pl::Def;
    const NAME: &'static str = "func-prose";
    const PREFIX: &'static str = PREFIX_PROSE;
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
