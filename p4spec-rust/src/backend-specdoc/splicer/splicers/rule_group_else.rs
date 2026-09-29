//! Relation else prose splices
//!
//! Initialization selects definitions in source order.
//! The generic splicer owns wrappers, anchors, and usage accounting.

use super::super::super::anchor::AnchorContext;
use std::collections::BTreeMap;

use super::super::super::adoc::pl::Renderer;

use super::super::{
    config::{PREFIX_PROSE, SUFFIX_PROSE},
    error::Error,
    splicer::{Kind, Selection},
};
use crate::lang::{el::ast as el, pl::ast as pl};

// == Splice initialization

/// Selects relations with a nonempty otherwise block.
fn init_from_pl(spec_pl: &pl::Spec) -> BTreeMap<String, (&pl::Id, &pl::DispatchBlock)> {
    spec_pl
        .iter()
        .filter_map(|def_pl| match &def_pl.node.node {
            pl::DefKind::Rel(pl::RelDef::Defined(rel)) => rel
                .block_else_opt
                .as_ref()
                .filter(|block| !block.is_empty())
                .map(|block| (rel.id.node.clone(), (&rel.id, block))),
            _ => None,
        })
        .collect()
}

// == Prose splicer

/// Renders prose fragments for this definition kind.
pub(in super::super) struct Prose;

impl<'spec> Kind<'spec> for Prose {
    type Key = String;
    type Value = (&'spec pl::Id, &'spec pl::DispatchBlock);
    const NAME: &'static str = "rulegroup-prose-else";
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
        idx_request: usize,
        values: &[Selection<'_, Self::Key, Self::Value>],
    ) -> Result<String, Error> {
        Ok(values
            .iter()
            .map(|selection| {
                let (id_rel, block_else) = selection.data;
                let anchor_prefix = format!(
                    "{}:{}:{idx_request}:{}",
                    Self::NAME,
                    selection.key,
                    selection.idx_key,
                );
                let mut renderer = Renderer::new(anchor_ctx, &anchor_prefix);
                renderer.render_rulegroup_else(id_rel, block_else)
            })
            .collect::<Vec<_>>()
            .join("\n\n"))
    }
}
