//! Relation rule-group splices
//!
//! Initialization selects definitions in source order.
//! The generic splicer owns wrappers, anchors, and usage accounting.

use std::collections::BTreeMap;

use crate::lang::el::ast as el;

use crate::lang::pl::{ast as pl, rule_group};

use crate::diagnostic::Report;

use super::super::super::{
    adoc::{
        self,
        pl::{Renderer, fallthrough},
    },
    anchor::AnchorContext,
    latex,
};

use super::super::{
    config::{
        PREFIX_LATEX, PREFIX_PROSE, PREFIX_SOURCE, SUFFIX_LATEX, SUFFIX_PROSE, SUFFIX_SOURCE,
    },
    error::Error,
    parser, source,
    splicer::{Key, Kind, Selection},
};

// == Splice initialization

/// Selects the EL definitions indexed by this marker.
fn init_from_el(spec_el: &el::Spec) -> BTreeMap<(String, String), &el::Def> {
    spec_el
        .iter()
        .filter_map(|def_el| match &def_el.node {
            el::DefKind::RuleGroup(def) => {
                Some(((def.relid.node.to_string(), def.groupid.node.to_string()), def_el))
            }
            _ => None,
        })
        .collect()
}

/// Collects groups from each relation's main dispatch tree.
fn init_from_pl(spec_pl: &pl::Spec) -> BTreeMap<(String, String), rule_group::RuleGroup<'_>> {
    let mut groups = BTreeMap::new();
    // Otherwise groups have their own marker and stay outside this store
    for def_pl in spec_pl {
        if let pl::DefKind::Rel(pl::RelDef::Defined(rel)) = &def_pl.node.node {
            for group in rule_group::collect_rule_groups(&rel.block) {
                groups.insert((rel.id.node.to_string(), group.id_group.node.to_string()), group);
            }
        }
    }
    groups
}

// == Splice key

impl Key for (String, String) {
    fn to_string(&self) -> String {
        format!("{}/{}", self.0, self.1)
    }

    fn to_anchor(&self) -> String {
        fallthrough::anchor_of_group(&self.0, &self.1)
    }

    fn parse(
        source: &mut source::Source<'_>,
    ) -> Result<Vec<crate::lang::common::source::Phrase<Self>>, Error> {
        Ok(vec![parser::parse_id_with_sub(source)?])
    }
}

// == Source splicer

/// Renders source fragments for this definition kind.
pub(in super::super) struct Source;

impl<'spec> Kind<'spec> for Source {
    type Key = (String, String);
    type Value = &'spec el::Def;
    const NAME: &'static str = "rulegroup-source";
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
    type Key = (String, String);
    type Value = &'spec el::Def;
    const NAME: &'static str = "rulegroup-latex";
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
    type Key = (String, String);
    type Value = rule_group::RuleGroup<'spec>;
    const NAME: &'static str = "rulegroup-prose";
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
            .map(|selection| {
                let group = selection.data;
                let anchor_prefix = format!(
                    "{}:{}:{idx_request}:{}",
                    Self::NAME,
                    selection.key.to_string(),
                    selection.idx_key,
                );
                let mut renderer = Renderer::new(anchor_ctx, warnings, &anchor_prefix);
                renderer.render_rulegroup(
                    group.hints,
                    group.id_rel,
                    group.rel_signature,
                    group.exps_input,
                    group.block,
                )
            })
            .collect::<Vec<_>>()
            .join("\n\n"))
    }

    fn anchor(_anchor_ctx: &AnchorContext<'_>, name: &str) -> Option<String> {
        Some(name.to_owned())
    }
}
