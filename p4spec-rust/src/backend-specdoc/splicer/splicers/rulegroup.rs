//! Relation rule-group splices
//!
//! Initialization selects definitions in source order.
//! The generic splicer owns wrappers, anchors, and usage accounting.

use super::super::{
    context::Context,
    error::Error,
    splicer::{
        Kind, PREFIX_LATEX, PREFIX_PROSE, PREFIX_SOURCE, SUFFIX_LATEX, SUFFIX_PROSE, SUFFIX_SOURCE,
    },
};
use crate::lang::{el::ast as el, pl::ast as pl};

// == Splice initialization

/// Selects the EL definitions indexed by this marker.
fn init(spec_el: &el::Spec) -> Vec<((String, String), &el::Def)> {
    spec_el
        .iter()
        .filter_map(|def_el| match &def_el.node {
            el::DefKind::RuleGroup(def) => {
                Some(((def.relid.node.clone(), def.groupid.node.clone()), def_el))
            }
            _ => None,
        })
        .collect()
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
    fn init(spec_el: &'spec el::Spec, _spec_pl: &'spec pl::Spec) -> Vec<(Self::Key, Self::Value)> {
        init(spec_el)
    }

    fn render(_ctx: &mut Context<'_>, values: &[&Self::Value]) -> Result<String, Error> {
        Ok(super::render_source(values.iter().copied().copied()))
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
    fn init(spec_el: &'spec el::Spec, _spec_pl: &'spec pl::Spec) -> Vec<(Self::Key, Self::Value)> {
        init(spec_el)
    }

    fn render(ctx: &mut Context<'_>, values: &[&Self::Value]) -> Result<String, Error> {
        super::render_latex(ctx, values.iter().copied().copied())
    }
}

// == Prose splicer

/// Renders prose fragments for this definition kind.
pub(in super::super) struct Prose;

impl<'spec> Kind<'spec> for Prose {
    type Key = (String, String);
    type Value = crate::lang::pl::rule_group::RuleGroup<'spec>;
    const NAME: &'static str = "rulegroup-prose";
    const PREFIX: &'static str = PREFIX_PROSE;
    const SUFFIX: &'static str = SUFFIX_PROSE;
    /// Collects groups from the relation's main dispatch tree.
    /// Selects the corresponding annotated PL definitions.
    fn init(_spec_el: &'spec el::Spec, spec_pl: &'spec pl::Spec) -> Vec<(Self::Key, Self::Value)> {
        let mut pairs = Vec::new();
        // Otherwise groups have their own marker and stay outside this store
        for def_pl in spec_pl {
            if let pl::DefKind::Rel(pl::RelDef::Defined(rel)) = &def_pl.node.node {
                for group in crate::lang::pl::rule_group::collect_rule_groups(&rel.block) {
                    pairs.push(((rel.id.node.clone(), group.id_group.node.clone()), group));
                }
            }
        }
        pairs
    }

    fn render(ctx: &mut Context<'_>, values: &[&Self::Value]) -> Result<String, Error> {
        Ok(values
            .iter()
            .map(|group| {
                ctx.renderer.render_rulegroup(
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

    fn anchor(_ctx: &Context<'_>, name: &str) -> Option<String> {
        Some(name.to_owned())
    }
}
