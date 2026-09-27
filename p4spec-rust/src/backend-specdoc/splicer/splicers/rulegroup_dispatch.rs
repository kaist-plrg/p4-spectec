//! Relation dispatch prose splices
//!
//! Initialization selects definitions in source order.
//! The generic splicer owns wrappers, anchors, and usage accounting.

use super::super::{
    context::Context,
    error::Error,
    splicer::{Kind, PREFIX_PROSE, SUFFIX_PROSE},
};
use crate::lang::{el::ast as el, pl::ast as pl};

// == Prose splicer

/// Renders prose fragments for this definition kind.
pub(in super::super) struct Prose;

impl<'spec> Kind<'spec> for Prose {
    type Key = String;
    type Value = &'spec pl::DefinedRel;
    const NAME: &'static str = "rulegroup-dispatch-prose";
    const PREFIX: &'static str = PREFIX_PROSE;
    const SUFFIX: &'static str = SUFFIX_PROSE;

    /// Selects the corresponding annotated PL definitions.
    fn init(_spec_el: &'spec el::Spec, spec_pl: &'spec pl::Spec) -> Vec<(Self::Key, Self::Value)> {
        spec_pl
            .iter()
            .filter_map(|def_pl| match &def_pl.node.node {
                pl::DefKind::Rel(pl::RelDef::Defined(rel)) => Some((rel.id.node.clone(), rel)),
                _ => None,
            })
            .collect()
    }

    fn render(ctx: &mut Context<'_>, values: &[&Self::Value]) -> Result<String, Error> {
        Ok(values
            .iter()
            .map(|rel| ctx.renderer.render_defined_rel_def_dispatch(rel))
            .collect::<Vec<_>>()
            .join("\n\n"))
    }
}
