//! Table splices
//!
//! Initialization selects definitions in source order.
//! The generic splicer owns wrappers, anchors, and usage accounting.

use super::super::{
    context::Context,
    error::Error,
    splicer::{Kind, PREFIX_LATEX, SUFFIX_LATEX},
};
use crate::lang::{el::ast as el, pl::ast as pl};

// == Splice initialization

/// Selects the EL definitions indexed by this marker.
fn init(spec_el: &el::Spec) -> Vec<(String, &el::Def)> {
    spec_el
        .iter()
        .filter_map(|def_el| match &def_el.node {
            el::DefKind::TableDef(def) => Some((def.id.node.clone(), def_el)),
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
    type Key = String;
    type Value = &'spec el::Def;
    const NAME: &'static str = "table-latex";
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
    type Key = String;
    type Value = &'spec pl::Def;
    const NAME: &'static str = "table-prose";
    const PREFIX: &'static str = "";
    const SUFFIX: &'static str = "\n";

    /// Selects the corresponding annotated PL definitions.
    fn init(_spec_el: &'spec el::Spec, spec_pl: &'spec pl::Spec) -> Vec<(Self::Key, Self::Value)> {
        spec_pl
            .iter()
            .filter_map(|def_pl| match &def_pl.node.node {
                pl::DefKind::MetaFunc(pl::MetaFuncDef::Table(func)) => {
                    Some((func.id.node.clone(), def_pl))
                }
                _ => None,
            })
            .collect()
    }

    fn render(ctx: &mut Context<'_>, values: &[&Self::Value]) -> Result<String, Error> {
        Ok(super::render_prose(ctx, values.iter().copied().copied()))
    }
}
