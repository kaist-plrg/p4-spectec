//! Function body splices
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

/// Collects clauses for each function while preserving their source order.
fn init(spec_el: &el::Spec) -> Vec<(String, Vec<&el::Def>)> {
    let mut defs: std::collections::BTreeMap<String, Vec<&el::Def>> =
        std::collections::BTreeMap::new();
    // Append clauses to the function selected by their declaration identifier
    for def_el in spec_el {
        if let el::DefKind::FuncDef(def) = &def_el.node {
            defs.entry(def.id.node.clone()).or_default().push(def_el);
        }
    }
    defs.into_iter().collect()
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

    fn init(spec_el: &'spec el::Spec, _spec_pl: &'spec pl::Spec) -> Vec<(Self::Key, Self::Value)> {
        init(spec_el)
    }

    fn render(_ctx: &mut Context<'_>, values: &[&Self::Value]) -> Result<String, Error> {
        Ok(super::render_source(values.iter().flat_map(|defs| defs.iter().copied())))
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

    fn init(spec_el: &'spec el::Spec, _spec_pl: &'spec pl::Spec) -> Vec<(Self::Key, Self::Value)> {
        init(spec_el)
    }

    fn render(ctx: &mut Context<'_>, values: &[&Self::Value]) -> Result<String, Error> {
        super::render_latex(ctx, values.iter().flat_map(|defs| defs.iter().copied()))
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

    /// Selects the corresponding annotated PL definitions.
    fn init(_spec_el: &'spec el::Spec, spec_pl: &'spec pl::Spec) -> Vec<(Self::Key, Self::Value)> {
        spec_pl
            .iter()
            .filter_map(|def_pl| match &def_pl.node.node {
                pl::DefKind::MetaFunc(pl::MetaFuncDef::Defined(func)) => {
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
