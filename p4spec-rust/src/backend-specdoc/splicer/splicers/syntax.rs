//! Syntax source splices
//!
//! Initialization selects definitions in source order.
//! The generic splicer owns wrappers, anchors, and usage accounting.

use super::super::{context::Context, error::Error, splicer::Kind};
use crate::lang::{el::ast as el, pl::ast as pl};

// == Splice initialization

/// Selects the EL definitions indexed by this marker.
fn init(spec_el: &el::Spec) -> Vec<(String, &el::Def)> {
    spec_el
        .iter()
        .filter_map(|def_el| match &def_el.node {
            el::DefKind::ExternSyntax(def) => Some((def.id.node.clone(), def_el)),
            el::DefKind::Typ(def) => Some((def.id.node.clone(), def_el)),
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
    const NAME: &'static str = "syntax";
    const PREFIX: &'static str = "[source,bison]\n----\n";
    const SUFFIX: &'static str = "\n----";

    fn init(spec_el: &'spec el::Spec, _spec_pl: &'spec pl::Spec) -> Vec<(Self::Key, Self::Value)> {
        init(spec_el)
    }

    fn render(_ctx: &mut Context<'_>, values: &[&Self::Value]) -> Result<String, Error> {
        Ok(super::render_source(values.iter().copied().copied()))
    }

    fn anchor(_ctx: &Context<'_>, name: &str) -> Option<String> {
        Some(name.to_owned())
    }
}
