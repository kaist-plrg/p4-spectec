//! Syntax source splices
//!
//! Initialization selects definitions in source order.
//! The generic splicer owns wrappers, anchors, and usage accounting.

use std::collections::BTreeMap;

use crate::lang::el::ast as el;

use crate::lang::pl::ast as pl;

use crate::diagnostic::Report;

use super::super::super::{adoc, anchor::AnchorContext};

use super::super::{
    error::Error,
    splicer::{Kind, Selection},
};

// == Splice initialization

/// Selects the EL definitions indexed by this marker.
fn init_from_el(spec_el: &el::Spec) -> BTreeMap<String, &el::Def> {
    spec_el
        .iter()
        .filter_map(|def_el| match &def_el.node {
            el::DefKind::ExternSyntax(def) => Some((def.id.node.to_string(), def_el)),
            el::DefKind::Typ(def) => Some((def.id.node.to_string(), def_el)),
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

    fn anchor(_anchor_ctx: &AnchorContext<'_>, name: &str) -> Option<String> {
        Some(name.to_owned())
    }
}
