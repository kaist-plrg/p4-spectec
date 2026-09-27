//! Marker-specific indexing and rendering
//!
//! Each kind supplies the data and wrappers used by the generic splicer.
//! EL rendering preserves key and clause order; PL rendering shares one renderer.

pub(super) mod func;
pub(super) mod func_title;
pub(super) mod rel_title;
pub(super) mod rulegroup;
pub(super) mod rulegroup_dispatch;
pub(super) mod rulegroup_else;
pub(super) mod syntax;
pub(super) mod table;

use super::super::{adoc, latex};
use super::{anchor::Presentation, context::Context, error::Error};
use crate::lang::{el::ast as el, pl::ast as pl};

/// Renders source definitions with the OCaml fragment separators.
fn render_source<'a>(defs: impl Iterator<Item = &'a el::Def>) -> String {
    defs.map(adoc::el::render_def)
        .collect::<Vec<_>>()
        .join("\n\n")
}

/// Renders adjacent EL clauses together for shared LaTeX alignment.
fn render_latex<'a>(
    ctx: &Context<'_>,
    defs: impl Iterator<Item = &'a el::Def>,
) -> Result<String, Error> {
    let func = |name: &str| ctx.targets.func(Presentation::Latex, name);
    let rel = |name: &str| ctx.targets.rel(Presentation::Latex, name);
    let anchors = latex::Anchors { func: &func, rel: &rel };
    let defs: Vec<_> = defs.cloned().collect();
    Ok(latex::render_defs(&defs, Some(&anchors))?)
}

/// Renders PL definitions with counters shared by the whole splice run.
fn render_prose<'a>(ctx: &mut Context<'_>, defs: impl Iterator<Item = &'a pl::Def>) -> String {
    defs.filter_map(|def| ctx.renderer.render_def(def))
        .collect::<Vec<_>>()
        .join("\n\n")
}

/// Renders declaration titles without their defined bodies.
fn render_titles<'a>(ctx: &mut Context<'_>, defs: impl Iterator<Item = &'a pl::Def>) -> String {
    defs.filter_map(|def| ctx.renderer.render_title(def))
        .collect::<Vec<_>>()
        .join("\n\n")
}
