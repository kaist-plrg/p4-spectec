//! Public entry points for wrapper-free EL LaTeX
//!
//! Each call renders the borrowed definitions through `Doc::of_def` and `Doc::of_defs`,
//! then serializes the resulting document with its selected layout.
//! The anchor context supplies optional destinations for function and relation links.

use crate::lang::el::ast::Def;

use super::super::anchor::AnchorContext;

use super::{
    error::Result,
    tex::{doc::Doc, serialize},
};

/// Renders one definition without a math-mode or document wrapper.
pub fn render_def(anchor_ctx: &AnchorContext<'_>, def: &Def) -> Result<String> {
    let doc = Doc::of_def(anchor_ctx, def)?;
    let text = serialize::to_string(&doc);
    Ok(text)
}

/// Renders definitions in source order, aligning consecutive function clauses.
pub fn render_defs<'a>(
    anchor_ctx: &AnchorContext<'_>,
    defs: impl IntoIterator<Item = &'a Def>,
) -> Result<String> {
    let defs: Vec<_> = defs.into_iter().collect();
    let doc = Doc::of_defs(anchor_ctx, &defs)?;
    let text = serialize::to_string(&doc);
    Ok(text)
}
