//! Public entry points for wrapper-free EL LaTeX
//!
//! Each call renders the borrowed definitions through `Doc::of_def` and `Doc::of_defs`,
//! then serializes the resulting document with its selected layout.

use crate::lang::el::ast::Def;

use super::{
    error::Result,
    tex::{doc::Doc, serialize},
};

/// Resolves references to anchors owned by the surrounding document.
pub struct Anchors<'a> {
    /// Resolves a function identifier to an anchor, without the leading `#`.
    pub func: &'a dyn Fn(&str) -> Option<String>,
    /// Resolves a relation identifier to an anchor, without the leading `#`.
    pub rel: &'a dyn Fn(&str) -> Option<String>,
}

/// Renders one definition without a math-mode or document wrapper.
pub fn render_def(def: &Def, anchors: Option<&Anchors<'_>>) -> Result<String> {
    let doc = Doc::of_def(def, anchors)?;
    let text = serialize::to_string(&doc);
    Ok(text)
}

/// Renders definitions in source order, aligning consecutive function clauses.
pub fn render_defs(defs: &[Def], anchors: Option<&Anchors<'_>>) -> Result<String> {
    let doc = Doc::of_defs(defs, anchors)?;
    let text = serialize::to_string(&doc);
    Ok(text)
}
