//! Canonical LaTeX rendering for EL definitions
//!
//! `render_def` and `render_defs` build semantic TeX documents,
//! resolve definition layouts, and serialize without display wrappers.
//! Optional `Anchors` resolve function and relation references;
//! the caller owns the surrounding document and its anchor declarations.

mod error;
mod precedence;
mod render;
mod renderer;
mod tex;

pub use error::Error;
pub use render::{Anchors, render_def, render_defs};
