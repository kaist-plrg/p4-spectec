//! AsciiDoc rendering for prose-language definitions

pub mod doc;
pub mod fallthrough;
mod render;

pub use render::{Renderer, render_def, render_spec};
