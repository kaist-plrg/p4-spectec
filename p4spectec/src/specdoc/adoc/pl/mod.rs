//! AsciiDoc rendering for prose-language definitions

pub mod doc;
mod render;

pub use render::{Renderer, render_def, render_def_title, render_spec};
