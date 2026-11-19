//! AsciiDoc prose documents and their serialization
//!
//! ```text
//! render::Renderer        PL def -> Block    links and markers still unresolved
//! serialize::ser_block    Block  -> String   AsciiDoc text
//! ```
//!
//! `caps` capitalizes leading prose; `width` measures prose for layout.

mod caps;
pub mod doc;
pub mod serialize;
mod width;
