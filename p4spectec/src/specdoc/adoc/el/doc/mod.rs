//! Width-sensitive AsciiDoc documents and their layout
//!
//! ```text
//! Doc::of_def    EL def -> Doc      breaks still undecided
//! Doc::render    Doc    -> String   breaks fixed at a line width
//! ```

pub(crate) mod doc;
pub(crate) mod layout;
