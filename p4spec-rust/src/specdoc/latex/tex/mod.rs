//! Semantic TeX documents and their interpretation
//!
//! ```text
//! Doc::of_def            EL def -> Doc      soft breaks still undecided
//! layout::resolve        Doc    -> Doc      breaks fixed at a line width
//! serialize::to_string   Doc    -> String   TeX text
//! ```
//!
//! `width` measures documents for layout; `link` assigns anchor ownership.

pub(crate) mod doc;
pub(crate) mod layout;
pub(crate) mod link;
pub(crate) mod serialize;
pub(crate) mod width;
