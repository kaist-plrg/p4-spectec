//! Marker-specific indexing and rendering
//!
//! Each kind supplies the data and wrappers used by the generic splicer.
//! EL rendering preserves key and clause order; each PL body has its own renderer.

pub(super) mod func;
pub(super) mod func_title;
pub(super) mod rel_title;
pub(super) mod rule_group;
pub(super) mod rule_group_dispatch;
pub(super) mod rule_group_else;
pub(super) mod syntax;
pub(super) mod table;
