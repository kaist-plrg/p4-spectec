//! Binding analysis for algorithmic conversion
//!
//! `transform` drives the pass over definitions;
//! `collect` finds binding occurrences,
//! `multiple` and `partial` rewrite repeated and partially bound patterns,
//! `antiunify` merges rule inputs into one template,
//! `iteration` and `dimension` track iteration scopes,
//! `pattern` and `shallow` check table rows,
//! and `context` and `bind` hold the environments.

pub(in crate::pass::algo) mod antiunify;
pub(in crate::pass::algo) mod bind;
pub(in crate::pass::algo) mod collect;
pub(in crate::pass::algo) mod context;
pub(in crate::pass::algo) mod dimension;
pub(in crate::pass::algo) mod iteration;
pub(in crate::pass::algo) mod multiple;
pub(in crate::pass::algo) mod partial;
pub(in crate::pass::algo) mod pattern;
pub(in crate::pass::algo) mod shallow;
pub(in crate::pass::algo) mod transform;
