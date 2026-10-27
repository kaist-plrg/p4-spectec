//! Interpreters a tower lends to a host for an extern's reentry
//!
//! A host extern call runs in a context around a lent interpreter,
//! which answers if the extern calls back into a relation, function,
//! or program entry; `none` lends one that fails,
//! and `meta` lends the P4 level's interpreter at one depth of spec-meta.

mod meta;
mod none;

pub(super) use meta::{MetaDepth, MetaEnv, MetaInterp};
pub(super) use none::NoReentry;
