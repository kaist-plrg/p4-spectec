//! Interpreters a tower lends to a host for an extern's reentry
//!
//! A host extern call runs in a context around a lent interpreter,
//! which answers if the extern calls back into a relation, function,
//! or program entry; `meta` lends the P4 level's interpreter
//! at one depth of spec-meta.

mod meta;

pub(super) use meta::{MetaDepth, MetaEnv, MetaInterp, Native};
