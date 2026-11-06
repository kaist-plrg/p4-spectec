//! The Redex backend: spec-meta-redex, the AL semantics written in PLT Redex (ST-in-Redex)
//!
//! `EngineRedex` runs the base in one `racket` process per tower (`runtime`);
//! `ProtocolStRedex` interprets the level above as AL, in the s-expressions of
//! `serve`: the Redex terms that spec-meta-redex reads and writes as they are,
//! printed by `sexp` and read back by `read`.

mod cache;
mod engine;
mod protocol;
mod read;
mod runtime;
mod serve;
mod sexp;

pub(super) use engine::{EngineRedex, StateRedex};
pub(super) use protocol::ProtocolStRedex;
pub(super) use runtime::RuntimeRedex;
