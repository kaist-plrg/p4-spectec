//! The Redex backend: spec-meta-redex, the AL semantics written in PLT Redex (ST-in-Redex)
//!
//! `EngineRedex` runs the base in one `racket` process per tower (`runtime`);
//! `ProtocolStRedex` interprets the level above as AL, in the JSON of `json`
//! and `serve` with specifications as s-expressions (`sexp`),
//! which spec-meta-redex reads.

mod engine;
mod json;
mod protocol;
mod runtime;
mod serve;
mod sexp;

pub(super) use engine::{EngineRedex, StateRedex};
pub(super) use json::Cache as CacheStRedex;
pub(super) use protocol::ProtocolStRedex;
pub(super) use runtime::RuntimeRedex;
pub(super) use serve::is_ok;
