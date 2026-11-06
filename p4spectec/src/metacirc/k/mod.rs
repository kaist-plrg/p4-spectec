//! The K backend: spec-meta-k, the AL semantics written in K (ST-in-K)
//!
//! `EngineK` runs the base from the library `kompile` made of spec-meta-k,
//! on a K thread per depth (`runtime`, `ffi`);
//! `ProtocolStK` interprets the level above as AL, in the JSON of `json`,
//! `serve`, and `spec_json`, which spec-meta-k reads.

mod engine;
mod ffi;
mod json;
mod protocol;
mod runtime;
mod serve;
mod spec_json;

pub(super) use engine::{EngineK, StateK};
pub(super) use protocol::ProtocolStK;
pub(super) use runtime::RuntimeK;
