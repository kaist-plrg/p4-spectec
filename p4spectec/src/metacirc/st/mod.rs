//! The St backend: specifications written in P4-SpecTec, run natively
//!
//! `EngineSt` executes the base on the AL or SL interpreter.
//! `ProtocolStSt` is spec-meta's protocol, interpreting the level above as AL;
//! `ProtocolStStExt` is spec-meta-ext's, interpreting it as AL or IL;
//! `ProtocolKSt` is spec-k's, interpreting the K definition above (K-in-ST).

mod engine;
mod protocol_k;
mod protocol_st;
mod protocol_st_ext;

pub(super) use engine::{EngineSt, SpecSt, StateSt};
pub(super) use protocol_k::ProtocolKSt;
pub(super) use protocol_st::ProtocolStSt;
pub(super) use protocol_st_ext::ProtocolStStExt;
