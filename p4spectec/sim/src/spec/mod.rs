//! The one place the simulator meets the specification
//!
//! Every specification value the rest of the simulator holds is a `SpecValue`,
//! which only this module can open or build,
//! so a change in the specification's syntax or relations lands here.
//! `entry` is how the specification calls into the simulator:
//! the `Extern` impls, which read each extern call it hands over;
//! `host` is the runner as the simulator calls it;
//! `ops` reads and writes P4 values and variables for `p4::P4Ops` and `p4::P4Vars`;
//! `pipe` runs an architecture's relations and holds its state for `p4::Sim`;
//! `table` finds tables and applies control-plane entries to them.
//! `call` calls specification definitions by name,
//! and `bridge` converts Rust values to and from the specification's;
//! both stay private to this module.

use p4spectec::lang::data::value::flat::Value;

// == Calls into the simulator

mod entry;

// == Calls into the specification

pub(crate) mod host;
pub(crate) mod ops;
pub(crate) mod pipe;
pub(crate) mod table;

mod call;

// == Values

mod bridge;

/// A specification value the simulator holds without looking inside,
/// such as an evaluation context or a parsed program.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SpecValue(pub(in crate::spec) Value);
