//! Mirror sessions of the v1model control plane
//!
//! A session id names the port a clone is sent to;
//! a clone through an unconfigured session is not made.

/// Mirror session ids mapped to output ports.
pub type Table = std::collections::BTreeMap<usize, usize>;
