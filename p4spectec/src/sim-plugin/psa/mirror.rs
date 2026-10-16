//! Mirror sessions of the PSA control plane
//!
//! A clone session id maps to a multicast group,
//! so a clone replicates to that group's nodes.

use std::collections::BTreeMap;

/// Mirror table: clone session id to multicast group id.
pub type Table = BTreeMap<usize, usize>;
