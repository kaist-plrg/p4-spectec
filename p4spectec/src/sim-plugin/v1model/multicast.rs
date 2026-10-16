//! Multicast groups of the v1model control plane
//!
//! A group is an ordered list of node handles;
//! each node replicates the packet to one port with one replication id.

use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
/// One replica: an output port and its replication id.
pub struct Node {
    /// Output port of the replica.
    pub port: usize,
    /// Replication id written to `egress_rid`.
    pub rid: usize,
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
/// Multicast groups, nodes, and the handle allocator.
pub struct State {
    /// Next unused node handle.
    pub handle_next: usize,
    /// Multicast group id to ordered node handles.
    pub groups: BTreeMap<usize, Vec<usize>>,
    /// Node handle to ordered multicast nodes.
    pub nodes: BTreeMap<usize, Vec<Node>>,
}

impl State {
    /// Creates an empty group, replacing any existing one.
    pub fn group_create(&mut self, group: usize) {
        self.groups.insert(group, vec![]);
    }

    /// Allocates a handle for a node replicating to `ports` with id `rid`.
    pub fn node_create(&mut self, rid: usize, ports: &[usize]) {
        // Handles are allocated in creation order
        let handle = self.handle_next;
        self.handle_next = handle + 1;
        self.nodes
            .insert(handle, ports.iter().map(|port| Node { port: *port, rid }).collect());
    }

    /// Prepends `handle` to `group`; an unknown group is ignored.
    pub fn node_associate(&mut self, group: usize, handle: usize) {
        if let Some(handles) = self.groups.get_mut(&group) {
            handles.insert(0, handle);
        }
    }
}
