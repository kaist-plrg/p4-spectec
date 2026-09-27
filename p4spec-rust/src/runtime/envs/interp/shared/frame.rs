//! Per-callable variable slots and copy-on-write value frames
//!
//! Layouts resolve names and iterator paths during preparation.
//! Execution uses slots;
//! a reserved slot stays unbound until an assignment writes its value.
//! Frames share their fixed-size value slice until one is written.

use std::{collections::HashMap, rc::Rc};

use crate::lang::{
    common::{Id, Iter},
    data::{
        value::Value,
        var::{IdSlot, SlotIdx, Var, VarSlot},
    },
};

// == Frame layouts

/// Slot assignment for one callable, keyed by name and iteration path.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct FrameLayout {
    /// Slot of each name under its iteration path.
    slots: HashMap<(String, Vec<Iter>), SlotIdx>,
    /// Optional and list transitions indexed by the inner slot.
    slots_iter: Vec<[Option<SlotIdx>; 2]>,
}

impl FrameLayout {
    // - Accessors

    pub fn len(&self) -> usize {
        self.slots.len()
    }

    pub fn is_empty(&self) -> bool {
        self.slots.is_empty()
    }

    // - Resolution

    /// The slot for a key, allocating the next one when the key is new.
    fn reserve(&mut self, key: (String, Vec<Iter>)) -> SlotIdx {
        // Reuse slots without changing their iteration transitions
        if let Some(slot) = self.slots.get(&key) {
            return *slot;
        }
        let slot = SlotIdx(self.slots.len());
        // Link children that were registered before their parent
        let mut slots_iter = [None; 2];
        for (idx, iter) in [Iter::Opt, Iter::List].into_iter().enumerate() {
            let mut key_outer = key.clone();
            key_outer.1.push(iter);
            slots_iter[idx] = self.slots.get(&key_outer).copied();
        }
        self.slots_iter.push(slots_iter);
        // Link a parent that was registered before this child
        let mut key_inner = key.clone();
        if let Some(iter) = key_inner.1.pop()
            && let Some(slot_inner) = self.slots.get(&key_inner)
        {
            self.slots_iter[slot_inner.0][iter_index(iter)] = Some(slot);
        }
        self.slots.insert(key, slot);
        slot
    }

    /// Resolves a plain identifier to its slot.
    pub fn resolve_id(&mut self, id: Id) -> IdSlot {
        let slot = self.reserve((id.node.clone(), vec![]));
        IdSlot { id, slot }
    }

    /// Resolves a variable under its iteration path to its slot.
    pub fn resolve_var(&mut self, var: Var) -> VarSlot {
        let slot = self.reserve((var.id.node.clone(), var.iters.clone()));
        VarSlot { slot, var }
    }

    /// Resolves one prepared iteration transition without hashing names.
    pub fn find_iter_slot(&self, slot: SlotIdx, iter: Iter) -> SlotIdx {
        self.slots_iter[slot.0][iter_index(iter)]
            .expect("iterated binding is resolved during preparation")
    }

    /// The slot of `var` one iteration deeper, resolved during preparation.
    pub fn find_iter_var(&self, var: &VarSlot, iter: Iter) -> VarSlot {
        let var_inner = var;
        let mut var = var_inner.var.clone();
        var.iters.push(iter);
        VarSlot { slot: self.find_iter_slot(var_inner.slot, iter), var }
    }
}

fn iter_index(iter: Iter) -> usize {
    match iter {
        Iter::Opt => 0,
        Iter::List => 1,
    }
}

// == Value frames

/// Values of one callable's slots, copy-on-write across clones.
#[derive(Clone, Debug, Default)]
pub struct Frame {
    /// Layout the slots follow.
    layout: Rc<FrameLayout>,
    /// Slot values in one allocation, shared until written.
    values: Rc<[Option<Value>]>,
}

impl Frame {
    // - Construction

    /// An all-unbound frame for the layout.
    pub fn new(layout: Rc<FrameLayout>) -> Self {
        let values = std::iter::repeat_n(None, layout.len()).collect();
        Self { layout, values }
    }

    /// A fresh frame with the same layout.
    pub fn wipe(&self) -> Self {
        Self::new(Rc::clone(&self.layout))
    }

    // - Accessors

    pub fn layout(&self) -> &Rc<FrameLayout> {
        &self.layout
    }

    pub fn get(&self, slot: SlotIdx) -> Option<&Value> {
        self.values[slot.0].as_ref()
    }

    // - Updates

    /// Writes a slot, copying the values first if they are shared.
    pub fn set(&mut self, slot: SlotIdx, value: Value) {
        Rc::make_mut(&mut self.values)[slot.0] = Some(value);
    }
}
