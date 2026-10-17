//! Per-callable variable slots and copy-on-write value frames
//!
//! Layouts resolve names and iterator paths during preparation.
//! Execution uses slots;
//! a reserved slot stays unbound until an assignment writes its value.
//! Frames share their fixed-size value slice until one is written.

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::lang::{
    common::{Id, Iter},
    data::{
        value::flat::Value,
        var::{IdSlot, SlotIdx, Var, VarSlot},
    },
};

// == Frame layouts

/// Prepared optional and list transitions from one inner slot.
#[derive(Clone, Debug, Default, PartialEq)]
struct IterSlots {
    slot_opt: Option<SlotIdx>,
    slot_list: Option<SlotIdx>,
}

/// Buffers a layout keeps for reuse, bounded for deep recursion.
const FRAME_POOL_LEN_MAX: usize = 8;

/// Recycles uniquely owned buffers without making them part of layout equality.
#[derive(Debug, Default)]
struct FramePool(RefCell<Vec<Rc<[Option<Value>]>>>);
impl Clone for FramePool {
    fn clone(&self) -> Self {
        Self::default()
    }
}
impl PartialEq for FramePool {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

/// Assigns variable slots and records one-step iteration transitions.
///
/// If `x` has slot 0 and `x*` has slot 2:
/// - `vars` maps `("x", [])` to slot 0 and `("x", [List])` to slot 2.
/// - `iters[0].slot_list` points to slot 2.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct FrameLayout {
    /// Maps each variable's name and iteration path to its slot.
    vars: HashMap<(String, Vec<Iter>), SlotIdx>,
    /// Maps each slot to its optional and list iteration slots.
    iters: Vec<IterSlots>,
    pool: FramePool,
}

impl FrameLayout {
    /// Returns an unshared buffer of this layout's fixed size.
    fn buffer(&self) -> Rc<[Option<Value>]> {
        self.pool
            .0
            .borrow_mut()
            .pop()
            .unwrap_or_else(|| std::iter::repeat_n(None, self.len()).collect())
    }

    // - Accessors

    pub fn len(&self) -> usize {
        self.vars.len()
    }

    pub fn is_empty(&self) -> bool {
        self.vars.is_empty()
    }

    // - Resolution

    /// Returns the existing slot or reserves a new one and links its iterations.
    ///
    /// Registering `x` and `x*` in either order establishes the same link:
    /// `iters[slot_x].slot_list = Some(slot_x_list)`.
    fn reserve(&mut self, key: (String, Vec<Iter>)) -> SlotIdx {
        // An existing variable already has its slot and recorded links
        if let Some(slot) = self.vars.get(&key) {
            return *slot;
        }
        self.pool.0.get_mut().clear();
        let slot = SlotIdx(self.vars.len());

        // If `x*` was registered first, registering `x` finds its list slot here
        let find_outer_slot = |iter| {
            let mut key_outer = key.clone();
            key_outer.1.push(iter);
            self.vars.get(&key_outer).copied()
        };
        let slots_iter = IterSlots {
            slot_opt: find_outer_slot(Iter::Opt),
            slot_list: find_outer_slot(Iter::List),
        };
        self.iters.push(slots_iter);

        // If `x` was registered first, registering `x*` fills in that same link
        let mut key_inner = key.clone();
        if let Some(iter) = key_inner.1.pop()
            && let Some(slot_inner) = self.vars.get(&key_inner)
        {
            let slots_iter = &mut self.iters[slot_inner.0];
            match iter {
                Iter::Opt => slots_iter.slot_opt = Some(slot),
                Iter::List => slots_iter.slot_list = Some(slot),
            }
        }

        // Record the new variable after linking it to existing slots
        self.vars.insert(key, slot);
        slot
    }

    /// Resolves a plain identifier to its slot.
    pub fn resolve_id(&mut self, id: Id) -> IdSlot {
        let slot = self.reserve((id.node.to_string(), vec![]));
        IdSlot { id, slot }
    }

    /// Resolves a variable under its iteration path to its slot.
    pub fn resolve_var(&mut self, var: Var) -> VarSlot {
        let slot = self.reserve((var.id.node.to_string(), var.iters.clone()));
        VarSlot::new(slot, var)
    }

    /// Resolves one prepared iteration transition without hashing names.
    #[inline]
    pub fn find_slot_iterated(&self, slot: SlotIdx, iter: Iter) -> SlotIdx {
        let slots_iter = &self.iters[slot.0];
        match iter {
            Iter::Opt => slots_iter.slot_opt,
            Iter::List => slots_iter.slot_list,
        }
        .expect("iterated binding is resolved during preparation")
    }

    /// The slot of `var` one iteration deeper, resolved during preparation.
    pub fn find_var_iterated(&self, var: &VarSlot, iter: Iter) -> VarSlot {
        var.iterated(self.find_slot_iterated(var.slot, iter), iter)
    }
}

// == Value frames

/// Values of one callable's slots, copy-on-write across clones.
#[derive(Clone, Debug)]
pub struct Frame {
    /// Layout the slots follow.
    layout: Rc<FrameLayout>,
    /// Slot values in one allocation, shared until written.
    values: Option<Rc<[Option<Value>]>>,
}

impl Frame {
    // - Construction

    /// An all-unbound frame for the layout.
    pub fn new(layout: Rc<FrameLayout>) -> Self {
        let mut values = layout.buffer();
        Rc::get_mut(&mut values)
            .expect("pooled buffer is unique")
            .fill(None);
        Self { layout, values: Some(values) }
    }

    /// Clears row bindings while reusing uniquely owned frame storage.
    pub fn clear(&mut self) {
        let values = self.values.as_mut().expect("live frame");
        if Rc::strong_count(values) > 1 {
            *values = self.layout.buffer();
        }
        Rc::get_mut(values).expect("unshared frame").fill(None);
    }

    // - Accessors

    pub fn layout(&self) -> &Rc<FrameLayout> {
        &self.layout
    }

    #[inline]
    pub fn get(&self, slot: SlotIdx) -> Option<&Value> {
        self.values.as_ref().expect("live frame")[slot.0].as_ref()
    }

    // - Updates

    /// Writes a slot, copying the values first if they are shared.
    #[inline]
    pub fn set(&mut self, slot: SlotIdx, value: Value) {
        let values = self.values.as_mut().expect("live frame");
        if Rc::strong_count(values) > 1 {
            let mut copied = self.layout.buffer();
            Rc::get_mut(&mut copied)
                .expect("pooled buffer is unique")
                .copy_from_slice(values);
            *values = copied;
        }
        Rc::get_mut(values).expect("unshared frame")[slot.0] = Some(value);
    }
}

impl Default for Frame {
    fn default() -> Self {
        Self::new(Rc::default())
    }
}

impl Drop for Frame {
    fn drop(&mut self) {
        if let Some(values) = self.values.take()
            && Rc::strong_count(&values) == 1
        {
            let pool = &mut *self.layout.pool.0.borrow_mut();
            // Deep recursion must not retain an unbounded number of frames
            if pool.len() < FRAME_POOL_LEN_MAX {
                pool.push(values);
            }
        }
    }
}
