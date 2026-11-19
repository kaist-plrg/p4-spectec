//! Per-callable variable and function-parameter slots and copy-on-write value frames
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

/// Assigns slots to variables and function parameters.
///
/// If `x` has slot 0 and `x*` has slot 2,
/// `vars` maps `("x", [])` to slot 0 and `("x", [List])` to slot 2.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct FrameLayout {
    /// Maps each variable's name and iteration path to its slot.
    vars: HashMap<(String, Vec<Iter>), SlotIdx>,
    /// Maps each function parameter's name to the slot of its function value.
    funcs: Vec<(Rc<str>, SlotIdx)>,
    /// The number of slots.
    len: usize,
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
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    // - Resolution

    /// Appends a slot.
    fn push_slot(&mut self) -> SlotIdx {
        self.pool.0.get_mut().clear();
        let slot = SlotIdx(self.len);
        self.len += 1;
        slot
    }

    /// Returns a variable's existing slot or reserves a new one.
    fn reserve_var(&mut self, key: (String, Vec<Iter>)) -> SlotIdx {
        if let Some(slot) = self.vars.get(&key) {
            return *slot;
        }
        let slot = self.push_slot();
        self.vars.insert(key, slot);
        slot
    }

    /// Returns a function parameter's existing slot or reserves a new one.
    fn reserve_func(&mut self, id: &Id) -> SlotIdx {
        if let Some(slot) = self.find_func_slot(id) {
            return slot;
        }
        // Function values never iterate
        let slot = self.push_slot();
        self.funcs.push((Rc::clone(&id.node), slot));
        slot
    }

    /// Resolves a plain variable to its slot.
    pub fn resolve_var_id(&mut self, id: Id) -> IdSlot {
        let slot = self.reserve_var((id.node.to_string(), vec![]));
        IdSlot { id, slot }
    }

    /// Resolves a variable under its iteration path to its slot.
    pub fn resolve_var(&mut self, var: Var) -> VarSlot {
        let slot = self.reserve_var((var.id.node.to_string(), var.iters.clone()));
        VarSlot::new(slot, var)
    }

    /// Resolves a function parameter, which holds a function value, to its slot.
    pub fn resolve_func(&mut self, id: &Id) -> SlotIdx {
        self.reserve_func(id)
    }

    /// The slot of a function parameter, if `id` names one.
    #[inline]
    pub fn find_func_slot(&self, id: &Id) -> Option<SlotIdx> {
        self.funcs
            .iter()
            .find_map(|(name, slot)| (*name == id.node).then_some(*slot))
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

    pub fn get(&self, slot: SlotIdx) -> Option<&Value> {
        self.values.as_ref().expect("live frame")[slot.0].as_ref()
    }

    // - Updates

    /// Writes a slot, copying the values first if they are shared.
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
