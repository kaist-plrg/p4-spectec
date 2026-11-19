//! Synchronous dispatch of interpreter events to registered handlers
//!
//! `Hooks` forwards prepared AL premises and SL instructions in registration order.
//! Handlers borrow the execution arena to interpret arena-relative syntax.
//! Each handler owns its observation state and its lifecycle.

use crate::lang::{al, data::arena::Arena, sl};

/// Observes prepared syntax as the interpreter enters its evaluation.
///
/// Callbacks borrow the node and its arena only for the duration of the event.
/// Cloning prepared syntax does not make its arena-relative handles independent.
/// Handler failures are not converted into interpreter errors.
pub trait Handler {
    /// Observes an AL premise before it is evaluated.
    fn on_prem(&mut self, _arena: &Arena, _prem_al: &al::prepared::Prem) {}

    /// Observes an SL instruction before it is evaluated.
    fn on_instr(&mut self, _arena: &Arena, _instr_sl: &sl::prepared::Instr) {}
}

/// Forwards events synchronously to handlers in registration order.
#[derive(Default)]
pub struct Hooks {
    handlers: Vec<Box<dyn Handler>>,
}

impl Hooks {
    /// Appends a handler without changing previously registered handlers.
    pub fn register(&mut self, handler: Box<dyn Handler>) {
        self.handlers.push(handler);
    }

    /// Removes and drops every registered handler.
    pub fn clear_handlers(&mut self) {
        self.handlers.clear();
    }

    /// Forwards an AL premise and the arena used to prepare it.
    pub fn on_prem(&mut self, arena: &Arena, prem_al: &al::prepared::Prem) {
        for handler in &mut self.handlers {
            handler.on_prem(arena, prem_al);
        }
    }

    /// Forwards an SL instruction and the arena used to prepare it.
    pub fn on_instr(&mut self, arena: &Arena, instr_sl: &sl::prepared::Instr) {
        for handler in &mut self.handlers {
            handler.on_instr(arena, instr_sl);
        }
    }
}
