//! Source variables and their prepared slot references
//!
//! A `Var` names a binding with its type and iteration path;
//! `IdSlot` and `VarSlot` are the same references after preparation,
//! carrying the frame slot the interpreter reads.

mod slot;
mod var;

pub use slot::{IdSlot, SlotIdx, VarSlot};
pub use var::Var;
