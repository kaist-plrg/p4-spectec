//! Stateful fresh-type-id builtin
//!
//! Each builtin instance names fresh types `FRESH__0`, `FRESH__1`, ...;
//! clearing one instance restarts only that runner's sequence.

use crate::lang::{
    common::source::Span,
    data::{
        arena::Arena,
        value::flat::{self as value, Value},
    },
};

use crate::lang::il::ast::Typ;

use super::{BuiltinError, call::StatefulBuiltin, extract};

/// Next fresh index owned by one builtin table.
#[derive(Default)]
pub(super) struct Fresh {
    counter: u64,
}

impl StatefulBuiltin for Fresh {
    fn invoke(
        &mut self,
        arena: &mut Arena,
        targs: &[Typ],
        values: &[Value],
    ) -> Result<Value, BuiltinError> {
        // Validate before advancing the local sequence
        extract::zero(targs)?;
        extract::zero(values)?;
        let counter = self.counter;
        self.counter = counter.wrapping_add(1);
        let type_id = format!("FRESH__{counter}");
        let value = value::make::text(arena, type_id, Span::default())?;
        Ok(value)
    }

    fn clear(&mut self) {
        self.counter = 0;
    }
}
