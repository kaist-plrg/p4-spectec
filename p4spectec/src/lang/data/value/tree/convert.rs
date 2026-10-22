//! Interning tree values as flat values
//!
//! `into_flat` interns child values before their parent,
//! preserving types, source spans, and field and argument order.
//! Extern payloads decode through the receiving runner's definitions.

use crate::lang::data::{
    arena::Arena,
    encoding::Encoding,
    notation::flat as notation,
    value::extern_value::{ExternValue, TypDefs},
};

use super::super::{
    ValueError,
    flat::{self, external::DecodeContext},
};
use super::{Value, ValueCase, ValueField, ValueKind};

// = Values

impl Value {
    /// Interns the tree in the target arena, preserving its type and source span.
    ///
    /// Extern values are read through the definitions in `typdefs`.
    pub fn into_flat(
        self,
        arena: &mut Arena,
        typdefs: &TypDefs,
    ) -> Result<flat::Value, ValueError> {
        let kind = self.node.into_flat(arena, typdefs)?;
        flat::Value::new(arena, kind, self.note.into(), self.span)
    }
}

// = Bodies

impl ValueKind {
    /// Interns child trees in field and element order.
    pub(in crate::lang::data::value) fn into_flat(
        self,
        arena: &mut Arena,
        typdefs: &TypDefs,
    ) -> Result<flat::ValueKind, ValueError> {
        Ok(match self {
            Self::Bool(value) => flat::ValueKind::Bool(value),
            Self::Num(num) => flat::ValueKind::Num(num),
            Self::Text(text) => flat::ValueKind::Text(text),
            Self::Struct(value_fields) => flat::ValueKind::Struct(
                value_fields
                    .into_iter()
                    .map(|ValueField { atom, value }| {
                        Ok(flat::ValueField { atom, value: value.into_flat(arena, typdefs)? })
                    })
                    .collect::<Result<_, ValueError>>()?,
            ),
            Self::Case(value_case) => flat::ValueKind::Case(value_case.into_flat(arena, typdefs)?),
            Self::Tuple(values) => flat::ValueKind::Tuple(
                values
                    .into_iter()
                    .map(|value| value.into_flat(arena, typdefs))
                    .collect::<Result<_, _>>()?,
            ),
            Self::Opt(value) => flat::ValueKind::Opt(
                value
                    .map(|value| value.into_flat(arena, typdefs))
                    .transpose()?,
            ),
            Self::List(values) => flat::ValueKind::List(
                values
                    .into_iter()
                    .map(|value| value.into_flat(arena, typdefs))
                    .collect::<Result<_, _>>()?,
            ),
            Self::Func(id) => flat::ValueKind::Func(id),
            Self::Extern(encoded) => {
                let mut ctx = DecodeContext::new(arena, Encoding::ArenaIndependent, typdefs);
                flat::ValueKind::Extern(ExternValue::decode(&mut ctx, &encoded)?)
            }
        })
    }
}

// = Cases

impl ValueCase {
    /// Interns arguments in notation order, then their mixop.
    fn into_flat(
        self,
        arena: &mut Arena,
        typdefs: &TypDefs,
    ) -> Result<flat::ValueCase, ValueError> {
        // Intern argument values before the case's mixop
        let (mixop, values) = self.into_parts();
        let values = values
            .into_iter()
            .map(|value| value.into_flat(arena, typdefs))
            .collect::<Result<_, _>>()?;
        let arena_mixop = &mut arena.mixop;
        let mixop = mixop.into_flat(arena_mixop)?;

        // A filled tree supplies one value per argument position
        Ok(notation::Mixfix::new(arena_mixop, mixop, values)
            .expect("a tree case fills every position"))
    }
}
