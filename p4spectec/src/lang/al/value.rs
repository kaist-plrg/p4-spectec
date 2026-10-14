//! AL literal expressions constructed from runtime values
//!
//! `Value::to_exp` preserves value types, source spans, and argument order.
//! Case notation expands to the tree mixop required by the source AL AST.

use std::rc::Rc;

use crate::{
    lang::{
        al::ast as al,
        data::{
            arena::Arena,
            notation::{ArityMismatch, tree::Mixfix},
            value::flat::{Value, ValueKind},
        },
    },
    note_phrase,
};

/// A runtime value that cannot be represented as an AL literal.
#[derive(Debug, thiserror::Error)]
pub enum ValueToExpError {
    /// Functions and external host values have no literal expression.
    #[error("cannot embed a function or extern value in an AL expression")]
    Unsupported,
    /// The notation does not fit its argument expressions.
    #[error(transparent)]
    Arity(#[from] ArityMismatch),
}

impl Value {
    /// Converts a runtime value into an AL literal expression.
    pub fn to_exp(self, arena: &Arena) -> Result<al::Exp, ValueToExpError> {
        let value = self;
        let span = *arena.value.span(value.span);
        let typ = arena.value.typ(value.note).clone();
        let exp_kind = match arena.value.kind(value.node) {
            ValueKind::Bool(value) => al::ExpKind::Bool(*value),
            ValueKind::Num(num) => al::ExpKind::Num(num.clone()),
            ValueKind::Text(text) => al::ExpKind::Text(text.clone()),
            ValueKind::Struct(value_fields) => al::ExpKind::Str(
                value_fields
                    .iter()
                    .map(|value_field| {
                        Ok(al::ExpField {
                            atom: value_field.atom.clone(),
                            exp: value_field.value.to_exp(arena)?,
                        })
                    })
                    .collect::<Result<_, ValueToExpError>>()?,
            ),
            ValueKind::Case(value_case) => {
                let exps = value_case
                    .args()
                    .iter()
                    .map(|value| value.to_exp(arena))
                    .collect::<Result<_, ValueToExpError>>()?;
                let mixop = value_case.mixop().into_tree(&arena.mixop);
                al::ExpKind::Case(Box::new(Mixfix::new(Rc::new(mixop), exps)?))
            }
            ValueKind::Tuple(values) => al::ExpKind::Tuple(
                values
                    .iter()
                    .map(|value| value.to_exp(arena))
                    .collect::<Result<_, ValueToExpError>>()?,
            ),
            ValueKind::Opt(value) => al::ExpKind::Opt(
                value
                    .map(|value| value.to_exp(arena).map(Box::new))
                    .transpose()?,
            ),
            ValueKind::List(values) => al::ExpKind::List(
                values
                    .iter()
                    .map(|value| value.to_exp(arena))
                    .collect::<Result<_, ValueToExpError>>()?,
            ),
            ValueKind::Func(_) | ValueKind::Extern(_) => {
                return Err(ValueToExpError::Unsupported);
            }
        };
        Ok(note_phrase!(node: exp_kind, note: typ, span: span))
    }
}
