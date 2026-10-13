//! Type annotations for values in the AL meta specification
//!
//! Named types are resolved by the meta runner. Lists and options retain
//! their element annotations even when empty.

use crate::{
    lang::{
        common::{Iter, source::Span},
        data::typ::{TypKind, make},
    },
    phrase,
};
use std::rc::Rc;

/// Constructs a named meta type.
pub(super) fn named(name: &str) -> Rc<TypKind> {
    Rc::new(TypKind::Var(phrase!(node: name.into(), span: Span::default()), vec![]))
}

/// Constructs an iteration over a named meta type.
pub(super) fn iter(name: &str, iter_kind: Iter) -> Rc<TypKind> {
    let typ = phrase!(node: (*named(name)).clone(), span: Span::default());
    Rc::new(make::iter(typ, iter_kind).node)
}
