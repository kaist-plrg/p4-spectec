//! Placeholders for information omitted from the shared meta representation

use crate::lang::common::source::Span;
use crate::{lang::il::ast as il, phrase};

/// The origin omitted from a variant case.
pub(super) fn typ_origin() -> il::TypOrigin {
    let span = Span::default();
    phrase!(node: il::TypOriginKind {
        id: phrase!(node: "".into(), span: span),
        targs: vec![],
    }, span: span)
}

/// The general hints omitted from meta definitions.
pub(in crate::interface::meta) fn hints() -> Vec<il::Hint> {
    vec![]
}
