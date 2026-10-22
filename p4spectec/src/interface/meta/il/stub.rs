//! Canonical metadata omitted from the IL meta syntax

use std::rc::Rc;

use crate::interface::meta::shared::Error;
use crate::{
    lang::{
        common::source::Span,
        data::notation::tree::{Mixfix, Mixop},
        il::ast as il,
    },
    phrase,
};

/// Positional relation notation over arguments in their original order.
pub(super) fn notation<T>(args: Vec<T>) -> Result<Mixfix<T>, Error> {
    let mixop = Rc::new(Mixop::Seq(vec![Mixop::Arg; args.len()]));
    Ok(Mixfix::new(mixop, args)?)
}

/// The otherwise marker implied by an otherwise slot.
pub(super) fn otherwise(span: Span) -> il::Otherwise {
    phrase!(node: il::OtherwiseKind, span: span)
}
