//! Constructor notation shared by AL encoders and decoders
//!
//! These shapes follow `spec-meta/1-syntax.watsup`. Shape comparison uses
//! notation trees so quoted operators and silent tags remain distinct.

use crate::lang::{
    data::notation::tree::{Mixop, parse},
    traits::eq::SyntaxEq,
};

/// Compares a notation with a statically declared meta constructor.
pub(super) fn is(mixop: &Mixop, shape: &str) -> bool {
    mixop.syntax_eq(parse::mixop(shape).as_ref())
}

pub(super) const QUEST: &str = "QUEST";
pub(super) const STAR: &str = "STAR";
pub(super) const VARI: &str = "id typ iter*";
pub(super) const BOOL_TYP: &str = "BOOL";
pub(super) const NAT_TYP: &str = "NAT";
pub(super) const INT_TYP: &str = "INT";
pub(super) const TEXT_TYP: &str = "TEXT";
pub(super) const VAR_TYP: &str = "VAR id targ*";
pub(super) const TUP_TYP: &str = "TUP typ*";
pub(super) const ITER_TYP: &str = "ITER typ iter";
pub(super) const FUNC_TYP: &str = "FUNC tparam* typ* typ";
pub(super) const ALIAS: &str = "ALIAS typ";
pub(super) const TYPFIELD: &str = "atom typ";
pub(super) const STRUCT: &str = "STRUCT typfield*";
pub(super) const TYPCASE: &str = "mixop typ*";
pub(super) const VARIANT: &str = "VARIANT typcase*";
pub(super) const BOOL: &str = "BOOL bool";
pub(super) const NAT: &str = "NAT nat";
pub(super) const INT: &str = "INT int";
pub(super) const TEXT: &str = "TEXT text";
pub(super) const VALFIELD: &str = "atom val";
pub(super) const STR: &str = "STR valfield*";
pub(super) const VALCASE: &str = "mixop val*";
pub(super) const INJ: &str = "INJ valcase";
pub(super) const TUP: &str = "TUP val*";
pub(super) const OPT: &str = "OPT val?";
pub(super) const LIST: &str = "LIST val*";
pub(super) const FUNC: &str = "FUNC id";
pub(super) const EXT: &str = "EXT json";
