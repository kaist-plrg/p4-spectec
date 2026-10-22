//! IL constructor notation from `spec-meta/1-syntax/1.1-il.watsup`
//!
//! Forms shared with AL reuse the AL notation.

pub(super) use crate::interface::al::mixops::{
    BUILTINFUNC, DEBUG, EXTFUNC, EXTTYP, IF, IFHOLD, IFNOTHOLD, ITER_PREM, TYP_DEF,
};

pub(super) const REL_PREM: &str = "REL id ':' exp* `( inputHint `)";
pub(super) const RUL: &str = "id ':' exp* '-' premIL*";
pub(super) const RULGROUP: &str = "id ':' rulIL*";
pub(super) const ELSGROUP: &str = "id ':' rulIL";
pub(super) const TBLROW: &str = "arg* '=' exp";
pub(super) const CLAUSE: &str = "arg* '=' exp '-' premIL*";
pub(super) const EXTREL: &str = "EXTREL id ':' typ* `( inputHint `)";
pub(super) const REL_DEF: &str = "REL id ':' typ* `( inputHint `) '=' rulGroupIL* elsGroupIL?";
pub(super) const TABLEFUNC: &str = "TABLEFUNC id param* ':' typ '=' tblRowIL*";
pub(super) const FUNC_DEF: &str = "FUNC id tparam* param* ':' typ '=' clauseIL* elsClauseIL?";
