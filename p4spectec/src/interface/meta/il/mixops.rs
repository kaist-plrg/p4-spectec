//! IL constructor notation from `spec-meta/1-syntax/1.3-il.watsup`
//!
//! Forms shared with AL come from `meta::shared::mixops`,
//! named the same way.

pub(super) use crate::interface::meta::shared::mixops::{
    DEF_BUILTINFUNC, DEF_EXTFUNC, DEF_EXTTYP, DEF_TYP, PREM_DEBUG, PREM_IF, PREM_IFHOLD,
    PREM_IFNOTHOLD,
};

// == Premises

pub(super) const ITER_PREM: &str = "iter vari*";
pub(super) const PREM_REL: &str = "REL id ':' typedExp* `( inputHint `)";
pub(super) const PREM_ITER: &str = "ITER premIL premIterIL";

// == Rules and clauses

pub(super) const RUL: &str = "id ':' typedExp* '-' premIL*";
pub(super) const RUL_GROUP: &str = "id ':' rulIL*";
pub(super) const ELS_GROUP: &str = "id ':' rulIL";
pub(super) const TBL_ROW: &str = "arg* '=' typedExp";
pub(super) const CLAUSE: &str = "arg* '=' typedExp '-' premIL*";

// == Definitions

pub(super) const DEF_EXTREL: &str = "EXTREL id ':' typ* `( inputHint `)";
pub(super) const DEF_REL: &str = "REL id ':' typ* `( inputHint `) '=' rulGroupIL* elsGroupIL?";
pub(super) const DEF_TABLEFUNC: &str = "TABLEFUNC id param* ':' typ '=' tblRowIL*";
pub(super) const DEF_FUNC: &str = "FUNC id tparam* param* ':' typ '=' clauseIL* elsClauseIL?";
