//! IL constructor notation from `spec-meta/1-syntax/1.1-il.watsup`
//!
//! Forms shared with AL come from `meta::shared::mixops`.

pub(super) use crate::interface::meta::shared::mixops::{
    BUILTINFUNC, DEBUG, EXTFUNC, EXTTYP, IF, IFHOLD, IFNOTHOLD, ITER_PREM, TYP_DEF,
};

// == Premises

pub(super) const ITERPREM: &str = "iter vari*";
pub(super) const REL_PREM: &str = "REL id ':' typedExp* `( inputHint `)";

// == Rules and clauses

pub(super) const RUL: &str = "id ':' typedExp* '-' premIL*";
pub(super) const RULGROUP: &str = "id ':' rulIL*";
pub(super) const ELSGROUP: &str = "id ':' rulIL";
pub(super) const TBLROW: &str = "arg* '=' typedExp";
pub(super) const CLAUSE: &str = "arg* '=' typedExp '-' premIL*";

// == Definitions

pub(super) const EXTREL: &str = "EXTREL id ':' typ* `( inputHint `)";
pub(super) const REL_DEF: &str = "REL id ':' typ* `( inputHint `) '=' rulGroupIL* elsGroupIL?";
pub(super) const TABLEFUNC: &str = "TABLEFUNC id param* ':' typ '=' tblRowIL*";
pub(super) const FUNC_DEF: &str = "FUNC id tparam* param* ':' typ '=' clauseIL* elsClauseIL?";
