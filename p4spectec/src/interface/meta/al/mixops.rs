//! Constructor notation for AL syntax and runtime values
//!
//! These shapes follow `spec-meta/1-syntax/1.2-al.watsup`
//! and `spec-meta/2-runtime/2.0-value.watsup`;
//! shapes shared with IL come from `meta::shared::mixops`, named the same way.

pub(super) use crate::interface::meta::shared::mixops::*;

// == Values

pub(super) const VAL_BOOL: &str = "BOOL bool";
pub(super) const VAL_NAT: &str = "NAT nat";
pub(super) const VAL_INT: &str = "INT int";
pub(super) const VAL_TEXT: &str = "TEXT text";
pub(super) const VAL_STR: &str = "STR valField*";
pub(super) const VAL_INJ: &str = "INJ valCase";
pub(super) const VAL_TUP: &str = "TUP val*";
pub(super) const VAL_OPT: &str = "OPT val?";
pub(super) const VAL_LIST: &str = "LIST val*";
pub(super) const VAL_FUNC: &str = "FUNC id";
pub(super) const VAL_EXT: &str = "EXT json";
pub(super) const VAL_FIELD: &str = "atom val";
pub(super) const VAL_CASE: &str = "mixop val*";

// == Premises

pub(super) const ITER_PREM: &str = "iter '<-' vari* '->' vari*";
pub(super) const PREM_REL: &str = "REL id ':' typedExp* '->' typedExp*";
pub(super) const PREM_LET: &str = "LET typedExp '=' typedExp";
pub(super) const PREM_ITER: &str = "ITER premAL iterPremAL";

// == Rules and clauses

pub(super) const RUL: &str = "id ':' typedExp* '->' typedExp* '-' premAL*";
pub(super) const RUL_GROUP: &str = "id ':' rulAL*";
pub(super) const ELS_GROUP: &str = "id ':' rulAL";
pub(super) const TBL_ROW: &str = "arg* '=' typedExp '-' premAL*";
pub(super) const CLAUSE: &str = "arg* '=' typedExp '-' premAL*";

// == Definitions

pub(super) const DEF_EXTREL: &str = "EXTREL id ':' typ* '->' typ*";
pub(super) const DEF_REL: &str = "REL id ':' typ* '->' typ* '=' rulGroupAL* elsGroupAL?";
pub(super) const DEF_TABLEFUNC: &str = "TABLEFUNC id param* ':' typ '=' tblRowAL*";
pub(super) const DEF_FUNC: &str = "FUNC id tparam* param* ':' typ '=' clauseAL* elsClauseAL?";
