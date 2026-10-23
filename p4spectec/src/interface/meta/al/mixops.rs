//! Constructor notation for AL syntax and runtime values
//!
//! These shapes follow `spec-meta/1-syntax/1.2-al.watsup`
//! and `spec-meta/2-runtime/2.0-value.watsup`;
//! shapes shared with IL come from `meta::shared::mixops`.

pub(super) use crate::interface::meta::shared::mixops::*;

// == Values

pub(super) const VALFIELD: &str = "atom val";
pub(super) const VALCASE: &str = "mixop val*";
pub(super) const FUNC: &str = "FUNC id";
pub(super) const EXT: &str = "EXT json";

// == Premises

pub(super) const REL_PREM: &str = "REL id ':' exp* '->' exp*";
pub(super) const ITERPREM: &str = "iter '<-' vari* '->' vari*";
pub(super) const LET: &str = "LET exp '=' exp";

// == Rules and clauses

pub(super) const RULMATCH: &str = "exp* '-' prem*";
pub(super) const RULPATH: &str = "id '=' exp* '-' prem*";
pub(super) const RUL: &str = "rulMatch '->' rulPath";
pub(super) const RULGROUP: &str = "id ':' rul*";
pub(super) const ELSGROUP: &str = "id ':' rul";
pub(super) const CLAUSE: &str = "arg* '=' exp '-' prem*";

// == Definitions

pub(super) const EXTREL: &str = "EXTREL id ':' typ* '->' typ*";
pub(super) const REL_DEF: &str = "REL id ':' typ* '->' typ* '=' rulGroup* elsGroup?";
pub(super) const TABLEFUNC: &str = "TABLEFUNC id param* ':' typ '=' tblRow*";
pub(super) const FUNC_DEF: &str = "FUNC id tparam* param* ':' typ '=' clause* elsClause?";

// == Results

pub(super) const OK: &str = "OK val";
