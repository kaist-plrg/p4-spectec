//! EL constructor notation from `spec-meta/1-syntax/1.1-el.watsup`
//!
//! A case of a variant syntax is named `<SYNTAX>_<ATOM>`, as `EXP_BIN`;
//! a syntax with a single shape takes its own name, as `TYP_FIELD`.

// == Types

pub(super) const PLAIN_TYP_BOOL: &str = "BOOL";
pub(super) const PLAIN_TYP_NAT: &str = "NAT";
pub(super) const PLAIN_TYP_INT: &str = "INT";
pub(super) const PLAIN_TYP_TEXT: &str = "TEXT";
pub(super) const PLAIN_TYP_VAR: &str = "VAR id plainTypEL*";
pub(super) const PLAIN_TYP_PAREN: &str = "PAREN plainTypEL";
pub(super) const PLAIN_TYP_TUP: &str = "TUP plainTypEL*";
pub(super) const PLAIN_TYP_ITER: &str = "ITER plainTypEL iter";

pub(super) const TYP_PLAIN: &str = "PLAIN plainTypEL";
pub(super) const TYP_NOTATION: &str = "NOTATION notTypEL";

pub(super) const NOT_TYP_ATOM: &str = "ATOM atom";
pub(super) const NOT_TYP_SEQ: &str = "SEQ typEL*";
pub(super) const NOT_TYP_INFIX: &str = "INFIX typEL atom typEL";
pub(super) const NOT_TYP_BRACK: &str = "BRACK atom typEL atom";

pub(super) const TYP_FIELD: &str = "atom plainTypEL";
pub(super) const DEF_TYP_ALIAS: &str = "ALIAS plainTypEL";
pub(super) const DEF_TYP_STRUCT: &str = "STRUCT typFieldEL*";
pub(super) const DEF_TYP_VARIANT: &str = "VARIANT typEL*";

// == Expressions

pub(super) const EXP_FIELD: &str = "atom expEL";

pub(super) const HOLE_NTH: &str = "NTH nat";
pub(super) const HOLE_NEXT: &str = "NEXT";
pub(super) const HOLE_REST: &str = "REST";
pub(super) const HOLE_NONE: &str = "NONE";

pub(super) const EXP_BOOL: &str = "BOOL bool";
pub(super) const EXP_NAT: &str = "NAT nat";
pub(super) const EXP_INT: &str = "INT int";
pub(super) const EXP_TEXT: &str = "TEXT text";
pub(super) const EXP_ID: &str = "ID id";
pub(super) const EXP_UN: &str = "UN unOp expEL";
pub(super) const EXP_BIN: &str = "BIN expEL binOp expEL";
pub(super) const EXP_CMP: &str = "CMP expEL cmpOp expEL";
pub(super) const EXP_ARITH: &str = "ARITH expEL";
pub(super) const EXP_EPS: &str = "EPS";
pub(super) const EXP_LIST: &str = "LIST expEL*";
pub(super) const EXP_CONS: &str = "CONS expEL expEL";
pub(super) const EXP_CAT: &str = "CAT expEL expEL";
pub(super) const EXP_IDX: &str = "IDX expEL expEL";
pub(super) const EXP_SLICE: &str = "SLICE expEL expEL expEL";
pub(super) const EXP_LEN: &str = "LEN expEL";
pub(super) const EXP_MEM: &str = "MEM expEL expEL";
pub(super) const EXP_STR: &str = "STR expFieldEL*";
pub(super) const EXP_DOT: &str = "DOT expEL atom";
pub(super) const EXP_UPD: &str = "UPD expEL pathEL expEL";
pub(super) const EXP_PAREN: &str = "PAREN expEL";
pub(super) const EXP_TUP: &str = "TUP expEL*";
pub(super) const EXP_CALL: &str = "CALL id plainTypEL* argEL*";
pub(super) const EXP_ITER: &str = "ITER expEL iter";
pub(super) const EXP_SUB: &str = "SUB expEL plainTypEL";
pub(super) const EXP_ATOM: &str = "ATOM atom";
pub(super) const EXP_SEQ: &str = "SEQ expEL*";
pub(super) const EXP_INFIX: &str = "INFIX expEL atom expEL";
pub(super) const EXP_BRACK: &str = "BRACK atom expEL atom";
pub(super) const EXP_HOLE: &str = "HOLE holeEL";
pub(super) const EXP_FUSE: &str = "FUSE expEL expEL";
pub(super) const EXP_UNPAREN: &str = "UNPAREN expEL";
pub(super) const EXP_LATEX: &str = "LATEX text";

// == Paths

pub(super) const PATH_ROOT: &str = "ROOT";
pub(super) const PATH_IDX: &str = "IDX pathEL expEL";
pub(super) const PATH_SLICE: &str = "SLICE pathEL expEL expEL";
pub(super) const PATH_DOT: &str = "DOT pathEL atom";

// == Arguments and parameters

pub(super) const ARG_EXP: &str = "EXP expEL";
pub(super) const ARG_FUN: &str = "FUN id";

pub(super) const PARAM_EXP: &str = "EXP plainTypEL";
pub(super) const PARAM_FUN: &str = "FUN id ':' tparam* paramEL* '->' plainTypEL";

// == Premises

pub(super) const PREM_VAR: &str = "VAR id ':' plainTypEL";
pub(super) const PREM_REL: &str = "REL id ':' expEL";
pub(super) const PREM_RELNOT: &str = "RELNOT id ':' expEL";
pub(super) const PREM_IF: &str = "IF expEL";
pub(super) const PREM_ELSE: &str = "ELSE";
pub(super) const PREM_ITER: &str = "ITER premEL iter";
pub(super) const PREM_DEBUG: &str = "DEBUG expEL";

// == Hints, rules, and table rows

pub(super) const HINT: &str = "id expEL";
pub(super) const RUL: &str = "id '/' id ':' expEL '-' premEL*";
pub(super) const TBL_ROW: &str = "expEL '=' expEL";

// == Definitions

pub(super) const SYNTAX_ENTRY: &str = "id tparam*";

pub(super) const DEF_EXTSYNTAX: &str = "EXTSYNTAX id";
pub(super) const DEF_SYNTAX: &str = "SYNTAX syntaxEntryEL*";
pub(super) const DEF_TYP: &str = "TYP id tparam* '=' defTypEL";
pub(super) const DEF_VAR: &str = "VAR id ':' plainTypEL";
pub(super) const DEF_EXTREL: &str = "EXTREL id ':' notTypEL hintEL*";
pub(super) const DEF_REL: &str = "REL id ':' notTypEL hintEL*";
pub(super) const DEF_RULEGROUP: &str = "RULEGROUP id '/' id ':' rulEL*";
pub(super) const DEF_EXTDEC: &str = "EXTDEC id tparam* paramEL* ':' plainTypEL";
pub(super) const DEF_BUILTINDEC: &str = "BUILTINDEC id tparam* paramEL* ':' plainTypEL";
pub(super) const DEF_TABLEDEC: &str = "TABLEDEC id paramEL* ':' plainTypEL";
pub(super) const DEF_DEC: &str = "DEC id tparam* paramEL* ':' plainTypEL";
pub(super) const DEF_TABLE: &str = "TABLE id tblRowEL*";
pub(super) const DEF_DEF: &str = "DEF id tparam* argEL* '=' expEL '-' premEL*";
