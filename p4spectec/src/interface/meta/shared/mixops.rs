//! Constructor notation for syntax shared by IL and AL
//!
//! These shapes follow `spec-meta/1-syntax/1.0-common.watsup`
//! and `spec-meta/1-syntax/1.2-typed.watsup`.
//! A case of a variant syntax is named `<SYNTAX>_<ATOM>`, as `EXP_BIN`;
//! a syntax with a single shape takes its own name, as `TYP_FIELD`.

// == Iterations and variables

pub(in crate::interface::meta) const ITER_QUEST: &str = "QUEST";
pub(in crate::interface::meta) const ITER_STAR: &str = "STAR";
pub(in crate::interface::meta) const VARI: &str = "id typ iter*";

// == Types

pub(in crate::interface::meta) const TYP_BOOL: &str = "BOOL";
pub(in crate::interface::meta) const TYP_NAT: &str = "NAT";
pub(in crate::interface::meta) const TYP_INT: &str = "INT";
pub(in crate::interface::meta) const TYP_TEXT: &str = "TEXT";
pub(in crate::interface::meta) const TYP_VAR: &str = "VAR id targ*";
pub(in crate::interface::meta) const TYP_TUP: &str = "TUP typ*";
pub(in crate::interface::meta) const TYP_ITER: &str = "ITER typ iter";
pub(in crate::interface::meta) const TYP_FUNC: &str = "FUNC tparam* typ* typ";

// == Type definitions

pub(in crate::interface::meta) const DEFTYP_ALIAS: &str = "ALIAS typ";
pub(in crate::interface::meta) const DEFTYP_STRUCT: &str = "STRUCT typField*";
pub(in crate::interface::meta) const DEFTYP_VARIANT: &str = "VARIANT typCase*";
pub(in crate::interface::meta) const TYP_FIELD: &str = "atom typ";
pub(in crate::interface::meta) const TYP_CASE: &str = "mixop typ*";

// == Operators

pub(in crate::interface::meta) const UNOP_NOT: &str = "NOT";
pub(in crate::interface::meta) const UNOP_PLUS: &str = "PLUS";
pub(in crate::interface::meta) const UNOP_MINUS: &str = "MINUS";
pub(in crate::interface::meta) const BINOP_AND: &str = "AND";
pub(in crate::interface::meta) const BINOP_OR: &str = "OR";
pub(in crate::interface::meta) const BINOP_IMPL: &str = "IMPL";
pub(in crate::interface::meta) const BINOP_EQUIV: &str = "EQUIV";
pub(in crate::interface::meta) const BINOP_ADD: &str = "ADD";
pub(in crate::interface::meta) const BINOP_SUB: &str = "SUB";
pub(in crate::interface::meta) const BINOP_MUL: &str = "MUL";
pub(in crate::interface::meta) const BINOP_DIV: &str = "DIV";
pub(in crate::interface::meta) const BINOP_MOD: &str = "MOD";
pub(in crate::interface::meta) const BINOP_POW: &str = "POW";
pub(in crate::interface::meta) const CMPOP_EQ: &str = "EQ";
pub(in crate::interface::meta) const CMPOP_NE: &str = "NE";
pub(in crate::interface::meta) const CMPOP_LT: &str = "LT";
pub(in crate::interface::meta) const CMPOP_LE: &str = "LE";
pub(in crate::interface::meta) const CMPOP_GT: &str = "GT";
pub(in crate::interface::meta) const CMPOP_GE: &str = "GE";

// == Patterns

pub(in crate::interface::meta) const PAT_INJ: &str = "INJ mixop";
pub(in crate::interface::meta) const PAT_CONS: &str = "CONS";
pub(in crate::interface::meta) const PAT_FIXED: &str = "FIXED nat";
pub(in crate::interface::meta) const PAT_NIL: &str = "NIL";
pub(in crate::interface::meta) const PAT_SOME: &str = "SOME";
pub(in crate::interface::meta) const PAT_NONE: &str = "NONE";

// == Expressions

pub(in crate::interface::meta) const TYPED_EXP: &str = "exp '#' typ";
pub(in crate::interface::meta) const EXP_BOOL: &str = "BOOL bool";
pub(in crate::interface::meta) const EXP_NAT: &str = "NAT nat";
pub(in crate::interface::meta) const EXP_INT: &str = "INT int";
pub(in crate::interface::meta) const EXP_TEXT: &str = "TEXT text";
pub(in crate::interface::meta) const EXP_VAR: &str = "VAR id";
pub(in crate::interface::meta) const EXP_UN: &str = "UN unOp typedExp";
pub(in crate::interface::meta) const EXP_BIN: &str = "BIN binOp typedExp typedExp";
pub(in crate::interface::meta) const EXP_CMP: &str = "CMP cmpOp typedExp typedExp";
pub(in crate::interface::meta) const EXP_UPCAST: &str = "UPCAST typ typedExp";
pub(in crate::interface::meta) const EXP_DOWNCAST: &str = "DOWNCAST typ typedExp";
pub(in crate::interface::meta) const EXP_SUB: &str = "SUB typedExp typ";
pub(in crate::interface::meta) const EXP_MATCH: &str = "MATCH typedExp pat";
pub(in crate::interface::meta) const EXP_TUP: &str = "TUP typedExp*";
pub(in crate::interface::meta) const EXP_INJ: &str = "INJ expCase";
pub(in crate::interface::meta) const EXP_STR: &str = "STR expField*";
pub(in crate::interface::meta) const EXP_OPT: &str = "OPT typedExp?";
pub(in crate::interface::meta) const EXP_LIST: &str = "LIST typedExp*";
pub(in crate::interface::meta) const EXP_CONS: &str = "CONS typedExp typedExp";
pub(in crate::interface::meta) const EXP_CAT: &str = "CAT typedExp typedExp";
pub(in crate::interface::meta) const EXP_MEM: &str = "MEM typedExp typedExp";
pub(in crate::interface::meta) const EXP_LEN: &str = "LEN typedExp";
pub(in crate::interface::meta) const EXP_DOT: &str = "DOT typedExp atom";
pub(in crate::interface::meta) const EXP_IDX: &str = "IDX typedExp typedExp";
pub(in crate::interface::meta) const EXP_SLICE: &str = "SLICE typedExp typedExp typedExp";
pub(in crate::interface::meta) const EXP_UPD: &str = "UPD typedExp typedPath typedExp";
pub(in crate::interface::meta) const EXP_CALL: &str = "CALL id targ* arg*";
pub(in crate::interface::meta) const EXP_ITER: &str = "ITER typedExp expIter";
pub(in crate::interface::meta) const EXP_CASE: &str = "mixop typedExp*";
pub(in crate::interface::meta) const EXP_FIELD: &str = "atom typedExp";
pub(in crate::interface::meta) const ITER_EXP: &str = "iter vari*";

// == Paths

pub(in crate::interface::meta) const TYPED_PATH: &str = "path '#' typ";
pub(in crate::interface::meta) const PATH_ROOT: &str = "ROOT";
pub(in crate::interface::meta) const PATH_IDX: &str = "IDX typedPath typedExp";
pub(in crate::interface::meta) const PATH_SLICE: &str = "SLICE typedPath typedExp typedExp";
pub(in crate::interface::meta) const PATH_DOT: &str = "DOT typedPath atom";

// == Arguments

pub(in crate::interface::meta) const ARG_EXP: &str = "EXP typedExp";
pub(in crate::interface::meta) const ARG_FUN: &str = "FUN id";

// == Parameters

pub(in crate::interface::meta) const PARAM_EXP: &str = "EXP typ";
pub(in crate::interface::meta) const PARAM_FUN: &str = "FUN id ':' tparam* param* '->' typ";

// == Premises

pub(in crate::interface::meta) const PREM_IF: &str = "IF typedExp";
pub(in crate::interface::meta) const PREM_IFHOLD: &str = "IFHOLD id ':' typedExp*";
pub(in crate::interface::meta) const PREM_IFNOTHOLD: &str = "IFNOTHOLD id ':' typedExp*";
pub(in crate::interface::meta) const PREM_DEBUG: &str = "DEBUG typedExp";

// == Definitions

pub(in crate::interface::meta) const DEF_EXTTYP: &str = "EXTTYP id";
pub(in crate::interface::meta) const DEF_TYP: &str = "TYP id tparam* '=' deftyp";
pub(in crate::interface::meta) const DEF_EXTFUNC: &str = "EXTFUNC id tparam* param* ':' typ";
pub(in crate::interface::meta) const DEF_BUILTINFUNC: &str =
    "BUILTINFUNC id tparam* param* ':' typ";
