//! Constructor notation for syntax shared by IL and AL
//!
//! These shapes follow `spec-meta/1-syntax/1.0-common.watsup`;
//! literal and aggregate shapes are also those of runtime values
//! in `spec-meta/2-runtime/2.0-value.watsup`.

// == Iterations and variables

pub(in crate::interface::meta) const QUEST: &str = "QUEST";
pub(in crate::interface::meta) const STAR: &str = "STAR";
pub(in crate::interface::meta) const VARI: &str = "id typ iter*";

// == Types

pub(in crate::interface::meta) const BOOL_TYP: &str = "BOOL";
pub(in crate::interface::meta) const NAT_TYP: &str = "NAT";
pub(in crate::interface::meta) const INT_TYP: &str = "INT";
pub(in crate::interface::meta) const TEXT_TYP: &str = "TEXT";
pub(in crate::interface::meta) const VAR_TYP: &str = "VAR id targ*";
pub(in crate::interface::meta) const TUP_TYP: &str = "TUP typ*";
pub(in crate::interface::meta) const ITER_TYP: &str = "ITER typ iter";
pub(in crate::interface::meta) const FUNC_TYP: &str = "FUNC tparam* typ* typ";

// == Type definitions

pub(in crate::interface::meta) const ALIAS: &str = "ALIAS typ";
pub(in crate::interface::meta) const TYPFIELD: &str = "atom typ";
pub(in crate::interface::meta) const STRUCT: &str = "STRUCT typField*";
pub(in crate::interface::meta) const TYPCASE: &str = "mixop typ*";
pub(in crate::interface::meta) const VARIANT: &str = "VARIANT typCase*";

// == Literals and aggregates of expressions and values

pub(in crate::interface::meta) const BOOL: &str = "BOOL bool";
pub(in crate::interface::meta) const NAT: &str = "NAT nat";
pub(in crate::interface::meta) const INT: &str = "INT int";
pub(in crate::interface::meta) const TEXT: &str = "TEXT text";
pub(in crate::interface::meta) const STR: &str = "STR valField*";
pub(in crate::interface::meta) const INJ: &str = "INJ valCase";
pub(in crate::interface::meta) const TUP: &str = "TUP val*";
pub(in crate::interface::meta) const OPT: &str = "OPT val?";
pub(in crate::interface::meta) const LIST: &str = "LIST val*";

// == Expressions and paths

pub(in crate::interface::meta) const EXP: &str = "exp '#' typ";
pub(in crate::interface::meta) const PATH: &str = "path '#' typ";
pub(in crate::interface::meta) const VAR_EXP: &str = "VAR id";
pub(in crate::interface::meta) const UN: &str = "UN unop typedExp";
pub(in crate::interface::meta) const BIN: &str = "BIN binop typedExp typedExp";
pub(in crate::interface::meta) const CMP: &str = "CMP cmpop typedExp typedExp";
pub(in crate::interface::meta) const UPCAST: &str = "UPCAST typ typedExp";
pub(in crate::interface::meta) const DOWNCAST: &str = "DOWNCAST typ typedExp";
pub(in crate::interface::meta) const SUB_EXP: &str = "SUB typedExp typ";
pub(in crate::interface::meta) const MATCH: &str = "MATCH typedExp pat";
pub(in crate::interface::meta) const EXPCASE: &str = "mixop typedExp*";
pub(in crate::interface::meta) const EXPFIELD: &str = "atom typedExp";
pub(in crate::interface::meta) const CONS: &str = "CONS typedExp typedExp";
pub(in crate::interface::meta) const CAT: &str = "CAT typedExp typedExp";
pub(in crate::interface::meta) const MEM: &str = "MEM typedExp typedExp";
pub(in crate::interface::meta) const LEN: &str = "LEN typedExp";
pub(in crate::interface::meta) const DOT: &str = "DOT typedExp atom";
pub(in crate::interface::meta) const IDX: &str = "IDX typedExp typedExp";
pub(in crate::interface::meta) const SLICE: &str = "SLICE typedExp typedExp typedExp";
pub(in crate::interface::meta) const UPD: &str = "UPD typedExp typedPath typedExp";
pub(in crate::interface::meta) const CALL: &str = "CALL id targ* arg*";
pub(in crate::interface::meta) const ITER_EXP: &str = "ITER typedExp iterExp";
pub(in crate::interface::meta) const ITEREXP: &str = "iter vari*";
pub(in crate::interface::meta) const ROOT: &str = "ROOT";

// == Patterns

pub(in crate::interface::meta) const CASE_PATTERN: &str = "INJ mixop";
pub(in crate::interface::meta) const CONS_PATTERN: &str = "CONS";
pub(in crate::interface::meta) const FIXED: &str = "FIXED nat";
pub(in crate::interface::meta) const NIL: &str = "NIL";
pub(in crate::interface::meta) const SOME: &str = "SOME";
pub(in crate::interface::meta) const NONE: &str = "NONE";

// == Arguments

pub(in crate::interface::meta) const EXP_ARG: &str = "EXP typedExp";
pub(in crate::interface::meta) const FUN_ARG: &str = "FUN id";

// == Operators

pub(in crate::interface::meta) const NOT: &str = "NOT";
pub(in crate::interface::meta) const PLUS: &str = "PLUS";
pub(in crate::interface::meta) const MINUS: &str = "MINUS";
pub(in crate::interface::meta) const AND: &str = "AND";
pub(in crate::interface::meta) const OR: &str = "OR";
pub(in crate::interface::meta) const IMPL: &str = "IMPL";
pub(in crate::interface::meta) const EQUIV: &str = "EQUIV";
pub(in crate::interface::meta) const ADD: &str = "ADD";
pub(in crate::interface::meta) const SUB: &str = "SUB";
pub(in crate::interface::meta) const MUL: &str = "MUL";
pub(in crate::interface::meta) const DIV: &str = "DIV";
pub(in crate::interface::meta) const MOD: &str = "MOD";
pub(in crate::interface::meta) const POW: &str = "POW";
pub(in crate::interface::meta) const EQ: &str = "EQ";
pub(in crate::interface::meta) const NE: &str = "NE";
pub(in crate::interface::meta) const LT: &str = "LT";
pub(in crate::interface::meta) const LE: &str = "LE";
pub(in crate::interface::meta) const GT: &str = "GT";
pub(in crate::interface::meta) const GE: &str = "GE";

// == Parameters

pub(in crate::interface::meta) const EXP_PARAM: &str = "EXP typ";
pub(in crate::interface::meta) const FUN_PARAM: &str = "FUN id ':' tparam* param* '->' typ";

// == Premises

pub(in crate::interface::meta) const IF: &str = "IF typedExp";
pub(in crate::interface::meta) const IFHOLD: &str = "IFHOLD id ':' typedExp*";
pub(in crate::interface::meta) const IFNOTHOLD: &str = "IFNOTHOLD id ':' typedExp*";
pub(in crate::interface::meta) const ITER_PREM: &str = "ITER prem iterPrem";
pub(in crate::interface::meta) const DEBUG: &str = "DEBUG typedExp";

// == Definitions

pub(in crate::interface::meta) const EXTTYP: &str = "EXTTYP id";
pub(in crate::interface::meta) const TYP_DEF: &str = "TYP id tparam* '=' deftyp";
pub(in crate::interface::meta) const EXTFUNC: &str = "EXTFUNC id tparam* param* ':' typ";
pub(in crate::interface::meta) const BUILTINFUNC: &str = "BUILTINFUNC id tparam* param* ':' typ";
