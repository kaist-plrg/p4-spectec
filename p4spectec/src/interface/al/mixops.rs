//! Constructor notation shared by AL encoders and decoders
//!
//! These shapes follow `spec-meta/1-syntax/1.0-common.watsup`,
//! `spec-meta/1-syntax/1.2-al.watsup`, and
//! `spec-meta/2-runtime/2.0-value.watsup` and are shared by the encoders and decoders.

// == Iterations and variables

pub(in crate::interface) const QUEST: &str = "QUEST";
pub(in crate::interface) const STAR: &str = "STAR";
pub(in crate::interface) const VARI: &str = "id typ iter*";

// == Types

pub(in crate::interface) const BOOL_TYP: &str = "BOOL";
pub(in crate::interface) const NAT_TYP: &str = "NAT";
pub(in crate::interface) const INT_TYP: &str = "INT";
pub(in crate::interface) const TEXT_TYP: &str = "TEXT";
pub(in crate::interface) const VAR_TYP: &str = "VAR id targ*";
pub(in crate::interface) const TUP_TYP: &str = "TUP typ*";
pub(in crate::interface) const ITER_TYP: &str = "ITER typ iter";
pub(in crate::interface) const FUNC_TYP: &str = "FUNC tparam* typ* typ";

// == Type definitions

pub(in crate::interface) const ALIAS: &str = "ALIAS typ";
pub(in crate::interface) const TYPFIELD: &str = "atom typ";
pub(in crate::interface) const STRUCT: &str = "STRUCT typField*";
pub(in crate::interface) const TYPCASE: &str = "mixop typ*";
pub(in crate::interface) const VARIANT: &str = "VARIANT typCase*";

// == Values

pub(in crate::interface) const BOOL: &str = "BOOL bool";
pub(in crate::interface) const NAT: &str = "NAT nat";
pub(in crate::interface) const INT: &str = "INT int";
pub(in crate::interface) const TEXT: &str = "TEXT text";
pub(in crate::interface) const VALFIELD: &str = "atom val";
pub(in crate::interface) const STR: &str = "STR valField*";
pub(in crate::interface) const VALCASE: &str = "mixop val*";
pub(in crate::interface) const INJ: &str = "INJ valCase";
pub(in crate::interface) const TUP: &str = "TUP val*";
pub(in crate::interface) const OPT: &str = "OPT val?";
pub(in crate::interface) const LIST: &str = "LIST val*";
pub(in crate::interface) const FUNC: &str = "FUNC id";
pub(in crate::interface) const EXT: &str = "EXT json";

// == Expressions and paths

pub(in crate::interface) const EXP: &str = "expKind '#' typ";
pub(in crate::interface) const PATH: &str = "pathKind '#' typ";
pub(in crate::interface) const VAR_EXP: &str = "VAR id";
pub(in crate::interface) const UN: &str = "UN unop exp";
pub(in crate::interface) const BIN: &str = "BIN binop exp exp";
pub(in crate::interface) const CMP: &str = "CMP cmpop exp exp";
pub(in crate::interface) const UPCAST: &str = "UPCAST typ exp";
pub(in crate::interface) const DOWNCAST: &str = "DOWNCAST typ exp";
pub(in crate::interface) const SUB_EXP: &str = "SUB exp typ";
pub(in crate::interface) const MATCH: &str = "MATCH exp pat";
pub(in crate::interface) const EXPCASE: &str = "mixop exp*";
pub(in crate::interface) const EXPFIELD: &str = "atom exp";
pub(in crate::interface) const CONS: &str = "CONS exp exp";
pub(in crate::interface) const CAT: &str = "CAT exp exp";
pub(in crate::interface) const MEM: &str = "MEM exp exp";
pub(in crate::interface) const LEN: &str = "LEN exp";
pub(in crate::interface) const DOT: &str = "DOT exp atom";
pub(in crate::interface) const IDX: &str = "IDX exp exp";
pub(in crate::interface) const SLICE: &str = "SLICE exp exp exp";
pub(in crate::interface) const UPD: &str = "UPD exp path exp";
pub(in crate::interface) const CALL: &str = "CALL id targ* arg*";
pub(in crate::interface) const ITER_EXP: &str = "ITER exp iterExp";
pub(in crate::interface) const ITEREXP: &str = "iter vari*";
pub(in crate::interface) const ROOT: &str = "ROOT";

// == Patterns

pub(in crate::interface) const CASE_PATTERN: &str = "INJ mixop";
pub(in crate::interface) const CONS_PATTERN: &str = "CONS";
pub(in crate::interface) const FIXED: &str = "FIXED nat";
pub(in crate::interface) const NIL: &str = "NIL";
pub(in crate::interface) const SOME: &str = "SOME";
pub(in crate::interface) const NONE: &str = "NONE";

// == Arguments

pub(in crate::interface) const EXP_ARG: &str = "EXP exp";
pub(in crate::interface) const FUN_ARG: &str = "FUN id";

// == Operators

pub(in crate::interface) const NOT: &str = "NOT";
pub(in crate::interface) const PLUS: &str = "PLUS";
pub(in crate::interface) const MINUS: &str = "MINUS";
pub(in crate::interface) const AND: &str = "AND";
pub(in crate::interface) const OR: &str = "OR";
pub(in crate::interface) const IMPL: &str = "IMPL";
pub(in crate::interface) const EQUIV: &str = "EQUIV";
pub(in crate::interface) const ADD: &str = "ADD";
pub(in crate::interface) const SUB: &str = "SUB";
pub(in crate::interface) const MUL: &str = "MUL";
pub(in crate::interface) const DIV: &str = "DIV";
pub(in crate::interface) const MOD: &str = "MOD";
pub(in crate::interface) const POW: &str = "POW";
pub(in crate::interface) const EQ: &str = "EQ";
pub(in crate::interface) const NE: &str = "NE";
pub(in crate::interface) const LT: &str = "LT";
pub(in crate::interface) const LE: &str = "LE";
pub(in crate::interface) const GT: &str = "GT";
pub(in crate::interface) const GE: &str = "GE";

// == Parameters

pub(in crate::interface) const EXP_PARAM: &str = "EXP typ";
pub(in crate::interface) const FUN_PARAM: &str = "FUN id ':' tparam* param* '->' typ";

// == Premises

pub(in crate::interface) const ITERPREM: &str = "iter '<-' vari* '->' vari*";
pub(in crate::interface) const REL_PREM: &str = "REL id ':' exp* '->' exp*";
pub(in crate::interface) const IF: &str = "IF exp";
pub(in crate::interface) const IFHOLD: &str = "IFHOLD id ':' exp*";
pub(in crate::interface) const IFNOTHOLD: &str = "IFNOTHOLD id ':' exp*";
pub(in crate::interface) const LET: &str = "LET exp '=' exp";
pub(in crate::interface) const ITER_PREM: &str = "ITER prem iterPrem";
pub(in crate::interface) const DEBUG: &str = "DEBUG exp";

// == Rules and clauses

pub(in crate::interface) const RULMATCH: &str = "exp* '-' prem*";
pub(in crate::interface) const RULPATH: &str = "id '=' exp* '-' prem*";
pub(in crate::interface) const RUL: &str = "rulMatch '->' rulPath";
pub(in crate::interface) const RULGROUP: &str = "id ':' rul*";
pub(in crate::interface) const ELSGROUP: &str = "id ':' rul";
pub(in crate::interface) const CLAUSE: &str = "arg* '=' exp '-' prem*";

// == Definitions

pub(in crate::interface) const EXTTYP: &str = "EXTTYP id";
pub(in crate::interface) const TYP_DEF: &str = "TYP id tparam* '=' deftyp";
pub(in crate::interface) const EXTREL: &str = "EXTREL id ':' typ* '->' typ*";
pub(in crate::interface) const REL_DEF: &str = "REL id ':' typ* '->' typ* '=' rulGroup* elsGroup?";
pub(in crate::interface) const EXTFUNC: &str = "EXTFUNC id tparam* param* ':' typ";
pub(in crate::interface) const BUILTINFUNC: &str = "BUILTINFUNC id tparam* param* ':' typ";
pub(in crate::interface) const TABLEFUNC: &str = "TABLEFUNC id param* ':' typ '=' tblRow*";
pub(in crate::interface) const FUNC_DEF: &str =
    "FUNC id tparam* param* ':' typ '=' clause* elsClause?";

// == Results

pub(in crate::interface) const OK: &str = "OK val";
