//! Constructor notation shared by AL encoders and decoders
//!
//! These shapes follow `spec-meta/1-syntax.watsup` and are shared by the
//! encoders and decoders.

// == Iterations and variables

pub(super) const QUEST: &str = "QUEST";
pub(super) const STAR: &str = "STAR";
pub(super) const VARI: &str = "id typ iter*";

// == Types

pub(super) const BOOL_TYP: &str = "BOOL";
pub(super) const NAT_TYP: &str = "NAT";
pub(super) const INT_TYP: &str = "INT";
pub(super) const TEXT_TYP: &str = "TEXT";
pub(super) const VAR_TYP: &str = "VAR id targ*";
pub(super) const TUP_TYP: &str = "TUP typ*";
pub(super) const ITER_TYP: &str = "ITER typ iter";
pub(super) const FUNC_TYP: &str = "FUNC tparam* typ* typ";

// == Type definitions

pub(super) const ALIAS: &str = "ALIAS typ";
pub(super) const TYPFIELD: &str = "atom typ";
pub(super) const STRUCT: &str = "STRUCT typField*";
pub(super) const TYPCASE: &str = "mixop typ*";
pub(super) const VARIANT: &str = "VARIANT typCase*";

// == Values

pub(super) const BOOL: &str = "BOOL bool";
pub(super) const NAT: &str = "NAT nat";
pub(super) const INT: &str = "INT int";
pub(super) const TEXT: &str = "TEXT text";
pub(super) const VALFIELD: &str = "atom val";
pub(super) const STR: &str = "STR valField*";
pub(super) const VALCASE: &str = "mixop val*";
pub(super) const INJ: &str = "INJ valCase";
pub(super) const TUP: &str = "TUP val*";
pub(super) const OPT: &str = "OPT val?";
pub(super) const LIST: &str = "LIST val*";
pub(super) const FUNC: &str = "FUNC id";
pub(super) const EXT: &str = "EXT json";

// == Expressions and paths

pub(super) const VAR_EXP: &str = "VAR id";
pub(super) const UN: &str = "UN unop exp";
pub(super) const BIN: &str = "BIN binop exp exp";
pub(super) const CMP: &str = "CMP cmpop exp exp";
pub(super) const UPCAST: &str = "UPCAST typ exp";
pub(super) const DOWNCAST: &str = "DOWNCAST typ exp";
pub(super) const SUB_EXP: &str = "SUB exp typ";
pub(super) const MATCH: &str = "MATCH exp pattern";
pub(super) const EXPCASE: &str = "mixop exp*";
pub(super) const EXPFIELD: &str = "atom exp";
pub(super) const CONS: &str = "CONS exp exp";
pub(super) const CAT: &str = "CAT exp exp";
pub(super) const MEM: &str = "MEM exp exp";
pub(super) const LEN: &str = "LEN exp";
pub(super) const DOT: &str = "DOT exp atom";
pub(super) const IDX: &str = "IDX exp exp";
pub(super) const SLICE: &str = "SLICE exp exp exp";
pub(super) const UPD: &str = "UPD exp path exp";
pub(super) const CALL: &str = "CALL id targ* arg*";
pub(super) const ITER_EXP: &str = "ITER exp iterExp";
pub(super) const ITEREXP: &str = "iter vari*";
pub(super) const ROOT: &str = "ROOT";

// == Patterns

pub(super) const CASE_PATTERN: &str = "INJ mixop";
pub(super) const CONS_PATTERN: &str = "CONS";
pub(super) const FIXED: &str = "FIXED nat";
pub(super) const NIL: &str = "NIL";
pub(super) const SOME: &str = "SOME";
pub(super) const NONE: &str = "NONE";

// == Arguments

pub(super) const EXP_ARG: &str = "EXP exp";
pub(super) const FUN_ARG: &str = "FUN id";

// == Operators

pub(super) const NOT: &str = "NOT";
pub(super) const PLUS: &str = "PLUS";
pub(super) const MINUS: &str = "MINUS";
pub(super) const AND: &str = "AND";
pub(super) const OR: &str = "OR";
pub(super) const IMPL: &str = "IMPL";
pub(super) const EQUIV: &str = "EQUIV";
pub(super) const ADD: &str = "ADD";
pub(super) const SUB: &str = "SUB";
pub(super) const MUL: &str = "MUL";
pub(super) const DIV: &str = "DIV";
pub(super) const MOD: &str = "MOD";
pub(super) const POW: &str = "POW";
pub(super) const EQ: &str = "EQ";
pub(super) const NE: &str = "NE";
pub(super) const LT: &str = "LT";
pub(super) const LE: &str = "LE";
pub(super) const GT: &str = "GT";
pub(super) const GE: &str = "GE";

// == Parameters

pub(super) const EXP_PARAM: &str = "EXP typ";
pub(super) const FUN_PARAM: &str = "FUN id ':' tparam* param* '->' typ";

// == Premises

pub(super) const ITERPREM: &str = "iter vari* vari*";
pub(super) const REL_PREM: &str = "REL id ':' exp* '->' exp*";
pub(super) const IF: &str = "IF exp";
pub(super) const IFHOLD: &str = "IFHOLD id ':' exp*";
pub(super) const IFNOTHOLD: &str = "IFNOTHOLD id ':' exp*";
pub(super) const LET: &str = "LET exp '=' exp";
pub(super) const ITER_PREM: &str = "ITER prem iterPrem";
pub(super) const DEBUG: &str = "DEBUG exp";

// == Rules and clauses

pub(super) const RULMATCH: &str = "exp* '-' prem*";
pub(super) const RULPATH: &str = "id '=' exp* '-' prem*";
pub(super) const RULGROUP: &str = "id ':' rulMatch '=' rulPath*";
pub(super) const ELSGROUP: &str = "id ':' rulMatch '=' rulPath";
pub(super) const CLAUSE: &str = "arg* '=' exp '-' prem*";

// == Definitions

pub(super) const EXTTYP: &str = "EXTTYP id";
pub(super) const TYP_DEF: &str = "TYP id tparam* '=' deftyp";
pub(super) const EXTREL: &str = "EXTREL id ':' typ* '->' typ*";
pub(super) const REL_DEF: &str = "REL id ':' typ* '->' typ* '=' rulGroup* elsGroup?";
pub(super) const EXTFUNC: &str = "EXTFUNC id tparam* param* ':' typ";
pub(super) const BUILTINFUNC: &str = "BUILTINFUNC id tparam* param* ':' typ";
pub(super) const TABLEFUNC: &str = "TABLEFUNC id param* ':' typ '=' tblRow*";
pub(super) const FUNC_DEF: &str = "FUNC id tparam* param* ':' typ '=' clause* elsClause?";

// == Results

pub(super) const OK: &str = "OK val";
