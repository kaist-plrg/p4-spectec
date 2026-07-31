module Value = Runtime.Value

(* Pre-computed mixop constants shared between boot.ml and unboot.ml *)

(* Parameters *)

let mop_exp_param = Value.Mixops.of_string "EXP typ exp"
let mop_def_param = Value.Mixops.of_string "FUN id ':' tparam* param* '->' typ"

(* Iter instructions *)

let mop_iterinstr = Value.Mixops.of_string "iter vari* vari*"

(* Instructions *)

let mop_both_holdcase = Value.Mixops.of_string "BOTH block block"
let mop_hold_holdcase = Value.Mixops.of_string "HOLD block"
let mop_nothold_holdcase = Value.Mixops.of_string "NOTHOLD block"
let mop_bool_guard = Value.Mixops.of_string "BOOL bool"
let mop_cmp_guard = Value.Mixops.of_string "CMP cmpop exp"
let mop_sub_guard = Value.Mixops.of_string "SUB typ"
let mop_match_guard = Value.Mixops.of_string "MATCH pattern"
let mop_mem_guard = Value.Mixops.of_string "MEM exp"
let mop_case = Value.Mixops.of_string "guard block"
let mop_if_instr = Value.Mixops.of_string "IF exp iterexp* block"

let mop_hold_instr =
  Value.Mixops.of_string "IFHOLD id ':' exp* iterexp* holdcase"

let mop_case_instr = Value.Mixops.of_string "CASE exp case*"

let mop_group_instr =
  Value.Mixops.of_string "GROUP id exp* ':' typ* '->' typ* '='  block"

let mop_let_instr = Value.Mixops.of_string "LET exp '=' exp iterinstr* block"

let mop_rule_instr =
  Value.Mixops.of_string "REL id ':' exp* '->' exp* iterinstr* block"

let mop_result_instr = Value.Mixops.of_string "RESULT typ* '->' typ* ':' exp*"
let mop_return_instr = Value.Mixops.of_string "RETURN exp"
let mop_debug_instr = Value.Mixops.of_string "DEBUG exp instr"

(* Rule structure *)

let mop_tablerow = Value.Mixops.of_string "exp* '=' exp '-' block"

(* Definitions *)

let mop_extern_typ_def = Value.Mixops.of_string "EXTTYP id"
let mop_typ_def = Value.Mixops.of_string "TYP id tparam* '=' deftyp"

let mop_extern_rel_def =
  Value.Mixops.of_string "EXTREL id exp* ':' typ* '->' typ*"

let mop_rel_def =
  Value.Mixops.of_string "REL id exp* ':' typ* '->' typ* '=' block block?"

let mop_extern_func_def =
  Value.Mixops.of_string "EXTFUNC id tparam* param* ':' typ"

let mop_builtin_func_def =
  Value.Mixops.of_string "BUILTINFUNC id tparam* param* ':' typ"

let mop_table_func_def =
  Value.Mixops.of_string "TABLEFUNC id param* ':' typ '=' tblrow*"

let mop_func_def =
  Value.Mixops.of_string "FUNC id tparam* param* ':' typ '=' block block?"
