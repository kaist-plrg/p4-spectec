module Value = Runtime.Value

(* Pre-computed mixop constants shared between boot.ml and unboot.ml *)

(* Parameters *)

let mop_exp_paramIL = Value.Mixops.of_string "EXP typ"

let mop_def_paramIL =
  Value.Mixops.of_string "FUN id `: tparam* paramIL* `-> typ"

(* Iter premises *)

let mop_iterprem = Value.Mixops.of_string "iter vari* vari*"

(* Premises *)

let mop_rel_prem = Value.Mixops.of_string "REL id `: exp* `-> exp*"
let mop_if_prem = Value.Mixops.of_string "IF exp"
let mop_if_hold_prem = Value.Mixops.of_string "IFHOLD id `: exp*"
let mop_if_nothold_prem = Value.Mixops.of_string "IFNOTHOLD id `: exp*"
let mop_let_prem = Value.Mixops.of_string "LET exp `= exp"
let mop_iter_prem = Value.Mixops.of_string "ITER prem iterprem"
let mop_debug_prem = Value.Mixops.of_string "DEBUG exp"

(* Rule structure *)

let mop_rulematch = Value.Mixops.of_string "exp* `- prem*"
let mop_rulepath = Value.Mixops.of_string "id `= exp* `- prem*"
let mop_rulegroup = Value.Mixops.of_string "id `: rulmatch `= rulpath*"
let mop_elsegroup = Value.Mixops.of_string "id `: rulmatch `= rulpath"
let mop_clause = Value.Mixops.of_string "arg* `= exp `- prem*"
let mop_tablerowIL = Value.Mixops.of_string "arg* `= exp `- prem*"

(* Definitions *)

let mop_extern_typ_defIL = Value.Mixops.of_string "EXTTYP id"
let mop_typ_defIL = Value.Mixops.of_string "TYP id tparam* `= deftyp"
let mop_extern_rel_defIL = Value.Mixops.of_string "EXTREL id `: typ* `-> typ*"

let mop_rel_defIL =
  Value.Mixops.of_string "REL id `: typ* `-> typ* `= rulgroup* elsgroup?"

let mop_extern_func_defIL =
  Value.Mixops.of_string "EXTFUNC id tparam* paramIL* `: typ"

let mop_builtin_func_defIL =
  Value.Mixops.of_string "BUILTINFUNC id tparam* paramIL* `: typ"

let mop_table_func_defIL =
  Value.Mixops.of_string "TABLEFUNC id paramIL* `: typ `= tblrowIL*"

let mop_func_defIL =
  Value.Mixops.of_string "FUNC id tparam* paramIL* `: typ `= clause* elsclause?"
