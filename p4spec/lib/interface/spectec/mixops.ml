module Value = Runtime.Value

(* Pre-computed mixop constants shared between boot.ml and unboot.ml *)

(* Iterators *)

let mop_quest = Value.Mixops.of_string "QUEST"
let mop_star = Value.Mixops.of_string "STAR"

(* Variables *)

let mop_vari = Value.Mixops.of_string "id typ iter*"

(* Types *)

let mop_bool_typ = Value.Mixops.of_string "BOOL"
let mop_num_typ_nat = Value.Mixops.of_string "NAT"
let mop_num_typ_int = Value.Mixops.of_string "INT"
let mop_text_typ = Value.Mixops.of_string "TEXT"
let mop_var_typ = Value.Mixops.of_string "VAR id targ*"
let mop_tuple_typ = Value.Mixops.of_string "TUP typ*"
let mop_iter_typ = Value.Mixops.of_string "ITER typ iter"
let mop_func_typ = Value.Mixops.of_string "FUNC"

(* Defined types *)

let mop_plain_deftyp = Value.Mixops.of_string "ALIAS typ"
let mop_typfield = Value.Mixops.of_string "atom typ"
let mop_struct_deftyp = Value.Mixops.of_string "STRUCT typfield*"
let mop_typcase = Value.Mixops.of_string "mixop typ*"
let mop_variant_deftyp = Value.Mixops.of_string "VARIANT typcase*"

(* Values *)

let mop_bool_value = Value.Mixops.of_string "BOOL bool"
let mop_num_value_nat = Value.Mixops.of_string "NAT nat"
let mop_num_value_int = Value.Mixops.of_string "INT int"
let mop_text_value = Value.Mixops.of_string "TEXT text"
let mop_valuefield = Value.Mixops.of_string "atom val"
let mop_struct_value = Value.Mixops.of_string "STR valfield*"
let mop_valuecase = Value.Mixops.of_string "mixop val*"
let mop_case_value = Value.Mixops.of_string "INJ valcase"
let mop_tuple_value = Value.Mixops.of_string "TUP val*"
let mop_opt_value = Value.Mixops.of_string "OPT val?"
let mop_list_value = Value.Mixops.of_string "LIST val*"
let mop_func_value = Value.Mixops.of_string "FUNC id"
let mop_extern_value = Value.Mixops.of_string "EXT json"

(* Unary operators *)

let mop_not_unop = Value.Mixops.of_string "NOT"
let mop_plus_unop = Value.Mixops.of_string "PLUS"
let mop_minus_unop = Value.Mixops.of_string "MINUS"

(* Binary operators *)

let mop_and_binop = Value.Mixops.of_string "AND"
let mop_or_binop = Value.Mixops.of_string "OR"
let mop_impl_binop = Value.Mixops.of_string "IMPL"
let mop_equiv_binop = Value.Mixops.of_string "EQUIV"
let mop_add_binop = Value.Mixops.of_string "ADD"
let mop_sub_binop = Value.Mixops.of_string "SUB"
let mop_mul_binop = Value.Mixops.of_string "MUL"
let mop_div_binop = Value.Mixops.of_string "DIV"
let mop_mod_binop = Value.Mixops.of_string "MOD"
let mop_pow_binop = Value.Mixops.of_string "POW"

(* Comparison operators *)

let mop_eq_cmpop = Value.Mixops.of_string "EQ"
let mop_ne_cmpop = Value.Mixops.of_string "NE"
let mop_lt_cmpop = Value.Mixops.of_string "LT"
let mop_le_cmpop = Value.Mixops.of_string "LE"
let mop_gt_cmpop = Value.Mixops.of_string "GT"
let mop_ge_cmpop = Value.Mixops.of_string "GE"

(* Parameters and arguments *)

let mop_exp_paramIL = Value.Mixops.of_string "EXP typ"

let mop_def_paramIL =
  Value.Mixops.of_string "FUN id `: tparam* paramIL* `-> typ"

let mop_exp_arg = Value.Mixops.of_string "EXP exp"
let mop_def_arg = Value.Mixops.of_string "FUN id"

(* Expressions *)

let mop_bool_exp = mop_bool_value
let mop_text_exp = mop_text_value
let mop_var_exp = Value.Mixops.of_string "VAR id"
let mop_un_exp = Value.Mixops.of_string "UN unop exp"
let mop_bin_exp = Value.Mixops.of_string "BIN binop exp exp"
let mop_cmp_exp = Value.Mixops.of_string "CMP cmpop exp exp"
let mop_upcast_exp = Value.Mixops.of_string "UPCAST typ exp"
let mop_downcast_exp = Value.Mixops.of_string "DOWNCAST typ exp"
let mop_sub_exp = Value.Mixops.of_string "SUB exp typ"
let mop_match_exp = Value.Mixops.of_string "MATCH exp pattern"
let mop_tuple_exp = Value.Mixops.of_string "TUP exp*"
let mop_expcase = Value.Mixops.of_string "mixop exp*"
let mop_case_exp = Value.Mixops.of_string "INJ expcase"
let mop_iter_exp = Value.Mixops.of_string "ITER exp iterexp"
let mop_expfield = Value.Mixops.of_string "atom exp"
let mop_struct_exp = Value.Mixops.of_string "STR expfield*"
let mop_opt_exp = Value.Mixops.of_string "OPT exp?"
let mop_list_exp = Value.Mixops.of_string "LIST exp*"
let mop_cons_exp = Value.Mixops.of_string "CONS exp exp"
let mop_cat_exp = Value.Mixops.of_string "CAT exp exp"
let mop_mem_exp = Value.Mixops.of_string "MEM exp exp"
let mop_len_exp = Value.Mixops.of_string "LEN exp"
let mop_dot_exp = Value.Mixops.of_string "DOT exp atom"
let mop_idx_exp = Value.Mixops.of_string "IDX exp exp"
let mop_slice_exp = Value.Mixops.of_string "SLICE exp exp exp"
let mop_upd_exp = Value.Mixops.of_string "UPD exp path exp"
let mop_call_exp = Value.Mixops.of_string "CALL id targ* arg*"

(* Paths *)

let mop_root_path = Value.Mixops.of_string "ROOT"
let mop_idx_path = Value.Mixops.of_string "IDX path exp"
let mop_slice_path = Value.Mixops.of_string "SLICE path exp exp"
let mop_dot_path = Value.Mixops.of_string "DOT path atom"

(* Patterns *)

let mop_case_pattern = Value.Mixops.of_string "INJ mixop"
let mop_list_cons_pattern = Value.Mixops.of_string "CONS"
let mop_list_fixed_pattern = Value.Mixops.of_string "FIXED nat"
let mop_list_nil_pattern = Value.Mixops.of_string "NIL"
let mop_opt_some_pattern = Value.Mixops.of_string "SOME"
let mop_opt_none_pattern = Value.Mixops.of_string "NONE"

(* Iter expressions and premises *)

let mop_iterexp = Value.Mixops.of_string "iter vari*"
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
