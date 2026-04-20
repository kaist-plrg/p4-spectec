open Domain
open Lang
open Xl
module Typ = Runtime.Type.Typ
module Value = Runtime.Value
open Util.Source

(* Identifiers *)

let boot_id (id : Il.id) : Value.t =
  let value_id = Value.Make.text ~at:id.at id.it in
  Value.Make.(value_id #@@ "id")

(* Atoms *)

let boot_atom (atom : Il.atom) : Value.t =
  let value_atom =
    atom.it |> Atom.raw_string_of_atom |> Value.Make.text ~at:atom.at
  in
  Value.Make.(value_atom #@@ "atom")

(* Mixifx operators *)

let boot_mixop (mixop : Il.mixop) : Value.t =
  let atoms_matrix = Mixop.atoms_matrix mixop in
  let values_atoms =
    List.map
      (fun atoms ->
        let values_atoms = List.map boot_atom atoms in
        let typ_atoms = Typ.Make.var ("atom" $ no_region) [] |> Typ.Make.list in
        Value.Make.list typ_atoms values_atoms)
      atoms_matrix
  in
  let value_atoms_matrix =
    let typ_atoms_matrix =
      Typ.Make.var ("atom" $ no_region) [] |> Typ.Make.list
    in
    Value.Make.list typ_atoms_matrix values_atoms
  in
  Value.Make.(value_atoms_matrix #@@ "mixop")

(* Iterators *)

let boot_iter (iter : Il.iter) : Value.t =
  match iter with
  | Opt -> Value.Make.("QUEST" <| [] <<| "iter" <<<| no_region)
  | List -> Value.Make.("STAR" <| [] <<| "iter" <<<| no_region)

let boot_iters (iters : Il.iter list) : Value.t =
  let values_iters = List.map boot_iter iters in
  let typ_iters = Typ.Make.var ("iter" $ no_region) [] |> Typ.Make.list in
  Value.Make.list typ_iters values_iters

(* Variables *)

let rec boot_var (var : Il.var) : Value.t =
  let id, typ, iters = var in
  let value_id = boot_id id in
  let value_typ = boot_typ typ in
  let value_iters = boot_iters iters in
  Value.Make.(
    "id typ iter*"
    <| [ value_id; value_typ; value_iters ]
    <<| "vari" <<<| no_region)

and boot_vars (vars : Il.var list) : Value.t =
  let values_vars = List.map boot_var vars in
  let typ_vars = Typ.Make.var ("vari" $ no_region) [] |> Typ.Make.list in
  Value.Make.list typ_vars values_vars

(* Types *)

and boot_typ (typ : Il.typ) : Value.t =
  let at = typ.at in
  match typ.it with
  | BoolT -> boot_bool_typ at
  | NumT numtyp -> boot_num_typ at numtyp
  | TextT -> boot_text_typ at
  | VarT (id, targs) -> boot_var_typ at id targs
  | TupleT typs -> boot_tuple_typ at typs
  | IterT (typ, iter) -> boot_iter_typ at typ iter
  | FuncT (_, _, _) -> boot_func_typ at

and boot_typs (typs : Il.typ list) : Value.t =
  let values_typs = List.map boot_typ typs in
  let typ_typs = Typ.Make.var ("typ" $ no_region) [] |> Typ.Make.list in
  Value.Make.list typ_typs values_typs

and boot_bool_typ (at : region) : Value.t =
  Value.Make.("BOOL" <| [] <<| "optyp" <<<| at)

and boot_num_typ (at : region) (numtyp : Num.typ) : Value.t =
  match numtyp with
  | `NatT -> Value.Make.("NAT" <| [] <<| "numtyp" <<<| at)
  | `IntT -> Value.Make.("INT" <| [] <<| "numtyp" <<<| at)

and boot_text_typ (at : region) : Value.t =
  Value.Make.("TEXT" <| [] <<| "optyp" <<<| at)

and boot_var_typ (at : region) (id : Il.id) (targs : Il.targ list) : Value.t =
  let value_id = boot_id id in
  let value_targs = boot_targs targs in
  Value.Make.("VAR id targ*" <| [ value_id; value_targs ] <<| "typ" <<<| at)

and boot_tuple_typ (at : region) (typs : Il.typ list) : Value.t =
  let value_typs = boot_typs typs in
  Value.Make.("TUP typ*" <| [ value_typs ] <<| "typ" <<<| at)

and boot_iter_typ (at : region) (typ : Il.typ) (iter : Il.iter) : Value.t =
  let value_typ = boot_typ typ in
  let value_iter = boot_iter iter in
  Value.Make.("ITER typ iter" <| [ value_typ; value_iter ] <<| "typ" <<<| at)

and boot_func_typ (at : region) : Value.t =
  Value.Make.("FUNC" <| [] <<| "typ" <<<| at)

(* Defined types *)

and boot_deftyp (deftyp : Il.deftyp) : Value.t =
  let at = deftyp.at in
  match deftyp.it with
  | PlainT typ -> boot_plain_deftyp at typ
  | StructT typfields -> boot_struct_deftyp at typfields
  | VariantT typcases -> boot_variant_deftyp at typcases

and boot_plain_deftyp (at : region) (typ : Il.typ) : Value.t =
  let value_typ = boot_typ typ in
  Value.Make.("ALIAS typ" <| [ value_typ ] <<| "deftyp" <<<| at)

and boot_typfield (typfield : Il.typfield) : Value.t =
  let atom, typ = typfield in
  let value_atom = boot_atom atom in
  let typ = boot_typ typ in
  Value.Make.("atom typ" <| [ value_atom; typ ] <<| "typfield" <<<| no_region)

and boot_typfields (typfields : Il.typfield list) : Value.t =
  let values_typfields = List.map boot_typfield typfields in
  let typ_typfields =
    Typ.Make.var ("typfield" $ no_region) [] |> Typ.Make.list
  in
  Value.Make.list typ_typfields values_typfields

and boot_struct_deftyp (at : region) (typfields : Il.typfield list) : Value.t =
  let value_typfields = boot_typfields typfields in
  Value.Make.("STRUCT typfield*" <| [ value_typfields ] <<| "deftyp" <<<| at)

and boot_typcase (typcase : Il.typcase) : Value.t =
  let nottyp, _, _ = typcase in
  let mixop, typs = nottyp.it in
  let value_mixop = boot_mixop mixop in
  let value_typs = boot_typs typs in
  Value.Make.(
    "mixop typ*" <| [ value_mixop; value_typs ] <<| "typcase" <<<| nottyp.at)

and boot_typcases (typcases : Il.typcase list) : Value.t =
  let values_typcases = List.map boot_typcase typcases in
  let typ_typcases = Typ.Make.var ("typcase" $ no_region) [] |> Typ.Make.list in
  Value.Make.list typ_typcases values_typcases

and boot_variant_deftyp (at : region) (typcases : Il.typcase list) : Value.t =
  let value_typcases = boot_typcases typcases in
  Value.Make.("VARIANT typcase*" <| [ value_typcases ] <<| "deftyp" <<<| at)

(* Values *)

and boot_value (value : Il.value) : Value.t =
  let at = value.at in
  match value.it with
  | BoolV b -> boot_bool_value at b
  | NumV num -> boot_num_value at num
  | TextV t -> boot_text_value at t
  | StructV valuefields -> boot_struct_value at valuefields
  | CaseV valuecase -> boot_case_value at valuecase
  | TupleV values -> boot_tuple_value at values
  | OptV value_opt -> boot_opt_value at value_opt
  | ListV values -> boot_list_value at values
  | FuncV id -> boot_func_value at id
  | ExternV json -> boot_extern_value at json

and boot_value_opt (value_opt : Il.value option) : Value.t =
  let typ_value_opt = Typ.Make.var ("val" $ no_region) [] |> Typ.Make.opt in
  Value.Make.opt typ_value_opt (Option.map boot_value value_opt)

and boot_values (values : Il.value list) : Value.t =
  let values_values = List.map boot_value values in
  let typ_values = Typ.Make.var ("val" $ no_region) [] |> Typ.Make.list in
  Value.Make.list typ_values values_values

and boot_bool_value (at : region) (b : bool) : Value.t =
  Value.Make.("BOOL bool" <| [ bool b ] <<| "val" <<<| at)

and boot_num_value (at : region) (num : Il.num) : Value.t =
  match num with
  | `Nat n -> Value.Make.("NAT nat" <| [ nat n ] <<| "num" <<<| at)
  | `Int i -> Value.Make.("INT int" <| [ int i ] <<| "num" <<<| at)

and boot_text_value (at : region) (t : string) : Value.t =
  Value.Make.("TEXT text" <| [ text t ] <<| "val" <<<| at)

and boot_valuefield (valuefield : Il.valuefield) : Value.t =
  let atom, value = valuefield in
  let value_atom = boot_atom atom in
  let value_value = boot_value value in
  Value.Make.(
    "atom val" <| [ value_atom; value_value ] <<| "valfield" <<<| no_region)

and boot_valuefields (valuefields : Il.valuefield list) : Value.t =
  let values_valuefields = List.map boot_valuefield valuefields in
  let typ_valuefields =
    Typ.Make.var ("valfield" $ no_region) [] |> Typ.Make.list
  in
  Value.Make.list typ_valuefields values_valuefields

and boot_struct_value (at : region) (valuefields : Il.valuefield list) : Value.t
    =
  let value_valuefields = boot_valuefields valuefields in
  Value.Make.("STR valfield*" <| [ value_valuefields ] <<| "val" <<<| at)

and boot_valuecase (valuecase : Il.valuecase) : Value.t =
  let mixop, values = valuecase in
  let value_mixop = boot_mixop mixop in
  let value_values = boot_values values in
  Value.Make.(
    "mixop val*" <| [ value_mixop; value_values ] <<| "valcase" <<<| no_region)

and boot_case_value (at : region) (valuecase : Il.valuecase) : Value.t =
  let value_valuecase = boot_valuecase valuecase in
  Value.Make.("INJ valcase" <| [ value_valuecase ] <<| "val" <<<| at)

and boot_tuple_value (at : region) (values : Il.value list) : Value.t =
  let value_values = boot_values values in
  Value.Make.("TUP val*" <| [ value_values ] <<| "val" <<<| at)

and boot_opt_value (at : region) (value_opt : Il.value option) : Value.t =
  let value_value_opt = boot_value_opt value_opt in
  Value.Make.("OPT val?" <| [ value_value_opt ] <<| "val" <<<| at)

and boot_list_value (at : region) (values : Il.value list) : Value.t =
  let value_values = boot_values values in
  Value.Make.("LIST val*" <| [ value_values ] <<| "val" <<<| at)

and boot_func_value (at : region) (id : Il.id) : Value.t =
  let value_id = boot_id id in
  Value.Make.("FUNC id" <| [ value_id ] <<| "val" <<<| at)

and boot_extern_value (at : region) (json : Yojson.Safe.t) : Value.t =
  let typ_json = Typ.Make.var ("json" $ no_region) [] in
  let value_json = Value.Make.extern typ_json json in
  Value.Make.("EXT json" <| [ value_json ] <<| "val" <<<| at)

(* Operators *)

and boot_unop (unop : Il.unop) : Value.t =
  match unop with
  | #Bool.unop as unop -> boot_unop_bool unop
  | #Num.unop as unop -> boot_unop_num unop

and boot_unop_bool (unop : Bool.unop) : Value.t =
  match unop with
  | `NotOp -> Value.Make.("NOT" <| [] <<| "boolunop" <<<| no_region)

and boot_unop_num (unop : Num.unop) : Value.t =
  match unop with
  | `PlusOp -> Value.Make.("PLUS" <| [] <<| "numunop" <<<| no_region)
  | `MinusOp -> Value.Make.("MINUS" <| [] <<| "numunop" <<<| no_region)

and boot_binop (binop : Il.binop) : Value.t =
  match binop with
  | #Bool.binop as binop -> boot_binop_bool binop
  | #Num.binop as binop -> boot_binop_num binop

and boot_binop_bool (binop : Bool.binop) : Value.t =
  match binop with
  | `AndOp -> Value.Make.("AND" <| [] <<| "boolbinop" <<<| no_region)
  | `OrOp -> Value.Make.("OR" <| [] <<| "boolbinop" <<<| no_region)
  | `ImplOp -> Value.Make.("IMPL" <| [] <<| "boolbinop" <<<| no_region)
  | `EquivOp -> Value.Make.("EQUIV" <| [] <<| "boolbinop" <<<| no_region)

and boot_binop_num (binop : Num.binop) : Value.t =
  match binop with
  | `AddOp -> Value.Make.("ADD" <| [] <<| "numbinop" <<<| no_region)
  | `SubOp -> Value.Make.("SUB" <| [] <<| "numbinop" <<<| no_region)
  | `MulOp -> Value.Make.("MUL" <| [] <<| "numbinop" <<<| no_region)
  | `DivOp -> Value.Make.("DIV" <| [] <<| "numbinop" <<<| no_region)
  | `ModOp -> Value.Make.("MOD" <| [] <<| "numbinop" <<<| no_region)
  | `PowOp -> Value.Make.("POW" <| [] <<| "numbinop" <<<| no_region)

and boot_cmpop (cmpop : Il.cmpop) : Value.t =
  match cmpop with
  | #Bool.cmpop as cmpop -> boot_cmpop_bool cmpop
  | #Num.cmpop as cmpop -> boot_cmpop_num cmpop

and boot_cmpop_bool (cmpop : Bool.cmpop) : Value.t =
  match cmpop with
  | `EqOp -> Value.Make.("EQ" <| [] <<| "polycmpop" <<<| no_region)
  | `NeOp -> Value.Make.("NE" <| [] <<| "polycmpop" <<<| no_region)

and boot_cmpop_num (cmpop : Num.cmpop) : Value.t =
  match cmpop with
  | `LtOp -> Value.Make.("LT" <| [] <<| "numcmpop" <<<| no_region)
  | `LeOp -> Value.Make.("LE" <| [] <<| "numcmpop" <<<| no_region)
  | `GtOp -> Value.Make.("GT" <| [] <<| "numcmpop" <<<| no_region)
  | `GeOp -> Value.Make.("GE" <| [] <<| "numcmpop" <<<| no_region)

(* Type arguments *)

and boot_targ (targ : Il.targ) : Value.t =
  let value_targ = boot_typ targ in
  Value.Make.(value_targ #@@ "targ")

and boot_targs (targs : Il.targ list) : Value.t =
  let values_targs = List.map boot_targ targs in
  let typ_targs = Typ.Make.var ("targ" $ no_region) [] |> Typ.Make.list in
  Value.Make.list typ_targs values_targs

(* Type parameters *)

and boot_tparam (tparam : Il.tparam) : Value.t = boot_id tparam

and boot_tparams (tparams : Il.tparam list) : Value.t =
  let values_tparams = List.map boot_tparam tparams in
  let typ_tparams = Typ.Make.var ("tparam" $ no_region) [] |> Typ.Make.list in
  Value.Make.list typ_tparams values_tparams

(* Parameters *)

and boot_param (param : Il.param) : Value.t =
  let at = param.at in
  match param.it with
  | ExpP typ ->
      let value_typ = boot_typ typ in
      Value.Make.("EXP typ" <| [ value_typ ] <<| "param" <<<| at)
  | DefP (id, tparams, params, typ) ->
      let value_id = boot_id id in
      let value_tparams = boot_tparams tparams in
      let value_params = boot_params params in
      let value_typ = boot_typ typ in
      Value.Make.(
        "FUN id `: tparam* param* `-> typ"
        <| [ value_id; value_tparams; value_params; value_typ ]
        <<| "param" <<<| at)

and boot_params (params : Il.param list) : Value.t =
  let values_params = List.map boot_param params in
  let typ_params = Typ.Make.var ("param" $ no_region) [] |> Typ.Make.list in
  Value.Make.list typ_params values_params

(* Arguments *)

and boot_arg (arg : Il.arg) : Value.t =
  let at = arg.at in
  match arg.it with
  | ExpA exp ->
      let value_exp = boot_exp exp in
      Value.Make.("EXP exp" <| [ value_exp ] <<| "arg" <<<| at)
  | DefA id ->
      let value_id = boot_id id in
      Value.Make.("FUN id" <| [ value_id ] <<| "arg" <<<| at)

and boot_args (args : Il.arg list) : Value.t =
  let values_args = List.map boot_arg args in
  let typ_args = Typ.Make.var ("arg" $ no_region) [] |> Typ.Make.list in
  Value.Make.list typ_args values_args

(* Expressions *)

and boot_exp (exp : Il.exp) : Value.t =
  let at = exp.at in
  match exp.it with
  | BoolE b -> boot_bool_exp at b
  | NumE num -> boot_num_exp at num
  | TextE t -> boot_text_exp at t
  | VarE id -> boot_var_exp at id
  | UnE (unop, _, exp) -> boot_un_exp at unop exp
  | BinE (binop, _, exp_l, exp_r) -> boot_bin_exp at binop exp_l exp_r
  | CmpE (cmpop, _, exp_l, exp_r) -> boot_cmp_exp at cmpop exp_l exp_r
  | UpCastE (typ, exp) -> boot_upcast_exp at typ exp
  | DownCastE (typ, exp) -> boot_downcast_exp at typ exp
  | SubE (exp, typ) -> boot_sub_exp at exp typ
  | MatchE (exp, pattern) -> boot_match_exp at exp pattern
  | TupleE exps -> boot_tuple_exp at exps
  | CaseE (mixop, exps) -> boot_case_exp at mixop exps
  | StrE expfields -> boot_str_exp at expfields
  | OptE exp_opt -> boot_opt_exp at exp_opt
  | ListE exps -> boot_list_exp at exps
  | ConsE (exp_h, exp_t) -> boot_cons_exp at exp_h exp_t
  | CatE (exp_l, exp_r) -> boot_cat_exp at exp_l exp_r
  | MemE (exp_e, exp_s) -> boot_mem_exp at exp_e exp_s
  | LenE exp -> boot_len_exp at exp
  | DotE (exp, atom) -> boot_dot_exp at exp atom
  | IdxE (exp_b, exp_i) -> boot_idx_exp at exp_b exp_i
  | SliceE (exp_b, exp_i, exp_n) -> boot_slice_exp at exp_b exp_i exp_n
  | UpdE (exp_b, path, exp_n) -> boot_upd_exp at exp_b path exp_n
  | CallE (id, targs, args) -> boot_call_exp at id targs args
  | IterE (exp, iterexp) -> boot_iter_exp at exp iterexp

and boot_bool_exp (at : region) (b : bool) : Value.t =
  Value.Make.("BOOL bool" <| [ bool b ] <<| "exp" <<<| at)

and boot_num_exp (at : region) (num : Il.num) : Value.t =
  let value_num = boot_num_value at num in
  Value.Make.(value_num #@@ "exp")

and boot_text_exp (at : region) (t : string) : Value.t =
  Value.Make.("TEXT text" <| [ text t ] <<| "exp" <<<| at)

and boot_var_exp (at : region) (id : Il.id) : Value.t =
  let value_id = boot_id id in
  Value.Make.("VAR id" <| [ value_id ] <<| "exp" <<<| at)

and boot_un_exp (at : region) (unop : Il.unop) (exp : Il.exp) : Value.t =
  let value_unop = boot_unop unop in
  let value_exp = boot_exp exp in
  Value.Make.("UN unop exp" <| [ value_unop; value_exp ] <<| "exp" <<<| at)

and boot_bin_exp (at : region) (binop : Il.binop) (exp_l : Il.exp)
    (exp_r : Il.exp) : Value.t =
  let value_binop = boot_binop binop in
  let value_exp_l = boot_exp exp_l in
  let value_exp_r = boot_exp exp_r in
  Value.Make.(
    "BIN binop exp exp"
    <| [ value_binop; value_exp_l; value_exp_r ]
    <<| "exp" <<<| at)

and boot_cmp_exp (at : region) (cmpop : Il.cmpop) (exp_l : Il.exp)
    (exp_r : Il.exp) : Value.t =
  let value_cmpop = boot_cmpop cmpop in
  let value_exp_l = boot_exp exp_l in
  let value_exp_r = boot_exp exp_r in
  Value.Make.(
    "CMP cmpop exp exp"
    <| [ value_cmpop; value_exp_l; value_exp_r ]
    <<| "exp" <<<| at)

and boot_upcast_exp (at : region) (typ : Il.typ) (exp : Il.exp) : Value.t =
  let value_typ = boot_typ typ in
  let value_exp = boot_exp exp in
  Value.Make.("UPCAST typ exp" <| [ value_typ; value_exp ] <<| "exp" <<<| at)

and boot_downcast_exp (at : region) (typ : Il.typ) (exp : Il.exp) : Value.t =
  let value_typ = boot_typ typ in
  let value_exp = boot_exp exp in
  Value.Make.("DOWNCAST typ exp" <| [ value_typ; value_exp ] <<| "exp" <<<| at)

and boot_sub_exp (at : region) (exp : Il.exp) (typ : Il.typ) : Value.t =
  let value_exp = boot_exp exp in
  let value_typ = boot_typ typ in
  Value.Make.("SUB exp typ" <| [ value_exp; value_typ ] <<| "exp" <<<| at)

and boot_match_exp (at : region) (exp : Il.exp) (pattern : Il.pattern) : Value.t
    =
  let value_exp = boot_exp exp in
  let value_pattern = boot_pattern pattern in
  Value.Make.(
    "MATCH exp pattern" <| [ value_exp; value_pattern ] <<| "exp" <<<| at)

and boot_tuple_exp (at : region) (exps : Il.exp list) : Value.t =
  let value_exps = boot_exps exps in
  Value.Make.("TUP exp*" <| [ value_exps ] <<| "exp" <<<| at)

and boot_expcase (mixop : Il.mixop) (exps : Il.exp list) : Value.t =
  let value_mixop = boot_mixop mixop in
  let value_exps = boot_exps exps in
  Value.Make.(
    "mixop exp*" <| [ value_mixop; value_exps ] <<| "expcase" <<<| no_region)

and boot_case_exp (at : region) (mixop : Il.mixop) (exps : Il.exp list) :
    Value.t =
  let value_expcase = boot_expcase mixop exps in
  Value.Make.("INJ expcase" <| [ value_expcase ] <<| "exp" <<<| at)

and boot_str_exp (at : region) (expfields : (Il.atom * Il.exp) list) : Value.t =
  let value_expfields = boot_expfields expfields in
  Value.Make.("STR expfield*" <| [ value_expfields ] <<| "exp" <<<| at)

and boot_opt_exp (at : region) (exp_opt : Il.exp option) : Value.t =
  let value_exp_opt = boot_exp_opt exp_opt in
  Value.Make.("OPT exp?" <| [ value_exp_opt ] <<| "exp" <<<| at)

and boot_list_exp (at : region) (exps : Il.exp list) : Value.t =
  let value_exps = boot_exps exps in
  Value.Make.("LIST exp*" <| [ value_exps ] <<| "exp" <<<| at)

and boot_cons_exp (at : region) (exp_h : Il.exp) (exp_t : Il.exp) : Value.t =
  let value_exp_h = boot_exp exp_h in
  let value_exp_t = boot_exp exp_t in
  Value.Make.("CONS exp exp" <| [ value_exp_h; value_exp_t ] <<| "exp" <<<| at)

and boot_cat_exp (at : region) (exp_l : Il.exp) (exp_r : Il.exp) : Value.t =
  let value_exp_l = boot_exp exp_l in
  let value_exp_r = boot_exp exp_r in
  Value.Make.("CAT exp exp" <| [ value_exp_l; value_exp_r ] <<| "exp" <<<| at)

and boot_mem_exp (at : region) (exp_e : Il.exp) (exp_s : Il.exp) : Value.t =
  let value_exp_e = boot_exp exp_e in
  let value_exp_s = boot_exp exp_s in
  Value.Make.("MEM exp exp" <| [ value_exp_e; value_exp_s ] <<| "exp" <<<| at)

and boot_len_exp (at : region) (exp : Il.exp) : Value.t =
  let value_exp = boot_exp exp in
  Value.Make.("LEN exp" <| [ value_exp ] <<| "exp" <<<| at)

and boot_dot_exp (at : region) (exp : Il.exp) (atom : Il.atom) : Value.t =
  let value_exp = boot_exp exp in
  let value_atom = boot_atom atom in
  Value.Make.("DOT exp atom" <| [ value_exp; value_atom ] <<| "exp" <<<| at)

and boot_idx_exp (at : region) (exp_b : Il.exp) (exp_i : Il.exp) : Value.t =
  let value_exp_b = boot_exp exp_b in
  let value_exp_i = boot_exp exp_i in
  Value.Make.("IDX exp exp" <| [ value_exp_b; value_exp_i ] <<| "exp" <<<| at)

and boot_slice_exp (at : region) (exp_b : Il.exp) (exp_i : Il.exp)
    (exp_n : Il.exp) : Value.t =
  let value_exp_b = boot_exp exp_b in
  let value_exp_i = boot_exp exp_i in
  let value_exp_n = boot_exp exp_n in
  Value.Make.(
    "SLICE exp exp exp"
    <| [ value_exp_b; value_exp_i; value_exp_n ]
    <<| "exp" <<<| at)

and boot_upd_exp (at : region) (exp_b : Il.exp) (path : Il.path)
    (exp_n : Il.exp) : Value.t =
  let value_exp_b = boot_exp exp_b in
  let value_path = boot_path path in
  let value_exp_n = boot_exp exp_n in
  Value.Make.(
    "UPD exp path exp"
    <| [ value_exp_b; value_path; value_exp_n ]
    <<| "exp" <<<| at)

and boot_call_exp (at : region) (id : Il.id) (targs : Il.targ list)
    (args : Il.arg list) : Value.t =
  let value_id = boot_id id in
  let value_targs = boot_targs targs in
  let value_args = boot_args args in
  Value.Make.(
    "CALL id targ* arg*"
    <| [ value_id; value_targs; value_args ]
    <<| "exp" <<<| at)

and boot_iter_exp (at : region) (exp : Il.exp) (iterexp : Il.iterexp) : Value.t
    =
  let value_exp = boot_exp exp in
  let value_iterexp = boot_iterexp iterexp in
  Value.Make.(
    "ITER exp iterexp" <| [ value_exp; value_iterexp ] <<| "exp" <<<| at)

and boot_exps (exps : Il.exp list) : Value.t =
  let values_exps = List.map boot_exp exps in
  let typ_exps = Typ.Make.var ("exp" $ no_region) [] |> Typ.Make.list in
  Value.Make.list typ_exps values_exps

and boot_exp_opt (exp_opt : Il.exp option) : Value.t =
  let typ_exp_opt = Typ.Make.var ("exp" $ no_region) [] |> Typ.Make.opt in
  Value.Make.opt typ_exp_opt (Option.map boot_exp exp_opt)

and boot_expfield ((atom, exp) : Il.atom * Il.exp) : Value.t =
  let value_atom = boot_atom atom in
  let value_exp = boot_exp exp in
  Value.Make.(
    "atom exp" <| [ value_atom; value_exp ] <<| "expfield" <<<| no_region)

and boot_expfields (expfields : (Il.atom * Il.exp) list) : Value.t =
  let values_expfields = List.map boot_expfield expfields in
  let typ_expfields =
    Typ.Make.var ("expfield" $ no_region) [] |> Typ.Make.list
  in
  Value.Make.list typ_expfields values_expfields

(* Paths *)

and boot_path (path : Il.path) : Value.t =
  let at = path.at in
  match path.it with
  | RootP -> Value.Make.("ROOT" <| [] <<| "path" <<<| at)
  | IdxP (path, exp) ->
      let value_path = boot_path path in
      let value_exp = boot_exp exp in
      Value.Make.(
        "IDX path exp" <| [ value_path; value_exp ] <<| "path" <<<| at)
  | SliceP (path, exp_i, exp_n) ->
      let value_path = boot_path path in
      let value_exp_i = boot_exp exp_i in
      let value_exp_n = boot_exp exp_n in
      Value.Make.(
        "SLICE path exp exp"
        <| [ value_path; value_exp_i; value_exp_n ]
        <<| "path" <<<| at)
  | DotP (path, atom) ->
      let value_path = boot_path path in
      let value_atom = boot_atom atom in
      Value.Make.(
        "DOT path atom" <| [ value_path; value_atom ] <<| "path" <<<| at)

(* Patterns *)

and boot_pattern (pattern : Il.pattern) : Value.t =
  match pattern with
  | CaseP mixop ->
      let value_mixop = boot_mixop mixop in
      Value.Make.("INJ mixop" <| [ value_mixop ] <<| "pattern" <<<| no_region)
  | ListP `Cons -> Value.Make.("CONS" <| [] <<| "listpattern" <<<| no_region)
  | ListP (`Fixed n) ->
      Value.Make.(
        "FIXED nat"
        <| [ nat (Bigint.of_int n) ]
        <<| "listpattern" <<<| no_region)
  | ListP `Nil -> Value.Make.("NIL" <| [] <<| "listpattern" <<<| no_region)
  | OptP `Some -> Value.Make.("SOME" <| [] <<| "optpattern" <<<| no_region)
  | OptP `None -> Value.Make.("NONE" <| [] <<| "optpattern" <<<| no_region)

(* Iter expressions and premises *)

and boot_iterexp ((iter, vars) : Il.iterexp) : Value.t =
  let value_iter = boot_iter iter in
  let value_vars = boot_vars vars in
  Value.Make.(
    "iter vari*" <| [ value_iter; value_vars ] <<| "iterexp" <<<| no_region)

and boot_iterprem ((iter, vars_in, vars_out) : Il.iterprem) : Value.t =
  let value_iter = boot_iter iter in
  let value_vars_in = boot_vars vars_in in
  let value_vars_out = boot_vars vars_out in
  Value.Make.(
    "iter vari* vari*"
    <| [ value_iter; value_vars_in; value_vars_out ]
    <<| "iterprem" <<<| no_region)

(* Premises *)

and boot_prem (prem : Il.prem) : Value.t option =
  let wrap_some value = Some value in
  let at = prem.at in
  match prem.it with
  | RulePr (id, (_, exps), input) -> boot_rel_prem at id exps input |> wrap_some
  | IfPr exp -> boot_if_prem at exp |> wrap_some
  | IfHoldPr (id, (_, exps)) -> boot_ifhold_prem at id exps |> wrap_some
  | IfNotHoldPr (id, (_, exps)) -> boot_ifnothold_prem at id exps |> wrap_some
  | LetPr (exp_l, exp_r) -> boot_let_prem at exp_l exp_r |> wrap_some
  | IterPr (prem, iterprem) -> boot_iter_prem at prem iterprem
  | DebugPr _ -> None

and boot_rel_prem (at : region) (id : Il.id) (exps : Il.exp list)
    (input : Hints.Input.t) : Value.t =
  let value_id = boot_id id in
  let exps_input, exps_output = Hints.Input.split input exps in
  let value_input_exps = boot_exps exps_input in
  let value_exps_output = boot_exps exps_output in
  Value.Make.(
    "REL id `: exp* `-> exp*"
    <| [ value_id; value_input_exps; value_exps_output ]
    <<| "prem" <<<| at)

and boot_if_prem (at : region) (exp : Il.exp) : Value.t =
  let value_exp = boot_exp exp in
  Value.Make.("IF exp" <| [ value_exp ] <<| "prem" <<<| at)

and boot_ifhold_prem (at : region) (id : Il.id) (exps : Il.exp list) : Value.t =
  let value_id = boot_id id in
  let value_exps = boot_exps exps in
  Value.Make.(
    "IFHOLD id `: exp*" <| [ value_id; value_exps ] <<| "prem" <<<| at)

and boot_ifnothold_prem (at : region) (id : Il.id) (exps : Il.exp list) :
    Value.t =
  let value_id = boot_id id in
  let value_exps = boot_exps exps in
  Value.Make.(
    "IFNOTHOLD id `: exp*" <| [ value_id; value_exps ] <<| "prem" <<<| at)

and boot_let_prem (at : region) (exp_l : Il.exp) (exp_r : Il.exp) : Value.t =
  let value_exp_l = boot_exp exp_l in
  let value_exp_r = boot_exp exp_r in
  Value.Make.(
    "LET exp `= exp" <| [ value_exp_l; value_exp_r ] <<| "prem" <<<| at)

and boot_iter_prem (at : region) (prem : Il.prem) (iterprem : Il.iterprem) :
    Value.t option =
  match boot_prem prem with
  | Some value_prem ->
      let value_iterprem = boot_iterprem iterprem in
      let value_prem =
        Value.Make.(
          "ITER prem iterprem"
          <| [ value_prem; value_iterprem ]
          <<| "prem" <<<| at)
      in
      Some value_prem
  | None -> None

and boot_prems (prems : Il.prem list) : Value.t =
  let values_prems = prems |> List.map boot_prem |> List.filter_map Fun.id in
  let typ_prems = Typ.Make.var ("prem" $ no_region) [] |> Typ.Make.list in
  Value.Make.list typ_prems values_prems

(* Rule matching and paths *)

and boot_rulmatch ((_, exps_input, prems) : Il.rulematch) : Value.t =
  let value_exps = boot_exps exps_input in
  let value_prems = boot_prems prems in
  Value.Make.(
    "exp* `- prem*" <| [ value_exps; value_prems ] <<| "rulmatch" <<<| no_region)

and boot_rulpath ((id, prems, exps_output) : Il.rulepath) : Value.t =
  let value_id = boot_id id in
  let value_exps_output = boot_exps exps_output in
  let value_prems = boot_prems prems in
  Value.Make.(
    "id `= exp* `- prem*"
    <| [ value_id; value_exps_output; value_prems ]
    <<| "rulpath" <<<| no_region)

and boot_rulpaths (rulpaths : Il.rulepath list) : Value.t =
  let values_rulpaths = List.map boot_rulpath rulpaths in
  let typ_rulpaths = Typ.Make.var ("rulpath" $ no_region) [] |> Typ.Make.list in
  Value.Make.list typ_rulpaths values_rulpaths

and boot_rulgroup (rulgroup : Il.rulegroup) : Value.t =
  let at = rulgroup.at in
  let id, rulmatch, rulpaths = rulgroup.it in
  let value_id = boot_id id in
  let value_rulmatch = boot_rulmatch rulmatch in
  let value_rulpaths = boot_rulpaths rulpaths in
  Value.Make.(
    "id `: rulmatch `= rulpath*"
    <| [ value_id; value_rulmatch; value_rulpaths ]
    <<| "rulgroup" <<<| at)

and boot_rulgroups (rulgroups : Il.rulegroup list) : Value.t =
  let values_rulgroups = List.map boot_rulgroup rulgroups in
  let typ_rulgroups =
    Typ.Make.var ("rulgroup" $ no_region) [] |> Typ.Make.list
  in
  Value.Make.list typ_rulgroups values_rulgroups

and boot_elsgroup (elsgroup : Il.elsegroup) : Value.t =
  let at = elsgroup.at in
  let id, rulmatch, rulpath = elsgroup.it in
  let value_id = boot_id id in
  let value_rulmatch = boot_rulmatch rulmatch in
  let value_rulpath = boot_rulpath rulpath in
  Value.Make.(
    "id `: rulmatch `= rulpath"
    <| [ value_id; value_rulmatch; value_rulpath ]
    <<| "elsgroup" <<<| at)

and boot_elsgroup_opt (elsgroup_opt : Il.elsegroup option) : Value.t =
  let typ_elsgroup_opt =
    Typ.Make.var ("elsgroup" $ no_region) [] |> Typ.Make.opt
  in
  Value.Make.opt typ_elsgroup_opt (Option.map boot_elsgroup elsgroup_opt)

(* Clauses *)

and boot_clause (clause : Il.clause) : Value.t =
  let at = clause.at in
  let args, exp, prems = clause.it in
  let value_args = boot_args args in
  let value_exp = boot_exp exp in
  let value_prems = boot_prems prems in
  Value.Make.(
    "arg* `= exp `- prem*"
    <| [ value_args; value_exp; value_prems ]
    <<| "clause" <<<| at)

and boot_clauses (clauses : Il.clause list) : Value.t =
  let values_clauses = List.map boot_clause clauses in
  let typ_clauses = Typ.Make.var ("clause" $ no_region) [] |> Typ.Make.list in
  Value.Make.list typ_clauses values_clauses

and boot_elsclause (elsclause : Il.elseclause) : Value.t = boot_clause elsclause

and boot_elsclause_opt (elsclause_opt : Il.elseclause option) : Value.t =
  let typ_elsclause_opt =
    Typ.Make.var ("elsclause" $ no_region) [] |> Typ.Make.opt
  in
  Value.Make.opt typ_elsclause_opt (Option.map boot_elsclause elsclause_opt)

(* Table rows *)

and boot_tablerow (tablerow : Il.tablerow) : Value.t =
  let at = tablerow.at in
  let _exps, args, exp, prems = tablerow.it in
  let value_args = boot_args args in
  let value_exp = boot_exp exp in
  let value_prems = boot_prems prems in
  Value.Make.(
    "arg* `= exp `- prem*"
    <| [ value_args; value_exp; value_prems ]
    <<| "tblrow" <<<| at)

and boot_tablerows (tablerows : Il.tablerow list) : Value.t =
  let values_tablerows = List.map boot_tablerow tablerows in
  let typ_tablerows = Typ.Make.var ("tblrow" $ no_region) [] |> Typ.Make.list in
  Value.Make.list typ_tablerows values_tablerows

(* Definitions *)

let rec boot_def (def : Il.def) : Value.t option =
  let wrap_some value = Some value in
  let at = def.at in
  match def.it with
  | ExternTypD (id, _) -> boot_extern_typ_def at id |> wrap_some
  | TypD (id, tparams, deftyp, _) ->
      boot_typ_def at id tparams deftyp |> wrap_some
  | VarD _ -> None
  | ExternRelD (id, nottyp, input, _) ->
      boot_extern_rel_def at id nottyp input |> wrap_some
  | RelD (id, nottyp, input, rulgroups, elsgroup, _) ->
      boot_rel_def at id nottyp input rulgroups elsgroup |> wrap_some
  | ExternDecD (id, tparams, params, typ, _) ->
      boot_extern_func_def at id tparams params typ |> wrap_some
  | BuiltinDecD (id, tparams, params, typ, _) ->
      boot_builtin_func_def at id tparams params typ |> wrap_some
  | TableDecD (id, params, typ, tablerows, _) ->
      boot_table_func_def at id params typ tablerows |> wrap_some
  | FuncDecD (id, tparams, params, typ, clauses, elsclause, _) ->
      boot_func_def at id tparams params typ clauses elsclause |> wrap_some

and boot_extern_typ_def (at : region) (id : Il.id) : Value.t =
  let value_id = boot_id id in
  Value.Make.("EXTTYP id" <| [ value_id ] <<| "defn" <<<| at)

and boot_typ_def (at : region) (id : Il.id) (tparams : Il.tparam list)
    (deftyp : Il.deftyp) : Value.t =
  let value_id = boot_id id in
  let value_tparams = boot_tparams tparams in
  let value_deftyp = boot_deftyp deftyp in
  Value.Make.(
    "TYP id tparam* `= deftyp"
    <| [ value_id; value_tparams; value_deftyp ]
    <<| "defn" <<<| at)

and boot_extern_rel_def (at : region) (id : Il.id) (nottyp : Il.nottyp)
    (input : Hints.Input.t) : Value.t =
  let _, typs = nottyp.it in
  let typs_input, typs_output = Hints.Input.split input typs in
  let value_id = boot_id id in
  let value_typs_input = boot_typs typs_input in
  let value_typs_output = boot_typs typs_output in
  Value.Make.(
    "EXTREL id `: typ* `-> typ*"
    <| [ value_id; value_typs_input; value_typs_output ]
    <<| "defn" <<<| at)

and boot_rel_def (at : region) (id : Il.id) (nottyp : Il.nottyp)
    (input : Hints.Input.t) (rulgroups : Il.rulegroup list)
    (elsgroup : Il.elsegroup option) : Value.t =
  let _, typs = nottyp.it in
  let typs_input, typs_output = Hints.Input.split input typs in
  let value_id = boot_id id in
  let value_typs_input = boot_typs typs_input in
  let value_typs_output = boot_typs typs_output in
  let value_rulgroups = boot_rulgroups rulgroups in
  let value_elsgroup = boot_elsgroup_opt elsgroup in
  Value.Make.(
    "REL id `: typ* `-> typ* `= rulgroup* elsgroup?"
    <| [
         value_id;
         value_typs_input;
         value_typs_output;
         value_rulgroups;
         value_elsgroup;
       ]
    <<| "defn" <<<| at)

and boot_extern_func_def (at : region) (id : Il.id) (tparams : Il.tparam list)
    (params : Il.param list) (typ : Il.typ) : Value.t =
  let value_id = boot_id id in
  let value_tparams = boot_tparams tparams in
  let value_params = boot_params params in
  let value_typ = boot_typ typ in
  Value.Make.(
    "EXTFUNC id tparam* param* `: typ"
    <| [ value_id; value_tparams; value_params; value_typ ]
    <<| "defn" <<<| at)

and boot_builtin_func_def (at : region) (id : Il.id) (tparams : Il.tparam list)
    (params : Il.param list) (typ : Il.typ) : Value.t =
  let value_id = boot_id id in
  let value_tparams = boot_tparams tparams in
  let value_params = boot_params params in
  let value_typ = boot_typ typ in
  Value.Make.(
    "BUILTINFUNC id tparam* param* `: typ"
    <| [ value_id; value_tparams; value_params; value_typ ]
    <<| "defn" <<<| at)

and boot_table_func_def (at : region) (id : Il.id) (params : Il.param list)
    (typ : Il.typ) (tablerows : Il.tablerow list) : Value.t =
  let value_id = boot_id id in
  let value_params = boot_params params in
  let value_typ = boot_typ typ in
  let value_tablerows = boot_tablerows tablerows in
  Value.Make.(
    "TABLEFUNC id param* `: typ `= tblrow*"
    <| [ value_id; value_params; value_typ; value_tablerows ]
    <<| "defn" <<<| at)

and boot_func_def (at : region) (id : Il.id) (tparams : Il.tparam list)
    (params : Il.param list) (typ : Il.typ) (clauses : Il.clause list)
    (elsclause : Il.elseclause option) : Value.t =
  let value_id = boot_id id in
  let value_tparams = boot_tparams tparams in
  let value_params = boot_params params in
  let value_typ = boot_typ typ in
  let value_clauses = boot_clauses clauses in
  let value_elsclause = boot_elsclause_opt elsclause in
  Value.Make.(
    "FUNC id tparam* param* `: typ `= clause* elsclause?"
    <| [
         value_id;
         value_tparams;
         value_params;
         value_typ;
         value_clauses;
         value_elsclause;
       ]
    <<| "defn" <<<| at)

(* Specification *)

let boot_spec (spec : Il.spec) : Value.t =
  let values_def = List.map boot_def spec |> List.filter_map Fun.id in
  let typ_script = Typ.Make.var ("script" $ no_region) [] in
  Value.Make.list typ_script values_def
