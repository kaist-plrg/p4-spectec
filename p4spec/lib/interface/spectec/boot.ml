open Domain
open Lang
open Xl
open Mixops
open Typs
module Value = Runtime.Value
open Util.Source

(* Identifiers *)

let boot_id (id : Il.id) : Value.t =
  let value_id = Value.Make.text ~at:id.at id.it in
  Value.Make.(value_id #@@ "id")

(* Atoms *)

let boot_atom (atom : Il.atom) : Value.t =
  let value_atom =
    atom.it |> Atom.string_of_atom |> Value.Make.text ~at:atom.at
  in
  Value.Make.(value_atom #@@ "atom")

(* Mixfix operators *)

let boot_mixop (mixop : Il.mixop) : Value.t =
  let atoms_matrix = Mixop.atoms_matrix mixop in
  let values_atoms =
    List.map
      (fun atoms ->
        let values_atoms = List.map boot_atom atoms in
        let typ_atoms =
          Runtime.Type.Typ.Make.var ("atom" $ no_region) []
          |> Runtime.Type.Typ.Make.list
        in
        Value.Make.list typ_atoms values_atoms)
      atoms_matrix
  in
  let value_atoms_matrix =
    let typ_atoms_matrix =
      Runtime.Type.Typ.Make.var ("atom" $ no_region) []
      |> Runtime.Type.Typ.Make.list
    in
    Value.Make.list typ_atoms_matrix values_atoms
  in
  Value.Make.(value_atoms_matrix #@@ "mixop")

(* Iterators *)

let boot_iter (iter : Il.iter) : Value.t =
  match iter with
  | Opt -> Value.Make.(mop_quest <|! [] <<|! typ_iter)
  | List -> Value.Make.(mop_star <|! [] <<|! typ_iter)

let boot_iters (iters : Il.iter list) : Value.t =
  let values_iters = List.map boot_iter iters in
  Value.Make.list (Runtime.Type.Typ.Make.list typ_iter) values_iters

(* Variables *)

let rec boot_var (var : Il.var) : Value.t =
  let id, typ, iters = var in
  let value_id = boot_id id in
  let value_typ = boot_typ typ in
  let value_iters = boot_iters iters in
  Value.Make.(mop_vari <|! [ value_id; value_typ; value_iters ] <<|! typ_vari)

and boot_vars (vars : Il.var list) : Value.t =
  let values_vars = List.map boot_var vars in
  Value.Make.list (Runtime.Type.Typ.Make.list typ_vari) values_vars

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
  Value.Make.list (Runtime.Type.Typ.Make.list typ_typ) values_typs

and boot_bool_typ (at : region) : Value.t =
  Value.Make.(mop_bool_typ <|! [] <<|! typ_optyp <<<| at)

and boot_num_typ (at : region) (numtyp : Num.typ) : Value.t =
  match numtyp with
  | `NatT -> Value.Make.(mop_num_typ_nat <|! [] <<|! typ_numtyp <<<| at)
  | `IntT -> Value.Make.(mop_num_typ_int <|! [] <<|! typ_numtyp <<<| at)

and boot_text_typ (at : region) : Value.t =
  Value.Make.(mop_text_typ <|! [] <<|! typ_optyp <<<| at)

and boot_var_typ (at : region) (id : Il.id) (targs : Il.targ list) : Value.t =
  let value_id = boot_id id in
  let value_targs = boot_targs targs in
  Value.Make.(mop_var_typ <|! [ value_id; value_targs ] <<|! typ_typ <<<| at)

and boot_tuple_typ (at : region) (typs : Il.typ list) : Value.t =
  let value_typs = boot_typs typs in
  Value.Make.(mop_tuple_typ <|! [ value_typs ] <<|! typ_typ <<<| at)

and boot_iter_typ (at : region) (typ : Il.typ) (iter : Il.iter) : Value.t =
  let value_typ = boot_typ typ in
  let value_iter = boot_iter iter in
  Value.Make.(mop_iter_typ <|! [ value_typ; value_iter ] <<|! typ_typ <<<| at)

and boot_func_typ (at : region) : Value.t =
  Value.Make.(mop_func_typ <|! [] <<|! typ_typ <<<| at)

(* Defined types *)

and boot_deftyp (deftyp : Il.deftyp) : Value.t =
  let at = deftyp.at in
  match deftyp.it with
  | PlainT typ -> boot_plain_deftyp at typ
  | StructT typfields -> boot_struct_deftyp at typfields
  | VariantT typcases -> boot_variant_deftyp at typcases

and boot_plain_deftyp (at : region) (typ : Il.typ) : Value.t =
  let value_typ = boot_typ typ in
  Value.Make.(mop_plain_deftyp <|! [ value_typ ] <<|! typ_deftyp <<<| at)

and boot_typfield (typfield : Il.typfield) : Value.t =
  let atom, typ = typfield in
  let value_atom = boot_atom atom in
  let value_typ = boot_typ typ in
  Value.Make.(mop_typfield <|! [ value_atom; value_typ ] <<|! typ_typfield)

and boot_typfields (typfields : Il.typfield list) : Value.t =
  let values_typfields = List.map boot_typfield typfields in
  Value.Make.list (Runtime.Type.Typ.Make.list typ_typfield) values_typfields

and boot_struct_deftyp (at : region) (typfields : Il.typfield list) : Value.t =
  let value_typfields = boot_typfields typfields in
  Value.Make.(mop_struct_deftyp <|! [ value_typfields ] <<|! typ_deftyp <<<| at)

and boot_typcase (typcase : Il.typcase) : Value.t =
  let nottyp, _, _ = typcase in
  let mop, typs = nottyp.it in
  let value_mixop = boot_mixop mop in
  let value_typs = boot_typs typs in
  Value.Make.(
    mop_typcase <|! [ value_mixop; value_typs ] <<|! typ_typcase <<<| nottyp.at)

and boot_typcases (typcases : Il.typcase list) : Value.t =
  let values_typcases = List.map boot_typcase typcases in
  Value.Make.list (Runtime.Type.Typ.Make.list typ_typcase) values_typcases

and boot_variant_deftyp (at : region) (typcases : Il.typcase list) : Value.t =
  let value_typcases = boot_typcases typcases in
  Value.Make.(mop_variant_deftyp <|! [ value_typcases ] <<|! typ_deftyp <<<| at)

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
  Value.Make.opt
    (Runtime.Type.Typ.Make.opt typ_val)
    (Option.map boot_value value_opt)

and boot_values (values : Il.value list) : Value.t =
  let values_values = List.map boot_value values in
  Value.Make.list (Runtime.Type.Typ.Make.list typ_val) values_values

and boot_bool_value (at : region) (b : bool) : Value.t =
  Value.Make.(mop_bool_value <|! [ bool b ] <<|! typ_val <<<| at)

and boot_num_value (at : region) (num : Il.num) : Value.t =
  match num with
  | `Nat n -> Value.Make.(mop_num_value_nat <|! [ nat n ] <<|! typ_num <<<| at)
  | `Int i -> Value.Make.(mop_num_value_int <|! [ int i ] <<|! typ_num <<<| at)

and boot_text_value (at : region) (t : string) : Value.t =
  Value.Make.(mop_text_value <|! [ text t ] <<|! typ_val <<<| at)

and boot_valuefield (vf : Il.valuefield) : Value.t =
  let atom, value = vf in
  let value_atom = boot_atom atom in
  let value_value = boot_value value in
  Value.Make.(mop_valuefield <|! [ value_atom; value_value ] <<|! typ_valfield)

and boot_valuefields (valuefields : Il.valuefield list) : Value.t =
  let values_valuefields = List.map boot_valuefield valuefields in
  Value.Make.list (Runtime.Type.Typ.Make.list typ_valfield) values_valuefields

and boot_struct_value (at : region) (valuefields : Il.valuefield list) : Value.t
    =
  let value_valuefields = boot_valuefields valuefields in
  Value.Make.(mop_struct_value <|! [ value_valuefields ] <<|! typ_val <<<| at)

and boot_valuecase (vc : Il.valuecase) : Value.t =
  let mop, values = vc in
  let value_mixop = boot_mixop mop in
  let value_values = boot_values values in
  Value.Make.(mop_valuecase <|! [ value_mixop; value_values ] <<|! typ_valcase)

and boot_case_value (at : region) (vc : Il.valuecase) : Value.t =
  let value_valuecase = boot_valuecase vc in
  Value.Make.(mop_case_value <|! [ value_valuecase ] <<|! typ_val <<<| at)

and boot_tuple_value (at : region) (values : Il.value list) : Value.t =
  let value_values = boot_values values in
  Value.Make.(mop_tuple_value <|! [ value_values ] <<|! typ_val <<<| at)

and boot_opt_value (at : region) (value_opt : Il.value option) : Value.t =
  let value_value_opt = boot_value_opt value_opt in
  Value.Make.(mop_opt_value <|! [ value_value_opt ] <<|! typ_val <<<| at)

and boot_list_value (at : region) (values : Il.value list) : Value.t =
  let value_values = boot_values values in
  Value.Make.(mop_list_value <|! [ value_values ] <<|! typ_val <<<| at)

and boot_func_value (at : region) (id : Il.id) : Value.t =
  let value_id = boot_id id in
  Value.Make.(mop_func_value <|! [ value_id ] <<|! typ_val <<<| at)

and boot_extern_value (at : region) (json : Yojson.Safe.t) : Value.t =
  let typ_json = Runtime.Type.Typ.Make.var ("json" $ no_region) [] in
  let value_json = Value.Make.extern typ_json json in
  Value.Make.(mop_extern_value <|! [ value_json ] <<|! typ_val <<<| at)

(* Operators *)

and boot_unop (unop : Il.unop) : Value.t =
  match unop with
  | #Bool.unop as unop -> boot_unop_bool unop
  | #Num.unop as unop -> boot_unop_num unop

and boot_unop_bool (unop : Bool.unop) : Value.t =
  match unop with `NotOp -> Value.Make.(mop_not_unop <|! [] <<|! typ_boolunop)

and boot_unop_num (unop : Num.unop) : Value.t =
  match unop with
  | `PlusOp -> Value.Make.(mop_plus_unop <|! [] <<|! typ_numunop)
  | `MinusOp -> Value.Make.(mop_minus_unop <|! [] <<|! typ_numunop)

and boot_binop (binop : Il.binop) : Value.t =
  match binop with
  | #Bool.binop as binop -> boot_binop_bool binop
  | #Num.binop as binop -> boot_binop_num binop

and boot_binop_bool (binop : Bool.binop) : Value.t =
  match binop with
  | `AndOp -> Value.Make.(mop_and_binop <|! [] <<|! typ_boolbinop)
  | `OrOp -> Value.Make.(mop_or_binop <|! [] <<|! typ_boolbinop)
  | `ImplOp -> Value.Make.(mop_impl_binop <|! [] <<|! typ_boolbinop)
  | `EquivOp -> Value.Make.(mop_equiv_binop <|! [] <<|! typ_boolbinop)

and boot_binop_num (binop : Num.binop) : Value.t =
  match binop with
  | `AddOp -> Value.Make.(mop_add_binop <|! [] <<|! typ_numbinop)
  | `SubOp -> Value.Make.(mop_sub_binop <|! [] <<|! typ_numbinop)
  | `MulOp -> Value.Make.(mop_mul_binop <|! [] <<|! typ_numbinop)
  | `DivOp -> Value.Make.(mop_div_binop <|! [] <<|! typ_numbinop)
  | `ModOp -> Value.Make.(mop_mod_binop <|! [] <<|! typ_numbinop)
  | `PowOp -> Value.Make.(mop_pow_binop <|! [] <<|! typ_numbinop)

and boot_cmpop (cmpop : Il.cmpop) : Value.t =
  match cmpop with
  | #Bool.cmpop as cmpop -> boot_cmpop_bool cmpop
  | #Num.cmpop as cmpop -> boot_cmpop_num cmpop

and boot_cmpop_bool (cmpop : Bool.cmpop) : Value.t =
  match cmpop with
  | `EqOp -> Value.Make.(mop_eq_cmpop <|! [] <<|! typ_polycmpop)
  | `NeOp -> Value.Make.(mop_ne_cmpop <|! [] <<|! typ_polycmpop)

and boot_cmpop_num (cmpop : Num.cmpop) : Value.t =
  match cmpop with
  | `LtOp -> Value.Make.(mop_lt_cmpop <|! [] <<|! typ_numcmpop)
  | `LeOp -> Value.Make.(mop_le_cmpop <|! [] <<|! typ_numcmpop)
  | `GtOp -> Value.Make.(mop_gt_cmpop <|! [] <<|! typ_numcmpop)
  | `GeOp -> Value.Make.(mop_ge_cmpop <|! [] <<|! typ_numcmpop)

(* Type arguments *)

and boot_targ (targ : Il.targ) : Value.t =
  let value_targ = boot_typ targ in
  Value.Make.(value_targ #@@ "targ")

and boot_targs (targs : Il.targ list) : Value.t =
  let values_targs = List.map boot_targ targs in
  Value.Make.list
    (Runtime.Type.Typ.Make.list
       (Runtime.Type.Typ.Make.var ("targ" $ no_region) []))
    values_targs

(* Type parameters *)

and boot_tparam (tparam : Il.tparam) : Value.t = boot_id tparam

and boot_tparams (tparams : Il.tparam list) : Value.t =
  let values_tparams = List.map boot_tparam tparams in
  Value.Make.list
    (Runtime.Type.Typ.Make.list
       (Runtime.Type.Typ.Make.var ("tparam" $ no_region) []))
    values_tparams

(* Parameters *)

and boot_param (param : Il.param) : Value.t =
  let at = param.at in
  match param.it with
  | ExpP typ ->
      let value_typ = boot_typ typ in
      Value.Make.(mop_exp_param <|! [ value_typ ] <<|! typ_param <<<| at)
  | DefP (id, tparams, params, typ) ->
      let value_id = boot_id id in
      let value_tparams = boot_tparams tparams in
      let value_params = boot_params params in
      let value_typ = boot_typ typ in
      Value.Make.(
        mop_def_param
        <|! [ value_id; value_tparams; value_params; value_typ ]
        <<|! typ_param <<<| at)

and boot_params (params : Il.param list) : Value.t =
  let values_params = List.map boot_param params in
  Value.Make.list (Runtime.Type.Typ.Make.list typ_param) values_params

(* Arguments *)

and boot_arg (arg : Il.arg) : Value.t =
  let at = arg.at in
  match arg.it with
  | ExpA e ->
      let value_exp = boot_exp e in
      Value.Make.(mop_exp_arg <|! [ value_exp ] <<|! typ_arg <<<| at)
  | DefA id ->
      let value_id = boot_id id in
      Value.Make.(mop_def_arg <|! [ value_id ] <<|! typ_arg <<<| at)

and boot_args (args : Il.arg list) : Value.t =
  let values_args = List.map boot_arg args in
  Value.Make.list (Runtime.Type.Typ.Make.list typ_arg) values_args

(* Expressions *)

and boot_exp (exp : Il.exp) : Value.t =
  let at = exp.at in
  match exp.it with
  | BoolE b -> boot_bool_exp at b
  | NumE num -> boot_num_exp at num
  | TextE t -> boot_text_exp at t
  | VarE id -> boot_var_exp at id
  | UnE (unop, _, e) -> boot_un_exp at unop e
  | BinE (binop, _, el, er) -> boot_bin_exp at binop el er
  | CmpE (cmpop, _, el, er) -> boot_cmp_exp at cmpop el er
  | UpCastE (typ, e) -> boot_upcast_exp at typ e
  | DownCastE (typ, e) -> boot_downcast_exp at typ e
  | SubE (e, typ) -> boot_sub_exp at e typ
  | MatchE (e, pattern) -> boot_match_exp at e pattern
  | TupleE exps -> boot_tuple_exp at exps
  | CaseE (mixop, exps) -> boot_case_exp at mixop exps
  | StrE expfields -> boot_struct_exp at expfields
  | OptE exp_opt -> boot_opt_exp at exp_opt
  | ListE exps -> boot_list_exp at exps
  | ConsE (eh, et) -> boot_cons_exp at eh et
  | CatE (el, er) -> boot_cat_exp at el er
  | MemE (ee, es) -> boot_mem_exp at ee es
  | LenE e -> boot_len_exp at e
  | DotE (e, atom) -> boot_dot_exp at e atom
  | IdxE (eb, ei) -> boot_idx_exp at eb ei
  | SliceE (eb, ei, en) -> boot_slice_exp at eb ei en
  | UpdE (eb, path, en) -> boot_upd_exp at eb path en
  | CallE (id, targs, args) -> boot_call_exp at id targs args
  | IterE (e, ie) -> boot_iter_exp at e ie

and boot_bool_exp (at : region) (b : bool) : Value.t =
  Value.Make.(mop_bool_exp <|! [ bool b ] <<|! typ_exp <<<| at)

and boot_num_exp (at : region) (num : Il.num) : Value.t =
  let value_num = boot_num_value at num in
  Value.Make.(value_num #@@ "exp")

and boot_text_exp (at : region) (t : string) : Value.t =
  Value.Make.(mop_text_exp <|! [ text t ] <<|! typ_exp <<<| at)

and boot_var_exp (at : region) (id : Il.id) : Value.t =
  let value_id = boot_id id in
  Value.Make.(mop_var_exp <|! [ value_id ] <<|! typ_exp <<<| at)

and boot_un_exp (at : region) (unop : Il.unop) (exp : Il.exp) : Value.t =
  let value_unop = boot_unop unop in
  let value_exp = boot_exp exp in
  Value.Make.(mop_un_exp <|! [ value_unop; value_exp ] <<|! typ_exp <<<| at)

and boot_bin_exp (at : region) (binop : Il.binop) (exp_l : Il.exp)
    (exp_r : Il.exp) : Value.t =
  let value_binop = boot_binop binop in
  let value_exp_l = boot_exp exp_l in
  let value_exp_r = boot_exp exp_r in
  Value.Make.(
    mop_bin_exp
    <|! [ value_binop; value_exp_l; value_exp_r ]
    <<|! typ_exp <<<| at)

and boot_cmp_exp (at : region) (cmpop : Il.cmpop) (exp_l : Il.exp)
    (exp_r : Il.exp) : Value.t =
  let value_cmpop = boot_cmpop cmpop in
  let value_exp_l = boot_exp exp_l in
  let value_exp_r = boot_exp exp_r in
  Value.Make.(
    mop_cmp_exp
    <|! [ value_cmpop; value_exp_l; value_exp_r ]
    <<|! typ_exp <<<| at)

and boot_upcast_exp (at : region) (typ : Il.typ) (exp : Il.exp) : Value.t =
  let value_typ = boot_typ typ in
  let value_exp = boot_exp exp in
  Value.Make.(mop_upcast_exp <|! [ value_typ; value_exp ] <<|! typ_exp <<<| at)

and boot_downcast_exp (at : region) (typ : Il.typ) (exp : Il.exp) : Value.t =
  let value_typ = boot_typ typ in
  let value_exp = boot_exp exp in
  Value.Make.(
    mop_downcast_exp <|! [ value_typ; value_exp ] <<|! typ_exp <<<| at)

and boot_sub_exp (at : region) (exp : Il.exp) (typ : Il.typ) : Value.t =
  let value_exp = boot_exp exp in
  let value_typ = boot_typ typ in
  Value.Make.(mop_sub_exp <|! [ value_exp; value_typ ] <<|! typ_exp <<<| at)

and boot_match_exp (at : region) (exp : Il.exp) (pattern : Il.pattern) : Value.t
    =
  let value_exp = boot_exp exp in
  let value_pattern = boot_pattern pattern in
  Value.Make.(
    mop_match_exp <|! [ value_exp; value_pattern ] <<|! typ_exp <<<| at)

and boot_tuple_exp (at : region) (exps : Il.exp list) : Value.t =
  let value_exps = boot_exps exps in
  Value.Make.(mop_tuple_exp <|! [ value_exps ] <<|! typ_exp <<<| at)

and boot_expcase (mixop : Il.mixop) (exps : Il.exp list) : Value.t =
  let value_mixop = boot_mixop mixop in
  let value_exps = boot_exps exps in
  Value.Make.(mop_expcase <|! [ value_mixop; value_exps ] <<|! typ_expcase)

and boot_case_exp (at : region) (mixop : Il.mixop) (exps : Il.exp list) :
    Value.t =
  let value_expcase = boot_expcase mixop exps in
  Value.Make.(mop_case_exp <|! [ value_expcase ] <<|! typ_exp <<<| at)

and boot_expfield ((atom, exp) : Il.atom * Il.exp) : Value.t =
  let value_atom = boot_atom atom in
  let value_exp = boot_exp exp in
  Value.Make.(mop_expfield <|! [ value_atom; value_exp ] <<|! typ_expfield)

and boot_expfields (expfields : (Il.atom * Il.exp) list) : Value.t =
  let values_expfields = List.map boot_expfield expfields in
  Value.Make.list (Runtime.Type.Typ.Make.list typ_expfield) values_expfields

and boot_struct_exp (at : region) (expfields : (Il.atom * Il.exp) list) :
    Value.t =
  let value_expfields = boot_expfields expfields in
  Value.Make.(mop_struct_exp <|! [ value_expfields ] <<|! typ_exp <<<| at)

and boot_opt_exp (at : region) (exp_opt : Il.exp option) : Value.t =
  let value_exp_opt = boot_exp_opt exp_opt in
  Value.Make.(mop_opt_exp <|! [ value_exp_opt ] <<|! typ_exp <<<| at)

and boot_list_exp (at : region) (exps : Il.exp list) : Value.t =
  let value_exps = boot_exps exps in
  Value.Make.(mop_list_exp <|! [ value_exps ] <<|! typ_exp <<<| at)

and boot_cons_exp (at : region) (exp_h : Il.exp) (exp_t : Il.exp) : Value.t =
  let value_exp_h = boot_exp exp_h in
  let value_exp_t = boot_exp exp_t in
  Value.Make.(
    mop_cons_exp <|! [ value_exp_h; value_exp_t ] <<|! typ_exp <<<| at)

and boot_cat_exp (at : region) (exp_l : Il.exp) (exp_r : Il.exp) : Value.t =
  let value_exp_l = boot_exp exp_l in
  let value_exp_r = boot_exp exp_r in
  Value.Make.(mop_cat_exp <|! [ value_exp_l; value_exp_r ] <<|! typ_exp <<<| at)

and boot_mem_exp (at : region) (exp_e : Il.exp) (exp_s : Il.exp) : Value.t =
  let value_exp_e = boot_exp exp_e in
  let value_exp_s = boot_exp exp_s in
  Value.Make.(mop_mem_exp <|! [ value_exp_e; value_exp_s ] <<|! typ_exp <<<| at)

and boot_len_exp (at : region) (exp : Il.exp) : Value.t =
  let value_exp = boot_exp exp in
  Value.Make.(mop_len_exp <|! [ value_exp ] <<|! typ_exp <<<| at)

and boot_dot_exp (at : region) (exp : Il.exp) (atom : Il.atom) : Value.t =
  let value_exp = boot_exp exp in
  let value_atom = boot_atom atom in
  Value.Make.(mop_dot_exp <|! [ value_exp; value_atom ] <<|! typ_exp <<<| at)

and boot_idx_exp (at : region) (exp_b : Il.exp) (exp_i : Il.exp) : Value.t =
  let value_exp_b = boot_exp exp_b in
  let value_exp_i = boot_exp exp_i in
  Value.Make.(mop_idx_exp <|! [ value_exp_b; value_exp_i ] <<|! typ_exp <<<| at)

and boot_slice_exp (at : region) (exp_b : Il.exp) (exp_i : Il.exp)
    (exp_n : Il.exp) : Value.t =
  let value_exp_b = boot_exp exp_b in
  let value_exp_i = boot_exp exp_i in
  let value_exp_n = boot_exp exp_n in
  Value.Make.(
    mop_slice_exp
    <|! [ value_exp_b; value_exp_i; value_exp_n ]
    <<|! typ_exp <<<| at)

and boot_upd_exp (at : region) (exp_b : Il.exp) (path : Il.path)
    (exp_n : Il.exp) : Value.t =
  let value_exp_b = boot_exp exp_b in
  let value_path = boot_path path in
  let value_exp_n = boot_exp exp_n in
  Value.Make.(
    mop_upd_exp
    <|! [ value_exp_b; value_path; value_exp_n ]
    <<|! typ_exp <<<| at)

and boot_call_exp (at : region) (id : Il.id) (targs : Il.targ list)
    (args : Il.arg list) : Value.t =
  let value_id = boot_id id in
  let value_targs = boot_targs targs in
  let value_args = boot_args args in
  Value.Make.(
    mop_call_exp <|! [ value_id; value_targs; value_args ] <<|! typ_exp <<<| at)

and boot_iter_exp (at : region) (exp : Il.exp) (iterexp : Il.iterexp) : Value.t
    =
  let value_exp = boot_exp exp in
  let value_iterexp = boot_iterexp iterexp in
  Value.Make.(
    mop_iter_exp <|! [ value_exp; value_iterexp ] <<|! typ_exp <<<| at)

and boot_exps (exps : Il.exp list) : Value.t =
  let values_exps = List.map boot_exp exps in
  Value.Make.list (Runtime.Type.Typ.Make.list typ_exp) values_exps

and boot_exp_opt (exp_opt : Il.exp option) : Value.t =
  Value.Make.opt
    (Runtime.Type.Typ.Make.opt typ_exp)
    (Option.map boot_exp exp_opt)

(* Paths *)

and boot_path (path : Il.path) : Value.t =
  let at = path.at in
  match path.it with
  | RootP -> Value.Make.(mop_root_path <|! [] <<|! typ_path <<<| at)
  | IdxP (path, exp) ->
      let value_path = boot_path path in
      let value_exp = boot_exp exp in
      Value.Make.(
        mop_idx_path <|! [ value_path; value_exp ] <<|! typ_path <<<| at)
  | SliceP (path, exp_i, exp_n) ->
      let value_path = boot_path path in
      let value_exp_i = boot_exp exp_i in
      let value_exp_n = boot_exp exp_n in
      Value.Make.(
        mop_slice_path
        <|! [ value_path; value_exp_i; value_exp_n ]
        <<|! typ_path <<<| at)
  | DotP (path, atom) ->
      let value_path = boot_path path in
      let value_atom = boot_atom atom in
      Value.Make.(
        mop_dot_path <|! [ value_path; value_atom ] <<|! typ_path <<<| at)

(* Patterns *)

and boot_pattern (pattern : Il.pattern) : Value.t =
  match pattern with
  | CaseP mixop ->
      let value_mixop = boot_mixop mixop in
      Value.Make.(mop_case_pattern <|! [ value_mixop ] <<|! typ_pattern)
  | ListP `Cons ->
      Value.Make.(mop_list_cons_pattern <|! [] <<|! typ_listpattern)
  | ListP (`Fixed n) ->
      Value.Make.(
        mop_list_fixed_pattern
        <|! [ nat (Bigint.of_int n) ]
        <<|! typ_listpattern)
  | ListP `Nil -> Value.Make.(mop_list_nil_pattern <|! [] <<|! typ_listpattern)
  | OptP `Some -> Value.Make.(mop_opt_some_pattern <|! [] <<|! typ_optpattern)
  | OptP `None -> Value.Make.(mop_opt_none_pattern <|! [] <<|! typ_optpattern)

(* Iter expressions and premises *)

and boot_iterexp ((iter, vars) : Il.iterexp) : Value.t =
  let value_iter = boot_iter iter in
  let value_vars = boot_vars vars in
  Value.Make.(mop_iterexp <|! [ value_iter; value_vars ] <<|! typ_iterexp)

and boot_iterprem ((iter, vars_in, vars_out) : Il.iterprem) : Value.t =
  let value_iter = boot_iter iter in
  let value_vars_in = boot_vars vars_in in
  let value_vars_out = boot_vars vars_out in
  Value.Make.(
    mop_iterprem
    <|! [ value_iter; value_vars_in; value_vars_out ]
    <<|! typ_iterprem)

(* Premises *)

and boot_prem (prem : Il.prem) : Value.t =
  let at = prem.at in
  match prem.it with
  | RulePr (id, (_, exps), input) -> boot_rel_prem at id exps input
  | IfPr e -> boot_if_prem at e
  | IfHoldPr (id, (_, exps)) -> boot_if_hold_prem at id exps
  | IfNotHoldPr (id, (_, exps)) -> boot_if_nothold_prem at id exps
  | LetPr (el, er) -> boot_let_prem at el er
  | IterPr (p, ip) -> boot_iter_prem at p ip
  | DebugPr e -> boot_debug_prem at e

and boot_rel_prem (at : region) (id : Il.id) (exps : Il.exp list)
    (input : Hints.Input.t) : Value.t =
  let value_id = boot_id id in
  let exps_in, exps_out = Hints.Input.split input exps in
  let value_exps_in = boot_exps exps_in in
  let value_exps_out = boot_exps exps_out in
  Value.Make.(
    mop_rel_prem
    <|! [ value_id; value_exps_in; value_exps_out ]
    <<|! typ_prem <<<| at)

and boot_if_prem (at : region) (e : Il.exp) : Value.t =
  let value_exp = boot_exp e in
  Value.Make.(mop_if_prem <|! [ value_exp ] <<|! typ_prem <<<| at)

and boot_if_hold_prem (at : region) (id : Il.id) (exps : Il.exp list) : Value.t
    =
  let value_id = boot_id id in
  let value_exps = boot_exps exps in
  Value.Make.(
    mop_if_hold_prem <|! [ value_id; value_exps ] <<|! typ_prem <<<| at)

and boot_if_nothold_prem (at : region) (id : Il.id) (exps : Il.exp list) :
    Value.t =
  let value_id = boot_id id in
  let value_exps = boot_exps exps in
  Value.Make.(
    mop_if_nothold_prem <|! [ value_id; value_exps ] <<|! typ_prem <<<| at)

and boot_let_prem (at : region) (el : Il.exp) (er : Il.exp) : Value.t =
  let value_el = boot_exp el in
  let value_er = boot_exp er in
  Value.Make.(mop_let_prem <|! [ value_el; value_er ] <<|! typ_prem <<<| at)

and boot_iter_prem (at : region) (p : Il.prem) (ip : Il.iterprem) : Value.t =
  let value_prem = boot_prem p in
  let value_iterprem = boot_iterprem ip in
  Value.Make.(
    mop_iter_prem <|! [ value_prem; value_iterprem ] <<|! typ_prem <<<| at)

and boot_debug_prem (at : region) (e : Il.exp) : Value.t =
  let value_exp = boot_exp e in
  Value.Make.(mop_debug_prem <|! [ value_exp ] <<|! typ_prem <<<| at)

and boot_prems (prems : Il.prem list) : Value.t =
  let values_prems = List.map boot_prem prems in
  Value.Make.list (Runtime.Type.Typ.Make.list typ_prem) values_prems

(* Rule matching and paths *)

and boot_rulmatch ((_, exps_input, prems) : Il.rulematch) : Value.t =
  let value_exps = boot_exps exps_input in
  let value_prems = boot_prems prems in
  Value.Make.(mop_rulematch <|! [ value_exps; value_prems ] <<|! typ_rulmatch)

and boot_rulpath ((id, prems, exps_output) : Il.rulepath) : Value.t =
  let value_id = boot_id id in
  let value_exps_output = boot_exps exps_output in
  let value_prems = boot_prems prems in
  Value.Make.(
    mop_rulepath
    <|! [ value_id; value_exps_output; value_prems ]
    <<|! typ_rulpath)

and boot_rulpaths (rulpaths : Il.rulepath list) : Value.t =
  let values_rulpaths = List.map boot_rulpath rulpaths in
  Value.Make.list (Runtime.Type.Typ.Make.list typ_rulpath) values_rulpaths

and boot_rulgroup (rg : Il.rulegroup) : Value.t =
  let at = rg.at in
  let id, rulmatch_, rulpaths = rg.it in
  let value_id = boot_id id in
  let value_rulmatch = boot_rulmatch rulmatch_ in
  let value_rulpaths = boot_rulpaths rulpaths in
  Value.Make.(
    mop_rulegroup
    <|! [ value_id; value_rulmatch; value_rulpaths ]
    <<|! typ_rulgroup <<<| at)

and boot_rulgroups (rulgroups : Il.rulegroup list) : Value.t =
  let values_rulgroups = List.map boot_rulgroup rulgroups in
  Value.Make.list (Runtime.Type.Typ.Make.list typ_rulgroup) values_rulgroups

and boot_elsgroup (eg : Il.elsegroup) : Value.t =
  let at = eg.at in
  let id, rulmatch_, rulpath_ = eg.it in
  let value_id = boot_id id in
  let value_rulmatch = boot_rulmatch rulmatch_ in
  let value_rulpath = boot_rulpath rulpath_ in
  Value.Make.(
    mop_elsegroup
    <|! [ value_id; value_rulmatch; value_rulpath ]
    <<|! typ_elsgroup <<<| at)

and boot_elsgroup_opt (elsgroup_opt : Il.elsegroup option) : Value.t =
  Value.Make.opt
    (Runtime.Type.Typ.Make.opt typ_elsgroup)
    (Option.map boot_elsgroup elsgroup_opt)

(* Clauses *)

and boot_clause (clause : Il.clause) : Value.t =
  let at = clause.at in
  let args, exp, prems = clause.it in
  let value_args = boot_args args in
  let value_exp = boot_exp exp in
  let value_prems = boot_prems prems in
  Value.Make.(
    mop_clause
    <|! [ value_args; value_exp; value_prems ]
    <<|! typ_clause <<<| at)

and boot_clauses (clauses : Il.clause list) : Value.t =
  let values_clauses = List.map boot_clause clauses in
  Value.Make.list (Runtime.Type.Typ.Make.list typ_clause) values_clauses

and boot_elsclause (elsclause : Il.elseclause) : Value.t = boot_clause elsclause

and boot_elsclause_opt (elsclause_opt : Il.elseclause option) : Value.t =
  Value.Make.opt
    (Runtime.Type.Typ.Make.opt
       (Runtime.Type.Typ.Make.var ("elsclause" $ no_region) []))
    (Option.map boot_elsclause elsclause_opt)

(* Table rows *)

and boot_tablerow (tablerow : Il.tablerow) : Value.t =
  let at = tablerow.at in
  let _exps, args, exp, prems = tablerow.it in
  let value_args = boot_args args in
  let value_exp = boot_exp exp in
  let value_prems = boot_prems prems in
  Value.Make.(
    mop_clause
    <|! [ value_args; value_exp; value_prems ]
    <<|! typ_tblrow <<<| at)

and boot_tablerows (tablerows : Il.tablerow list) : Value.t =
  let values_tablerows = List.map boot_tablerow tablerows in
  Value.Make.list (Runtime.Type.Typ.Make.list typ_tblrow) values_tablerows

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
  | RelD (id, nottyp, input, rulgroups, eg, _) ->
      boot_rel_def at id nottyp input rulgroups eg |> wrap_some
  | ExternDecD (id, tparams, params, typ, _) ->
      boot_extern_func_def at id tparams params typ |> wrap_some
  | BuiltinDecD (id, tparams, params, typ, _) ->
      boot_builtin_func_def at id tparams params typ |> wrap_some
  | TableDecD (id, params, typ, tablerows, _) ->
      boot_table_func_def at id params typ tablerows |> wrap_some
  | FuncDecD (id, tparams, params, typ, clauses, ec, _) ->
      boot_func_def at id tparams params typ clauses ec |> wrap_some

and boot_extern_typ_def (at : region) (id : Il.id) : Value.t =
  let value_id = boot_id id in
  Value.Make.(mop_extern_typ_def <|! [ value_id ] <<|! typ_defn <<<| at)

and boot_typ_def (at : region) (id : Il.id) (tparams : Il.tparam list)
    (deftyp : Il.deftyp) : Value.t =
  let value_id = boot_id id in
  let value_tparams = boot_tparams tparams in
  let value_deftyp = boot_deftyp deftyp in
  Value.Make.(
    mop_typ_def
    <|! [ value_id; value_tparams; value_deftyp ]
    <<|! typ_defn <<<| at)

and boot_extern_rel_def (at : region) (id : Il.id) (nottyp : Il.nottyp)
    (input : Hints.Input.t) : Value.t =
  let _, typs = nottyp.it in
  let typs_in, typs_out = Hints.Input.split input typs in
  let value_id = boot_id id in
  let value_typs_in = boot_typs typs_in in
  let value_typs_out = boot_typs typs_out in
  Value.Make.(
    mop_extern_rel_def
    <|! [ value_id; value_typs_in; value_typs_out ]
    <<|! typ_defn <<<| at)

and boot_rel_def (at : region) (id : Il.id) (nottyp : Il.nottyp)
    (input : Hints.Input.t) (rulgroups : Il.rulegroup list)
    (eg : Il.elsegroup option) : Value.t =
  let _, typs = nottyp.it in
  let typs_in, typs_out = Hints.Input.split input typs in
  let value_id = boot_id id in
  let value_typs_in = boot_typs typs_in in
  let value_typs_out = boot_typs typs_out in
  let value_rulgroups = boot_rulgroups rulgroups in
  let value_elsgroup = boot_elsgroup_opt eg in
  Value.Make.(
    mop_rel_def
    <|! [
          value_id;
          value_typs_in;
          value_typs_out;
          value_rulgroups;
          value_elsgroup;
        ]
    <<|! typ_defn <<<| at)

and boot_extern_func_def (at : region) (id : Il.id) (tparams : Il.tparam list)
    (params : Il.param list) (typ : Il.typ) : Value.t =
  let value_id = boot_id id in
  let value_tparams = boot_tparams tparams in
  let value_params = boot_params params in
  let value_typ = boot_typ typ in
  Value.Make.(
    mop_extern_func_def
    <|! [ value_id; value_tparams; value_params; value_typ ]
    <<|! typ_defn <<<| at)

and boot_builtin_func_def (at : region) (id : Il.id) (tparams : Il.tparam list)
    (params : Il.param list) (typ : Il.typ) : Value.t =
  let value_id = boot_id id in
  let value_tparams = boot_tparams tparams in
  let value_params = boot_params params in
  let value_typ = boot_typ typ in
  Value.Make.(
    mop_builtin_func_def
    <|! [ value_id; value_tparams; value_params; value_typ ]
    <<|! typ_defn <<<| at)

and boot_table_func_def (at : region) (id : Il.id) (params : Il.param list)
    (typ : Il.typ) (tablerows : Il.tablerow list) : Value.t =
  let value_id = boot_id id in
  let value_params = boot_params params in
  let value_typ = boot_typ typ in
  let value_tablerows = boot_tablerows tablerows in
  Value.Make.(
    mop_table_func_def
    <|! [ value_id; value_params; value_typ; value_tablerows ]
    <<|! typ_defn <<<| at)

and boot_func_def (at : region) (id : Il.id) (tparams : Il.tparam list)
    (params : Il.param list) (typ : Il.typ) (clauses : Il.clause list)
    (ec : Il.elseclause option) : Value.t =
  let value_id = boot_id id in
  let value_tparams = boot_tparams tparams in
  let value_params = boot_params params in
  let value_typ = boot_typ typ in
  let value_clauses = boot_clauses clauses in
  let value_elsclause = boot_elsclause_opt ec in
  Value.Make.(
    mop_func_def
    <|! [
          value_id;
          value_tparams;
          value_params;
          value_typ;
          value_clauses;
          value_elsclause;
        ]
    <<|! typ_defn <<<| at)

(* Specification *)

let boot_spec (spec : Il.spec) : Value.t =
  let values_def = List.map boot_def spec |> List.filter_map Fun.id in
  let typ_script = Runtime.Type.Typ.Make.var ("script" $ no_region) [] in
  Value.Make.list typ_script values_def
