module Atom = Domain.Atom
module Mixop = Domain.Mixop
module Mixfix = Domain.Mixfix
module Il = Lang.Il
open Lang.Xl
open Mixops
open Typs
module Value = Runtime.Value
module VCache = Runtime.Dynamic.Caches.ValueCache
module MCache = Domain.Caches.MixopCache
open Caches
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
  let cached = !find_boot_mixop_cache mixop in
  match cached with
  | Some value -> value
  | None ->
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
      let value = Value.Make.(value_atoms_matrix #@@ "mixop") in
      !add_boot_mixop_cache mixop value;
      value

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
  let mixop, typs = Mixfix.split nottyp.it in
  let value_mixop = boot_mixop mixop in
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
  let cached =
    let cached = !find_boot_value_pingpong_cache value in
    match cached with
    | Some value -> Some value
    | None -> !find_boot_value_cache value
  in
  match cached with
  | Some value_value -> value_value
  | None ->
      let at = value.at in
      let value_value =
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
      in
      !add_boot_value_cache value value_value;
      !add_unboot_value_pingpong_cache value_value value;
      value_value

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

and boot_valuecase (valuecase : Il.valuecase) : Value.t =
  let mixop, values = Mixfix.split valuecase in
  let value_mixop = boot_mixop mixop in
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
  | SubE (e, typ, _) -> boot_sub_exp at e typ
  | MatchE (e, pattern) -> boot_match_exp at e pattern
  | TupleE exps -> boot_tuple_exp at exps
  | CaseE notexp -> boot_case_exp at notexp
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

and boot_expcase (notexp : Il.notexp) : Value.t =
  let mixop, exps = Mixfix.split notexp in
  let value_mixop = boot_mixop mixop in
  let value_exps = boot_exps exps in
  Value.Make.(mop_expcase <|! [ value_mixop; value_exps ] <<|! typ_expcase)

and boot_case_exp (at : region) (notexp : Il.notexp) : Value.t =
  let value_expcase = boot_expcase notexp in
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

(* Iter expressions *)

and boot_iterexp ((iter, vars) : Il.iterexp) : Value.t =
  let value_iter = boot_iter iter in
  let value_vars = boot_vars vars in
  Value.Make.(mop_iterexp <|! [ value_iter; value_vars ] <<|! typ_iterexp)
