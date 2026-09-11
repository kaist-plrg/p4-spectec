module Atom = Domain.Atom
module Mixfix = Domain.Mixfix
module Il = Lang.Il
module Typ = Runtime.Type.Typ
module Value = Runtime.Value
module VCache = Runtime.Dynamic.Caches.ValueCache
open Mixops
open Stub
open Caches
open Util.Source

(* Errors *)

let error = Error.error

(* Forward references for match dispatch tables,
   populated after all sub-match functions are defined *)

let unboot_value_mtchtbl : Il.value Value.Get.mtchtbl ref =
  ref (Value.Get.MtchTbl.create 0)

let unboot_typ_mtchtbl : Il.typ Value.Get.mtchtbl ref =
  ref (Value.Get.MtchTbl.create 0)

let unboot_deftyp_mtchtbl : Il.deftyp Value.Get.mtchtbl ref =
  ref (Value.Get.MtchTbl.create 0)

let unboot_unop_mtchtbl : Il.unop Value.Get.mtchtbl ref =
  ref (Value.Get.MtchTbl.create 0)

let unboot_binop_mtchtbl : Il.binop Value.Get.mtchtbl ref =
  ref (Value.Get.MtchTbl.create 0)

let unboot_cmpop_mtchtbl : Il.cmpop Value.Get.mtchtbl ref =
  ref (Value.Get.MtchTbl.create 0)

let unboot_arg_mtchtbl : Il.arg Value.Get.mtchtbl ref =
  ref (Value.Get.MtchTbl.create 0)

let unboot_exp_mtchtbl : Il.exp Value.Get.mtchtbl ref =
  ref (Value.Get.MtchTbl.create 0)

let unboot_path_mtchtbl : Il.path Value.Get.mtchtbl ref =
  ref (Value.Get.MtchTbl.create 0)

let unboot_pattern_mtchtbl : Il.pattern Value.Get.mtchtbl ref =
  ref (Value.Get.MtchTbl.create 0)

(* Identifiers *)

let unboot_id (value_id : Value.t) : Il.id =
  let id = Value.Get.text value_id in
  id $ value_id.at

(* Atoms *)

let unboot_atom (value_atom : Value.t) : Il.atom =
  let atom = value_atom |> Value.Get.text |> Atom.atom_of_string in
  atom $ value_atom.at

(* Mixops *)

let unboot_mixop (value_mixop : Value.t) : Il.mixop =
  let cached = !find_unboot_mixop_cache value_mixop in
  match cached with
  | Some mixop -> mixop
  | None ->
      let atoms_matrix =
        value_mixop |> Value.Get.list
        |> List.map (fun value_atoms ->
               value_atoms |> Value.Get.list |> List.map unboot_atom
               |> List.map it)
      in
      let mixop = Value.Mixops.of_atoms_matrix atoms_matrix in
      !add_unboot_mixop_cache value_mixop mixop;
      mixop

(* Iterators *)

let unboot_iter_tbl =
  Value.Get.build_mtchtbl
    [ (mop_quest, fun _ _ -> Il.Opt); (mop_star, fun _ _ -> Il.List) ]

let unboot_iter (value_iter : Value.t) : Il.iter =
  Value.Get.mtch_dispatch value_iter unboot_iter_tbl (fun _ _ ->
      error "@unboot_iter")

let unboot_iters (value_iters : Value.t) : Il.iter list =
  value_iters |> Value.Get.list |> List.map unboot_iter

(* Variables *)

let rec unboot_vari (value_vari : Value.t) : Il.var =
  let values = Value.Get.(value_vari |>>! mop_vari) in
  let id = Value.Get.nth 0 values |> unboot_id in
  let typ = Value.Get.nth 1 values |> unboot_typ in
  let iters = Value.Get.nth 2 values |> unboot_iters in
  (id, typ, iters)

and unboot_varis (value_varis : Value.t) : Il.var list =
  value_varis |> Value.Get.list |> List.map unboot_vari

(* Types *)

and unboot_typ (value_typ : Value.t) : Il.typ =
  let cached = !find_unboot_typ_cache value_typ in
  match cached with
  | Some typ -> typ
  | None ->
      let typ =
        Value.Get.mtch_dispatch value_typ !unboot_typ_mtchtbl (fun _ _ ->
            error "@unboot_typ")
      in
      !add_unboot_typ_cache value_typ typ;
      typ

and unboot_typs (value_typs : Value.t) : Il.typ list =
  value_typs |> Value.Get.list |> List.map unboot_typ

and unboot_bool_typ (at : region) (_ : Value.t list) : Il.typ = Il.BoolT $ at

and unboot_num_typ_nat (at : region) (_ : Value.t list) : Il.typ =
  Il.NumT `NatT $ at

and unboot_num_typ_int (at : region) (_ : Value.t list) : Il.typ =
  Il.NumT `IntT $ at

and unboot_text_typ (at : region) (_ : Value.t list) : Il.typ = Il.TextT $ at

and unboot_var_typ (at : region) (values : Value.t list) : Il.typ =
  match values with
  | [ value_id; value_targs ] ->
      let id = unboot_id value_id in
      let targs = unboot_targs value_targs in
      Il.VarT (id, targs) $ at
  | _ -> error "@unboot_var_typ"

and unboot_tuple_typ (at : region) (values : Value.t list) : Il.typ =
  match values with
  | [ value_typs ] ->
      let typs = unboot_typs value_typs in
      Il.TupleT typs $ at
  | _ -> error "@unboot_tuple_typ"

and unboot_iter_typ (at : region) (values : Value.t list) : Il.typ =
  match values with
  | [ value_typ; value_iter ] ->
      let typ = unboot_typ value_typ in
      let iter = unboot_iter value_iter in
      Il.IterT (typ, iter) $ at
  | _ -> error "@unboot_iter_typ"

and unboot_func_typ (at : region) (_ : Value.t list) : Il.typ =
  let tparams, typs, typ = stub_func_typ_params () in
  Il.FuncT (tparams, typs, typ) $ at

(* Type arguments and parameters *)

and unboot_targ (value_targ : Value.t) : Il.targ = unboot_typ value_targ

and unboot_targs (value_targs : Value.t) : Il.targ list =
  value_targs |> Value.Get.list |> List.map unboot_targ

and unboot_tparam (value_tparam : Value.t) : Il.tparam = unboot_id value_tparam

and unboot_tparams (value_tparams : Value.t) : Il.tparam list =
  value_tparams |> Value.Get.list |> List.map unboot_tparam

(* Defined types *)

and unboot_deftyp (value_deftyp : Value.t) : Il.deftyp =
  Value.Get.mtch_dispatch value_deftyp !unboot_deftyp_mtchtbl (fun _ _ ->
      error "@unboot_deftyp")

and unboot_plain_deftyp (at : region) (values : Value.t list) : Il.deftyp =
  match values with
  | [ value_typ ] ->
      let typ = unboot_typ value_typ in
      Il.PlainT typ $ at
  | _ -> error "@unboot_plain_deftyp"

and unboot_typfield (value_typfield : Value.t) : Il.typfield =
  let values = Value.Get.(value_typfield |>>! mop_typfield) in
  let atom = Value.Get.nth 0 values |> unboot_atom in
  let typ = Value.Get.nth 1 values |> unboot_typ in
  (atom, typ)

and unboot_typfields (value_typfields : Value.t) : Il.typfield list =
  value_typfields |> Value.Get.list |> List.map unboot_typfield

and unboot_struct_deftyp (at : region) (values : Value.t list) : Il.deftyp =
  match values with
  | [ value_typfields ] ->
      let typfields = unboot_typfields value_typfields in
      Il.StructT typfields $ at
  | _ -> error "@unboot_struct_deftyp"

and unboot_typcase (value_typcase : Value.t) : Il.typcase =
  let values = Value.Get.(value_typcase |>>! mop_typcase) in
  let mixop = Value.Get.nth 0 values |> unboot_mixop in
  let typs = Value.Get.nth 1 values |> unboot_typs in
  let nottyp = Mixfix.fill mixop typs $ value_typcase.at in
  (nottyp, stub_typorigin (), [])

and unboot_typcases (value_typcases : Value.t) : Il.typcase list =
  value_typcases |> Value.Get.list |> List.map unboot_typcase

and unboot_variant_deftyp (at : region) (values : Value.t list) : Il.deftyp =
  match values with
  | [ value_typcases ] ->
      let typcases = unboot_typcases value_typcases in
      Il.VariantT typcases $ at
  | _ -> error "@unboot_variant_deftyp"

(* Values *)

and unboot_value (value_value : Value.t) : Il.value =
  let cached =
    let cached = !find_unboot_value_pingpong_cache value_value in
    match cached with
    | Some value -> Some value
    | None -> !find_unboot_value_cache value_value
  in
  match cached with
  | Some value -> value
  | None ->
      let value =
        Value.Get.mtch_dispatch value_value !unboot_value_mtchtbl (fun _ _ ->
            error "@unboot_value")
      in
      !add_unboot_value_cache value_value value;
      !add_boot_value_pingpong_cache value value_value;
      value

and unboot_values (value_values : Value.t) : Il.value list =
  value_values |> Value.Get.list |> List.map unboot_value

and unboot_bool_value (at : region) (values : Value.t list) : Il.value =
  match values with
  | [ value_bool ] ->
      let b = Value.Get.bool value_bool in
      Value.Make.bool ~at b
  | _ -> error "@unboot_bool_value"

and unboot_num_value_nat (at : region) (values : Value.t list) : Il.value =
  match values with
  | [ value_nat ] ->
      let n = Value.Get.num value_nat in
      Value.Make.num ~at n
  | _ -> error "@unboot_num_value_nat"

and unboot_num_value_int (at : region) (values : Value.t list) : Il.value =
  match values with
  | [ value_int ] ->
      let n = Value.Get.num value_int in
      Value.Make.num ~at n
  | _ -> error "@unboot_num_value_int"

and unboot_text_value (at : region) (values : Value.t list) : Il.value =
  match values with
  | [ value_text ] ->
      let s = Value.Get.text value_text in
      Value.Make.text ~at s
  | _ -> error "@unboot_text_value"

and unboot_valuefield (value_valuefield : Value.t) : Il.valuefield =
  let values = Value.Get.(value_valuefield |>>! mop_valuefield) in
  let atom = Value.Get.nth 0 values |> unboot_atom in
  let v = Value.Get.nth 1 values |> unboot_value in
  (atom, v)

and unboot_valuefields (value_valuefields : Value.t) : Il.valuefield list =
  value_valuefields |> Value.Get.list |> List.map unboot_valuefield

and unboot_struct_value (at : region) (values : Value.t list) : Il.value =
  match values with
  | [ value_valuefields ] ->
      let valuefields = unboot_valuefields value_valuefields in
      let typ = Typ.Make.var ("val" $ no_region) [] in
      Value.Make.str ~at typ valuefields
  | _ -> error "@unboot_struct_value"

and unboot_valuecase (value_valuecase : Value.t) : Il.valuecase =
  let values = Value.Get.(value_valuecase |>>! mop_valuecase) in
  let mixop = Value.Get.nth 0 values |> unboot_mixop in
  let values = Value.Get.nth 1 values |> unboot_values in
  let valuecase = Mixfix.fill mixop values in
  valuecase

and unboot_variant_value (at : region) (values : Value.t list) : Il.value =
  match values with
  | [ value_valcase ] ->
      let valuecase = unboot_valuecase value_valcase in
      let typ = Typ.Make.var ("val" $ no_region) [] in
      Value.Make.case ~at typ valuecase
  | _ -> error "@unboot_variant_value"

and unboot_tuple_value (at : region) (values : Value.t list) : Il.value =
  match values with
  | [ value_vals ] ->
      let vals = unboot_values value_vals in
      let typ = Typ.Make.var ("val" $ no_region) [] in
      Value.Make.tuple ~at typ vals
  | _ -> error "@unboot_tuple_value"

and unboot_value_opt (value_value_opt : Value.t) : Il.value option =
  value_value_opt |> Value.Get.opt |> Option.map unboot_value

and unboot_opt_value (at : region) (values : Value.t list) : Il.value =
  match values with
  | [ value_value_opt ] ->
      let value_opt = unboot_value_opt value_value_opt in
      let typ = Typ.Make.var ("val" $ no_region) [] |> Typ.Make.opt in
      Value.Make.opt ~at typ value_opt
  | _ -> error "@unboot_opt_value"

and unboot_list_value (at : region) (values : Value.t list) : Il.value =
  match values with
  | [ value_vals ] ->
      let vals = unboot_values value_vals in
      let typ = Typ.Make.var ("val" $ no_region) [] |> Typ.Make.list in
      Value.Make.list ~at typ vals
  | _ -> error "@unboot_list_value"

and unboot_func_value (at : region) (values : Value.t list) : Il.value =
  match values with
  | [ value_id ] ->
      let id = unboot_id value_id in
      let tparams, typs, typ = stub_func_typ_params () in
      Value.Make.func ~at id tparams typs typ
  | _ -> error "@unboot_func_value"

and unboot_extern_value (at : region) (values : Value.t list) : Il.value =
  match values with
  | [ value_json ] ->
      let json = Value.Get.extern value_json in
      let typ = Typ.Make.var ("json" $ no_region) [] in
      Value.Make.extern ~at typ json
  | _ -> error "@unboot_extern_value"

(* Operators *)

and unboot_unop (value_unop : Value.t) : Il.unop =
  Value.Get.mtch_dispatch value_unop !unboot_unop_mtchtbl (fun _ _ ->
      error "@unboot_unop")

and unboot_not_unop (_ : region) (_ : Value.t list) : Il.unop = `NotOp
and unboot_plus_unop (_ : region) (_ : Value.t list) : Il.unop = `PlusOp
and unboot_minus_unop (_ : region) (_ : Value.t list) : Il.unop = `MinusOp

and unboot_binop (value_binop : Value.t) : Il.binop =
  Value.Get.mtch_dispatch value_binop !unboot_binop_mtchtbl (fun _ _ ->
      error "@unboot_binop")

and unboot_and_binop (_ : region) (_ : Value.t list) : Il.binop = `AndOp
and unboot_or_binop (_ : region) (_ : Value.t list) : Il.binop = `OrOp
and unboot_impl_binop (_ : region) (_ : Value.t list) : Il.binop = `ImplOp
and unboot_equiv_binop (_ : region) (_ : Value.t list) : Il.binop = `EquivOp
and unboot_add_binop (_ : region) (_ : Value.t list) : Il.binop = `AddOp
and unboot_sub_binop (_ : region) (_ : Value.t list) : Il.binop = `SubOp
and unboot_mul_binop (_ : region) (_ : Value.t list) : Il.binop = `MulOp
and unboot_div_binop (_ : region) (_ : Value.t list) : Il.binop = `DivOp
and unboot_mod_binop (_ : region) (_ : Value.t list) : Il.binop = `ModOp
and unboot_pow_binop (_ : region) (_ : Value.t list) : Il.binop = `PowOp

and unboot_cmpop (value_cmpop : Value.t) : Il.cmpop =
  Value.Get.mtch_dispatch value_cmpop !unboot_cmpop_mtchtbl (fun _ _ ->
      error "@unboot_cmpop")

and unboot_eq_cmpop (_ : region) (_ : Value.t list) : Il.cmpop = `EqOp
and unboot_ne_cmpop (_ : region) (_ : Value.t list) : Il.cmpop = `NeOp
and unboot_lt_cmpop (_ : region) (_ : Value.t list) : Il.cmpop = `LtOp
and unboot_le_cmpop (_ : region) (_ : Value.t list) : Il.cmpop = `LeOp
and unboot_gt_cmpop (_ : region) (_ : Value.t list) : Il.cmpop = `GtOp
and unboot_ge_cmpop (_ : region) (_ : Value.t list) : Il.cmpop = `GeOp

(* Arguments *)

and unboot_arg (value_arg : Value.t) : Il.arg =
  Value.Get.mtch_dispatch value_arg !unboot_arg_mtchtbl (fun _ _ ->
      error "@unboot_arg")

and unboot_exp_arg (at : region) (values : Value.t list) : Il.arg =
  match values with
  | [ value_exp ] ->
      let exp = unboot_exp value_exp in
      Il.ExpA exp $ at
  | _ -> error "@unboot_arg/EXP"

and unboot_def_arg (at : region) (values : Value.t list) : Il.arg =
  match values with
  | [ value_id ] ->
      let id = unboot_id value_id in
      Il.DefA id $ at
  | _ -> error "@unboot_arg/FUN"

and unboot_args (value_args : Value.t) : Il.arg list =
  value_args |> Value.Get.list |> List.map unboot_arg

(* Expressions *)

and unboot_exp (value_exp : Value.t) : Il.exp =
  Value.Get.mtch_dispatch value_exp !unboot_exp_mtchtbl (fun _ _ ->
      error "@unboot_exp")

and unboot_exps (value_exps : Value.t) : Il.exp list =
  value_exps |> Value.Get.list |> List.map unboot_exp

and unboot_exp_opt (value_exp_opt : Value.t) : Il.exp option =
  value_exp_opt |> Value.Get.opt |> Option.map unboot_exp

and unboot_expfield (value_expfield : Value.t) : Il.atom * Il.exp =
  let values = Value.Get.(value_expfield |>>! mop_expfield) in
  let atom = Value.Get.nth 0 values |> unboot_atom in
  let exp = Value.Get.nth 1 values |> unboot_exp in
  (atom, exp)

and unboot_expfields (value_expfields : Value.t) : (Il.atom * Il.exp) list =
  value_expfields |> Value.Get.list |> List.map unboot_expfield

and unboot_bool_exp (at : region) (values : Value.t list) : Il.exp =
  match values with
  | [ value_bool ] ->
      let b = Value.Get.bool value_bool in
      Il.BoolE b $$ (at, stub_exp_note)
  | _ -> error "@unboot_bool_exp"

and unboot_num_exp_nat (at : region) (values : Value.t list) : Il.exp =
  match values with
  | [ value_nat ] ->
      let n = Value.Get.num value_nat in
      Il.NumE n $$ (at, stub_exp_note)
  | _ -> error "@unboot_num_exp_nat"

and unboot_num_exp_int (at : region) (values : Value.t list) : Il.exp =
  match values with
  | [ value_int ] ->
      let n = Value.Get.num value_int in
      Il.NumE n $$ (at, stub_exp_note)
  | _ -> error "@unboot_num_exp_int"

and unboot_text_exp (at : region) (values : Value.t list) : Il.exp =
  match values with
  | [ value_text ] ->
      let s = Value.Get.text value_text in
      Il.TextE s $$ (at, stub_exp_note)
  | _ -> error "@unboot_text_exp"

and unboot_var_exp (at : region) (values : Value.t list) : Il.exp =
  match values with
  | [ value_id ] ->
      let id = unboot_id value_id in
      Il.VarE id $$ (at, stub_exp_note)
  | _ -> error "@unboot_var_exp"

and unboot_un_exp (at : region) (values : Value.t list) : Il.exp =
  let optyp_of_unop : Il.unop -> Il.optyp = function
    | `NotOp -> (`BoolT : Il.optyp)
    | `PlusOp | `MinusOp -> `NatT
  in
  match values with
  | [ value_unop; value_exp ] ->
      let unop = unboot_unop value_unop in
      let exp = unboot_exp value_exp in
      let optyp = optyp_of_unop unop in
      Il.UnE (unop, optyp, exp) $$ (at, stub_exp_note)
  | _ -> error "@unboot_un_exp"

and unboot_bin_exp (at : region) (values : Value.t list) : Il.exp =
  let optyp_of_binop : Il.binop -> Il.optyp = function
    | `AndOp | `OrOp | `ImplOp | `EquivOp -> (`BoolT : Il.optyp)
    | _ -> `NatT
  in
  match values with
  | [ value_binop; value_exp_l; value_exp_r ] ->
      let binop = unboot_binop value_binop in
      let exp_l = unboot_exp value_exp_l in
      let exp_r = unboot_exp value_exp_r in
      let optyp = optyp_of_binop binop in
      Il.BinE (binop, optyp, exp_l, exp_r) $$ (at, stub_exp_note)
  | _ -> error "@unboot_bin_exp"

and unboot_cmp_exp (at : region) (values : Value.t list) : Il.exp =
  let optyp_of_cmpop : Il.cmpop -> Il.optyp = function
    | `EqOp | `NeOp -> (`BoolT : Il.optyp)
    | _ -> `NatT
  in
  match values with
  | [ value_cmpop; value_exp_l; value_exp_r ] ->
      let cmpop = unboot_cmpop value_cmpop in
      let exp_l = unboot_exp value_exp_l in
      let exp_r = unboot_exp value_exp_r in
      let optyp = optyp_of_cmpop cmpop in
      Il.CmpE (cmpop, optyp, exp_l, exp_r) $$ (at, stub_exp_note)
  | _ -> error "@unboot_cmp_exp"

and unboot_upcast_exp (at : region) (values : Value.t list) : Il.exp =
  match values with
  | [ value_typ; value_exp ] ->
      let typ = unboot_typ value_typ in
      let exp = unboot_exp value_exp in
      Il.UpCastE (typ, exp) $$ (at, stub_exp_note)
  | _ -> error "@unboot_upcast_exp"

and unboot_downcast_exp (at : region) (values : Value.t list) : Il.exp =
  match values with
  | [ value_typ; value_exp ] ->
      let typ = unboot_typ value_typ in
      let exp = unboot_exp value_exp in
      Il.DownCastE (typ, exp) $$ (at, stub_exp_note)
  | _ -> error "@unboot_downcast_exp"

and unboot_sub_exp (at : region) (values : Value.t list) : Il.exp =
  match values with
  | [ value_exp; value_typ ] ->
      let exp = unboot_exp value_exp in
      let typ = unboot_typ value_typ in
      Il.SubE (exp, typ, Il.RecurseSC typ) $$ (at, stub_exp_note)
  | _ -> error "@unboot_sub_exp"

and unboot_match_exp (at : region) (values : Value.t list) : Il.exp =
  match values with
  | [ value_exp; value_pattern ] ->
      let exp = unboot_exp value_exp in
      let pattern = unboot_pattern value_pattern in
      Il.MatchE (exp, pattern) $$ (at, stub_exp_note)
  | _ -> error "@unboot_match_exp"

and unboot_tuple_exp (at : region) (values : Value.t list) : Il.exp =
  match values with
  | [ value_exps ] ->
      let exps = unboot_exps value_exps in
      Il.TupleE exps $$ (at, stub_exp_note)
  | _ -> error "@unboot_tuple_exp"

and unboot_expcase (value_expcase : Value.t) : Il.mixop * Il.exp list =
  let values = Value.Get.(value_expcase |>>! mop_expcase) in
  let mixop = Value.Get.nth 0 values |> unboot_mixop in
  let exps = Value.Get.nth 1 values |> unboot_exps in
  (mixop, exps)

and unboot_case_exp (at : region) (values : Value.t list) : Il.exp =
  match values with
  | [ value_expcase ] ->
      let mixop, exps = unboot_expcase value_expcase in
      let notexp = Mixfix.fill mixop exps in
      Il.CaseE notexp $$ (at, stub_exp_note)
  | _ -> error "@unboot_case_exp"

and unboot_str_exp (at : region) (values : Value.t list) : Il.exp =
  match values with
  | [ value_expfields ] ->
      let expfields = unboot_expfields value_expfields in
      Il.StrE expfields $$ (at, stub_exp_note)
  | _ -> error "@unboot_str_exp"

and unboot_opt_exp (at : region) (values : Value.t list) : Il.exp =
  match values with
  | [ value_exp_opt ] ->
      let exp_opt = unboot_exp_opt value_exp_opt in
      Il.OptE exp_opt $$ (at, stub_exp_note)
  | _ -> error "@unboot_opt_exp"

and unboot_list_exp (at : region) (values : Value.t list) : Il.exp =
  match values with
  | [ value_exps ] ->
      let exps = unboot_exps value_exps in
      Il.ListE exps $$ (at, stub_exp_note)
  | _ -> error "@unboot_list_exp"

and unboot_cons_exp (at : region) (values : Value.t list) : Il.exp =
  match values with
  | [ value_exp_h; value_exp_t ] ->
      let exp_h = unboot_exp value_exp_h in
      let exp_t = unboot_exp value_exp_t in
      Il.ConsE (exp_h, exp_t) $$ (at, stub_exp_note)
  | _ -> error "@unboot_cons_exp"

and unboot_cat_exp (at : region) (values : Value.t list) : Il.exp =
  match values with
  | [ value_exp_l; value_exp_r ] ->
      let exp_l = unboot_exp value_exp_l in
      let exp_r = unboot_exp value_exp_r in
      Il.CatE (exp_l, exp_r) $$ (at, stub_exp_note)
  | _ -> error "@unboot_cat_exp"

and unboot_mem_exp (at : region) (values : Value.t list) : Il.exp =
  match values with
  | [ value_exp_e; value_exp_s ] ->
      let exp_e = unboot_exp value_exp_e in
      let exp_s = unboot_exp value_exp_s in
      Il.MemE (exp_e, exp_s) $$ (at, stub_exp_note)
  | _ -> error "@unboot_mem_exp"

and unboot_len_exp (at : region) (values : Value.t list) : Il.exp =
  match values with
  | [ value_exp ] ->
      let exp = unboot_exp value_exp in
      Il.LenE exp $$ (at, stub_exp_note)
  | _ -> error "@unboot_len_exp"

and unboot_dot_exp (at : region) (values : Value.t list) : Il.exp =
  match values with
  | [ value_exp; value_atom ] ->
      let exp = unboot_exp value_exp in
      let atom = unboot_atom value_atom in
      Il.DotE (exp, atom) $$ (at, stub_exp_note)
  | _ -> error "@unboot_dot_exp"

and unboot_idx_exp (at : region) (values : Value.t list) : Il.exp =
  match values with
  | [ value_exp_b; value_exp_i ] ->
      let exp_b = unboot_exp value_exp_b in
      let exp_i = unboot_exp value_exp_i in
      Il.IdxE (exp_b, exp_i) $$ (at, stub_exp_note)
  | _ -> error "@unboot_idx_exp"

and unboot_slice_exp (at : region) (values : Value.t list) : Il.exp =
  match values with
  | [ value_exp_b; value_exp_i; value_exp_n ] ->
      let exp_b = unboot_exp value_exp_b in
      let exp_i = unboot_exp value_exp_i in
      let exp_n = unboot_exp value_exp_n in
      Il.SliceE (exp_b, exp_i, exp_n) $$ (at, stub_exp_note)
  | _ -> error "@unboot_slice_exp"

and unboot_upd_exp (at : region) (values : Value.t list) : Il.exp =
  match values with
  | [ value_exp_b; value_path; value_exp_n ] ->
      let exp_b = unboot_exp value_exp_b in
      let path = unboot_path value_path in
      let exp_n = unboot_exp value_exp_n in
      Il.UpdE (exp_b, path, exp_n) $$ (at, stub_exp_note)
  | _ -> error "@unboot_upd_exp"

and unboot_call_exp (at : region) (values : Value.t list) : Il.exp =
  match values with
  | [ value_id; value_targs; value_args ] ->
      let id = unboot_id value_id in
      let targs = unboot_targs value_targs in
      let args = unboot_args value_args in
      Il.CallE (id, targs, args) $$ (at, stub_exp_note)
  | _ -> error "@unboot_call_exp"

and unboot_iter_exp (at : region) (values : Value.t list) : Il.exp =
  match values with
  | [ value_exp; value_iterexp ] ->
      let exp = unboot_exp value_exp in
      let iterexp = unboot_iterexp value_iterexp in
      Il.IterE (exp, iterexp) $$ (at, stub_exp_note)
  | _ -> error "@unboot_iter_exp"

(* Paths *)

and unboot_path (value_path : Value.t) : Il.path =
  Value.Get.mtch_dispatch value_path !unboot_path_mtchtbl (fun _ _ ->
      error "@unboot_path")

and unboot_root_path (at : region) (_ : Value.t list) : Il.path =
  Il.RootP $$ (at, stub_exp_note)

and unboot_idx_path (at : region) (values : Value.t list) : Il.path =
  match values with
  | [ value_path; value_exp ] ->
      let path = unboot_path value_path in
      let exp = unboot_exp value_exp in
      Il.IdxP (path, exp) $$ (at, stub_exp_note)
  | _ -> error "@unboot_path/IDX"

and unboot_slice_path (at : region) (values : Value.t list) : Il.path =
  match values with
  | [ value_path; value_exp_i; value_exp_n ] ->
      let path = unboot_path value_path in
      let exp_i = unboot_exp value_exp_i in
      let exp_n = unboot_exp value_exp_n in
      Il.SliceP (path, exp_i, exp_n) $$ (at, stub_exp_note)
  | _ -> error "@unboot_path/SLICE"

and unboot_dot_path (at : region) (values : Value.t list) : Il.path =
  match values with
  | [ value_path; value_atom ] ->
      let path = unboot_path value_path in
      let atom = unboot_atom value_atom in
      Il.DotP (path, atom) $$ (at, stub_exp_note)
  | _ -> error "@unboot_path/DOT"

(* Patterns *)

and unboot_pattern (value_pattern : Value.t) : Il.pattern =
  Value.Get.mtch_dispatch value_pattern !unboot_pattern_mtchtbl (fun _ _ ->
      error "@unboot_pattern")

and unboot_inj_pattern (_ : region) (values : Value.t list) : Il.pattern =
  match values with
  | [ value_mixop ] ->
      let mixop = unboot_mixop value_mixop in
      Il.CaseP mixop
  | _ -> error "@unboot_pattern/INJ"

and unboot_cons_pattern (_ : region) (_ : Value.t list) : Il.pattern =
  Il.ListP `Cons

and unboot_fixed_pattern (_ : region) (values : Value.t list) : Il.pattern =
  match values with
  | [ value_nat ] ->
      let n =
        match Value.Get.num value_nat with
        | `Nat n -> n
        | `Int _ -> error "@unboot_pattern/FIXED"
      in
      let n_int =
        match Bigint.to_int n with
        | Some i -> i
        | None -> error "@unboot_pattern/FIXED"
      in
      Il.ListP (`Fixed n_int)
  | _ -> error "@unboot_pattern/FIXED"

and unboot_nil_pattern (_ : region) (_ : Value.t list) : Il.pattern =
  Il.ListP `Nil

and unboot_some_pattern (_ : region) (_ : Value.t list) : Il.pattern =
  Il.OptP `Some

and unboot_none_pattern (_ : region) (_ : Value.t list) : Il.pattern =
  Il.OptP `None

(* Iter expressions *)

and unboot_iterexp (value_iterexp : Value.t) : Il.iterexp =
  let values = Value.Get.(value_iterexp |>>! mop_iterexp) in
  let iter = Value.Get.nth 0 values |> unboot_iter in
  let varis = Value.Get.nth 1 values |> unboot_varis in
  (iter, varis)

and unboot_iterexps (value_iterexps : Value.t) : Il.iterexp list =
  value_iterexps |> Value.Get.list |> List.map unboot_iterexp

(* Initialize dispatch tables after all handler functions are defined *)

let () =
  (* Values *)
  unboot_value_mtchtbl :=
    Value.Get.build_mtchtbl
      [
        (mop_bool_value, unboot_bool_value);
        (mop_num_value_nat, unboot_num_value_nat);
        (mop_num_value_int, unboot_num_value_int);
        (mop_text_value, unboot_text_value);
        (mop_struct_value, unboot_struct_value);
        (mop_case_value, unboot_variant_value);
        (mop_tuple_value, unboot_tuple_value);
        (mop_opt_value, unboot_opt_value);
        (mop_list_value, unboot_list_value);
        (mop_func_value, unboot_func_value);
        (mop_extern_value, unboot_extern_value);
      ];
  (* Types *)
  unboot_typ_mtchtbl :=
    Value.Get.build_mtchtbl
      [
        (mop_bool_typ, unboot_bool_typ);
        (mop_num_typ_nat, unboot_num_typ_nat);
        (mop_num_typ_int, unboot_num_typ_int);
        (mop_text_typ, unboot_text_typ);
        (mop_var_typ, unboot_var_typ);
        (mop_tuple_typ, unboot_tuple_typ);
        (mop_iter_typ, unboot_iter_typ);
        (mop_func_typ, unboot_func_typ);
      ];
  (* Defined types *)
  unboot_deftyp_mtchtbl :=
    Value.Get.build_mtchtbl
      [
        (mop_plain_deftyp, unboot_plain_deftyp);
        (mop_struct_deftyp, unboot_struct_deftyp);
        (mop_variant_deftyp, unboot_variant_deftyp);
      ];
  (* Operators *)
  unboot_unop_mtchtbl :=
    Value.Get.build_mtchtbl
      [
        (mop_not_unop, unboot_not_unop);
        (mop_plus_unop, unboot_plus_unop);
        (mop_minus_unop, unboot_minus_unop);
      ];
  unboot_binop_mtchtbl :=
    Value.Get.build_mtchtbl
      [
        (mop_and_binop, unboot_and_binop);
        (mop_or_binop, unboot_or_binop);
        (mop_impl_binop, unboot_impl_binop);
        (mop_equiv_binop, unboot_equiv_binop);
        (mop_add_binop, unboot_add_binop);
        (mop_sub_binop, unboot_sub_binop);
        (mop_mul_binop, unboot_mul_binop);
        (mop_div_binop, unboot_div_binop);
        (mop_mod_binop, unboot_mod_binop);
        (mop_pow_binop, unboot_pow_binop);
      ];
  unboot_cmpop_mtchtbl :=
    Value.Get.build_mtchtbl
      [
        (mop_eq_cmpop, unboot_eq_cmpop);
        (mop_ne_cmpop, unboot_ne_cmpop);
        (mop_lt_cmpop, unboot_lt_cmpop);
        (mop_le_cmpop, unboot_le_cmpop);
        (mop_gt_cmpop, unboot_gt_cmpop);
        (mop_ge_cmpop, unboot_ge_cmpop);
      ];
  (* Arguments *)
  unboot_arg_mtchtbl :=
    Value.Get.build_mtchtbl
      [ (mop_exp_arg, unboot_exp_arg); (mop_def_arg, unboot_def_arg) ];
  (* Expressions *)
  unboot_exp_mtchtbl :=
    Value.Get.build_mtchtbl
      [
        (mop_bool_exp, unboot_bool_exp);
        (mop_num_exp_nat, unboot_num_exp_nat);
        (mop_num_exp_int, unboot_num_exp_int);
        (mop_text_exp, unboot_text_exp);
        (mop_var_exp, unboot_var_exp);
        (mop_un_exp, unboot_un_exp);
        (mop_bin_exp, unboot_bin_exp);
        (mop_cmp_exp, unboot_cmp_exp);
        (mop_upcast_exp, unboot_upcast_exp);
        (mop_downcast_exp, unboot_downcast_exp);
        (mop_sub_exp, unboot_sub_exp);
        (mop_match_exp, unboot_match_exp);
        (mop_tuple_exp, unboot_tuple_exp);
        (mop_case_exp, unboot_case_exp);
        (mop_struct_exp, unboot_str_exp);
        (mop_opt_exp, unboot_opt_exp);
        (mop_list_exp, unboot_list_exp);
        (mop_cons_exp, unboot_cons_exp);
        (mop_cat_exp, unboot_cat_exp);
        (mop_mem_exp, unboot_mem_exp);
        (mop_len_exp, unboot_len_exp);
        (mop_dot_exp, unboot_dot_exp);
        (mop_idx_exp, unboot_idx_exp);
        (mop_slice_exp, unboot_slice_exp);
        (mop_upd_exp, unboot_upd_exp);
        (mop_call_exp, unboot_call_exp);
        (mop_iter_exp, unboot_iter_exp);
      ];
  (* Paths *)
  unboot_path_mtchtbl :=
    Value.Get.build_mtchtbl
      [
        (mop_root_path, unboot_root_path);
        (mop_idx_path, unboot_idx_path);
        (mop_slice_path, unboot_slice_path);
        (mop_dot_path, unboot_dot_path);
      ];
  (* Patterns *)
  unboot_pattern_mtchtbl :=
    Value.Get.build_mtchtbl
      [
        (mop_case_pattern, unboot_inj_pattern);
        (mop_list_cons_pattern, unboot_cons_pattern);
        (mop_list_fixed_pattern, unboot_fixed_pattern);
        (mop_list_nil_pattern, unboot_nil_pattern);
        (mop_opt_some_pattern, unboot_some_pattern);
        (mop_opt_none_pattern, unboot_none_pattern);
      ]
