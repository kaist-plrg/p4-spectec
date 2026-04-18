open Domain
open Lang
module Typ = Runtime.Type.Typ
module Value = Runtime.Value
open Util.Error
open Util.Source

(* Errors *)

let error = error_unparse

(* Stubs for lossy unboot conversions *)

(* typcase: typorigin (owning type def) and hint list are dropped by boot *)
let stub_typorigin () : Il.typorigin = ("" $ no_region, []) $ no_region

(* vari: the typ field of Il.var is dropped when encoding as vari *)
let stub_vari_typ () : Il.typ = Il.BoolT $ no_region

(* FuncT: tparam list, param list, and return typ are dropped by boot_func_typ *)
let stub_func_typ_params () : Il.tparam list * Il.typ list * Il.typ =
  ([], [], Il.BoolT $ no_region)

(* notexp/nottyp mixop: relation signature mixop is dropped by boot *)
let stub_notexp_mixop (arity : int) : Il.mixop =
  Mixop.Seq (List.init arity (fun _ -> Mixop.Arg))

(* input hint: boot splits inputs/outputs but doesn't store the hint explicitly.
   Recovered as [0 .. n_inputs - 1]. *)
let stub_input_hint (n_inputs : int) : Hints.Input.t = List.init n_inputs Fun.id

(* exp/path note: exp.note / path.note carries the IL type; lost in boot *)
let stub_exp_note : Il.typ' = Il.BoolT

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
  let mat =
    value_mixop |> Value.Get.list
    |> List.map (fun row -> Value.Get.list row |> List.map unboot_atom)
  in
  let pieces =
    List.concat_map
      (fun atoms -> List.map (fun a -> Mixop.Atom a) atoms @ [ Mixop.Arg ])
      mat
  in
  let pieces = List.rev (List.tl (List.rev pieces)) in
  match pieces with [ x ] -> x | _ -> Mixop.Seq pieces

(* Iterators *)

let unboot_iter (value_iter : Value.t) : Il.iter =
  Value.Get.mtch value_iter
    [ ("QUEST", fun _ -> Il.Opt); ("STAR", fun _ -> Il.List) ]
    (fun _ -> error "@unboot_iter")

let unboot_iters (value_iters : Value.t) : Il.iter list =
  value_iters |> Value.Get.list |> List.map unboot_iter

(* Variables *)

let rec unboot_vari (value_vari : Value.t) : Il.var =
  let vs = Value.Get.(value_vari |>> "id iter*") in
  let id = Value.Get.nth 0 vs |> unboot_id in
  let iters = Value.Get.nth 1 vs |> unboot_iters in
  (id, stub_vari_typ (), iters)

and unboot_varis (value_varis : Value.t) : Il.var list =
  value_varis |> Value.Get.list |> List.map unboot_vari

(* Types *)

and unboot_typ (value_typ : Value.t) : Il.typ =
  let at = value_typ.at in
  Value.Get.mtch value_typ
    [
      ("BOOL", unboot_bool_typ at);
      ("NAT", unboot_num_typ_nat at);
      ("INT", unboot_num_typ_int at);
      ("TEXT", unboot_text_typ at);
      ("VAR id targ*", unboot_var_typ at);
      ("TUP typ*", unboot_tuple_typ at);
      ("ITER typ iter", unboot_iter_typ at);
      ("FUNC", unboot_func_typ at);
    ]
    (fun _ -> error "@unboot_typ")

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
  let at = value_deftyp.at in
  Value.Get.mtch value_deftyp
    [
      ("ALIAS typ", unboot_plain_deftyp at);
      ("STRUCT typfield*", unboot_struct_deftyp at);
      ("VARIANT typcase*", unboot_variant_deftyp at);
    ]
    (fun _ -> error "@unboot_deftyp")

and unboot_plain_deftyp (at : region) (values : Value.t list) : Il.deftyp =
  match values with
  | [ value_typ ] ->
      let typ = unboot_typ value_typ in
      Il.PlainT typ $ at
  | _ -> error "@unboot_plain_deftyp"

and unboot_typfield (value_typfield : Value.t) : Il.typfield =
  let vs = Value.Get.(value_typfield |>> "atom typ") in
  let atom = Value.Get.nth 0 vs |> unboot_atom in
  let typ = Value.Get.nth 1 vs |> unboot_typ in
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
  let vs = Value.Get.(value_typcase |>> "mixop typ*") in
  let mixop = Value.Get.nth 0 vs |> unboot_mixop in
  let typs = Value.Get.nth 1 vs |> unboot_typs in
  let nottyp = (mixop, typs) $ value_typcase.at in
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
  let at = value_value.at in
  Value.Get.mtch value_value
    [
      ("BOOL bool", unboot_bool_value at);
      ("NAT nat", unboot_num_value_nat at);
      ("INT int", unboot_num_value_int at);
      ("TEXT text", unboot_text_value at);
      ("STR valfield*", unboot_struct_value at);
      ("INJ valcase", unboot_variant_value at);
      ("TUP val*", unboot_tuple_value at);
      ("OPT val?", unboot_opt_value at);
      ("LIST val*", unboot_list_value at);
      ("FUNC id", unboot_func_value at);
      ("EXT json", unboot_extern_value at);
    ]
    (fun _ -> error "@unboot_value")

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
  let vs = Value.Get.(value_valuefield |>> "atom val") in
  let atom = Value.Get.nth 0 vs |> unboot_atom in
  let v = Value.Get.nth 1 vs |> unboot_value in
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
  let vs = Value.Get.(value_valuecase |>> "mixop val*") in
  let mixop = Value.Get.nth 0 vs |> unboot_mixop in
  let values = Value.Get.nth 1 vs |> unboot_values in
  (mixop, values)

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
  Value.Get.mtch value_unop
    [
      ("NOT", fun _ : Il.unop -> `NotOp);
      ("PLUS", fun _ -> `PlusOp);
      ("MINUS", fun _ -> `MinusOp);
    ]
    (fun _ -> error "@unboot_unop")

and unboot_binop (value_binop : Value.t) : Il.binop =
  Value.Get.mtch value_binop
    [
      ("AND", fun _ : Il.binop -> `AndOp);
      ("OR", fun _ -> `OrOp);
      ("IMPL", fun _ -> `ImplOp);
      ("EQUIV", fun _ -> `EquivOp);
      ("ADD", fun _ -> `AddOp);
      ("SUB", fun _ -> `SubOp);
      ("MUL", fun _ -> `MulOp);
      ("DIV", fun _ -> `DivOp);
      ("MOD", fun _ -> `ModOp);
      ("POW", fun _ -> `PowOp);
    ]
    (fun _ -> error "@unboot_binop")

and unboot_cmpop (value_cmpop : Value.t) : Il.cmpop =
  Value.Get.mtch value_cmpop
    [
      ("EQ", fun _ : Il.cmpop -> `EqOp);
      ("NE", fun _ -> `NeOp);
      ("LT", fun _ -> `LtOp);
      ("LE", fun _ -> `LeOp);
      ("GT", fun _ -> `GtOp);
      ("GE", fun _ -> `GeOp);
    ]
    (fun _ -> error "@unboot_cmpop")

(* Parameters and arguments *)

and unboot_param (value_param : Value.t) : Il.param =
  let at = value_param.at in
  Value.Get.mtch value_param
    [
      ( "EXP typ",
        fun values ->
          match values with
          | [ value_typ ] ->
              let typ = unboot_typ value_typ in
              Il.ExpP typ $ at
          | _ -> error "@unboot_param/EXP" );
      ( "FUN id `: tparam* param* `-> typ",
        fun values ->
          match values with
          | [ value_id; value_tparams; value_params; value_typ ] ->
              let id = unboot_id value_id in
              let tparams = unboot_tparams value_tparams in
              let params = unboot_params value_params in
              let typ = unboot_typ value_typ in
              Il.DefP (id, tparams, params, typ) $ at
          | _ -> error "@unboot_param/FUN" );
    ]
    (fun _ -> error "@unboot_param")

and unboot_params (value_params : Value.t) : Il.param list =
  value_params |> Value.Get.list |> List.map unboot_param

and unboot_arg (value_arg : Value.t) : Il.arg =
  let at = value_arg.at in
  Value.Get.mtch value_arg
    [
      ( "EXP exp",
        fun values ->
          match values with
          | [ value_exp ] ->
              let exp = unboot_exp value_exp in
              Il.ExpA exp $ at
          | _ -> error "@unboot_arg" );
      ( "FUN id",
        fun values ->
          match values with
          | [ value_id ] ->
              let id = unboot_id value_id in
              Il.DefA id $ at
          | _ -> error "@unboot_arg" );
    ]
    (fun _ -> error "@unboot_arg")

and unboot_args (value_args : Value.t) : Il.arg list =
  value_args |> Value.Get.list |> List.map unboot_arg

(* Expressions *)

and unboot_exp (value_exp : Value.t) : Il.exp =
  let at = value_exp.at in
  Value.Get.mtch value_exp
    [
      ("BOOL bool", unboot_bool_exp at);
      ("NAT nat", unboot_num_exp_nat at);
      ("INT int", unboot_num_exp_int at);
      ("TEXT text", unboot_text_exp at);
      ("VAR id", unboot_var_exp at);
      ("UN unop exp", unboot_un_exp at);
      ("BIN binop exp exp", unboot_bin_exp at);
      ("CMP cmpop exp exp", unboot_cmp_exp at);
      ("UPCAST typ exp", unboot_upcast_exp at);
      ("DOWNCAST typ exp", unboot_downcast_exp at);
      ("SUB exp typ", unboot_sub_exp at);
      ("MATCH exp pattern", unboot_match_exp at);
      ("TUP exp*", unboot_tuple_exp at);
      ("INJ mixop exp*", unboot_case_exp at);
      ("STR expfield*", unboot_str_exp at);
      ("OPT exp?", unboot_opt_exp at);
      ("LIST exp*", unboot_list_exp at);
      ("CONS exp exp", unboot_cons_exp at);
      ("CAT exp exp", unboot_cat_exp at);
      ("MEM exp exp", unboot_mem_exp at);
      ("LEN exp", unboot_len_exp at);
      ("DOT exp atom", unboot_dot_exp at);
      ("IDX exp exp", unboot_idx_exp at);
      ("SLICE exp exp exp", unboot_slice_exp at);
      ("UPD exp path exp", unboot_upd_exp at);
      ("CALL id targ* arg*", unboot_call_exp at);
      ("ITER exp iterexp", unboot_iter_exp at);
    ]
    (fun _ -> error "@unboot_exp")

and unboot_exps (value_exps : Value.t) : Il.exp list =
  value_exps |> Value.Get.list |> List.map unboot_exp

and unboot_exp_opt (value_exp_opt : Value.t) : Il.exp option =
  value_exp_opt |> Value.Get.opt |> Option.map unboot_exp

and unboot_expfield (value_expfield : Value.t) : Il.atom * Il.exp =
  let vs = Value.Get.(value_expfield |>> "atom exp") in
  let atom = Value.Get.nth 0 vs |> unboot_atom in
  let exp = Value.Get.nth 1 vs |> unboot_exp in
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
      Il.SubE (exp, typ) $$ (at, stub_exp_note)
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

and unboot_case_exp (at : region) (values : Value.t list) : Il.exp =
  match values with
  | [ value_mixop; value_exps ] ->
      let mixop = unboot_mixop value_mixop in
      let exps = unboot_exps value_exps in
      Il.CaseE (mixop, exps) $$ (at, stub_exp_note)
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
  let at = value_path.at in
  Value.Get.mtch value_path
    [
      ("ROOT", fun _ -> Il.RootP $$ (at, stub_exp_note));
      ( "IDX path exp",
        fun values ->
          match values with
          | [ value_path; value_exp ] ->
              let path = unboot_path value_path in
              let exp = unboot_exp value_exp in
              Il.IdxP (path, exp) $$ (at, stub_exp_note)
          | _ -> error "@unboot_path" );
      ( "SLICE path exp exp",
        fun values ->
          match values with
          | [ value_path; value_exp_i; value_exp_n ] ->
              let path = unboot_path value_path in
              let exp_i = unboot_exp value_exp_i in
              let exp_n = unboot_exp value_exp_n in
              Il.SliceP (path, exp_i, exp_n) $$ (at, stub_exp_note)
          | _ -> error "@unboot_path" );
      ( "DOT path atom",
        fun values ->
          match values with
          | [ value_path; value_atom ] ->
              let path = unboot_path value_path in
              let atom = unboot_atom value_atom in
              Il.DotP (path, atom) $$ (at, stub_exp_note)
          | _ -> error "@unboot_path" );
    ]
    (fun _ -> error "@unboot_path")

(* Patterns *)

and unboot_pattern (value_pattern : Value.t) : Il.pattern =
  Value.Get.mtch value_pattern
    [
      ( "INJ mixop",
        fun values ->
          match values with
          | [ value_mixop ] ->
              let mixop = unboot_mixop value_mixop in
              Il.CaseP mixop
          | _ -> error "@unboot_pattern" );
      ("CONS", fun _ -> Il.ListP `Cons);
      ( "FIXED nat",
        fun values ->
          match values with
          | [ value_nat ] ->
              let n =
                match Value.Get.num value_nat with
                | `Nat n -> n
                | `Int _ -> error "@unboot_pattern"
              in
              let n_int =
                match Bigint.to_int n with
                | Some i -> i
                | None -> error "@unboot_pattern"
              in
              Il.ListP (`Fixed n_int)
          | _ -> error "@unboot_pattern" );
      ("NIL", fun _ -> Il.ListP `Nil);
      ("SOME", fun _ -> Il.OptP `Some);
      ("NONE", fun _ -> Il.OptP `None);
    ]
    (fun _ -> error "@unboot_pattern")

(* Iter expressions and premises *)

and unboot_iterexp (value_iterexp : Value.t) : Il.iterexp =
  let vs = Value.Get.(value_iterexp |>> "iter vari*") in
  let iter = Value.Get.nth 0 vs |> unboot_iter in
  let varis = Value.Get.nth 1 vs |> unboot_varis in
  (iter, varis)

and unboot_iterprem (value_iterprem : Value.t) : Il.iterprem =
  let vs = Value.Get.(value_iterprem |>> "iter vari* vari*") in
  let iter = Value.Get.nth 0 vs |> unboot_iter in
  let varis_in = Value.Get.nth 1 vs |> unboot_varis in
  let varis_out = Value.Get.nth 2 vs |> unboot_varis in
  (iter, varis_in, varis_out)

(* Premises *)

and unboot_prem (value_prem : Value.t) : Il.prem =
  let at = value_prem.at in
  Value.Get.mtch value_prem
    [
      ("REL id `: exp* `-> exp", unboot_rel_prem at);
      ("IF exp", unboot_if_prem at);
      ("IFHOLD id `: exp*", unboot_ifhold_prem at);
      ("IFNOTHOLD id `: exp*", unboot_ifnothold_prem at);
      ("LET exp `= exp", unboot_let_prem at);
      ("ITER prem iterprem", unboot_iter_prem at);
    ]
    (fun _ -> error "@unboot_prem")

and unboot_prems (value_prems : Value.t) : Il.prem list =
  value_prems |> Value.Get.list |> List.map unboot_prem

and unboot_rel_prem (at : region) (values : Value.t list) : Il.prem =
  match values with
  | [ value_id; value_input_exps; value_output_exp ] ->
      let id = unboot_id value_id in
      let input_exps = unboot_exps value_input_exps in
      let output_exp = unboot_exp value_output_exp in
      let exps = input_exps @ [ output_exp ] in
      let input = stub_input_hint (List.length input_exps) in
      let notexp : Il.notexp = (stub_notexp_mixop (List.length exps), exps) in
      Il.RulePr (id, notexp, input) $ at
  | _ -> error "@unboot_rel_prem"

and unboot_if_prem (at : region) (values : Value.t list) : Il.prem =
  match values with
  | [ value_exp ] ->
      let exp = unboot_exp value_exp in
      Il.IfPr exp $ at
  | _ -> error "@unboot_if_prem"

and unboot_ifhold_prem (at : region) (values : Value.t list) : Il.prem =
  match values with
  | [ value_id; value_exps ] ->
      let id = unboot_id value_id in
      let exps = unboot_exps value_exps in
      let notexp : Il.notexp = (stub_notexp_mixop (List.length exps), exps) in
      Il.IfHoldPr (id, notexp) $ at
  | _ -> error "@unboot_ifhold_prem"

and unboot_ifnothold_prem (at : region) (values : Value.t list) : Il.prem =
  match values with
  | [ value_id; value_exps ] ->
      let id = unboot_id value_id in
      let exps = unboot_exps value_exps in
      let notexp : Il.notexp = (stub_notexp_mixop (List.length exps), exps) in
      Il.IfNotHoldPr (id, notexp) $ at
  | _ -> error "@unboot_ifnothold_prem"

and unboot_let_prem (at : region) (values : Value.t list) : Il.prem =
  match values with
  | [ value_exp_l; value_exp_r ] ->
      let exp_l = unboot_exp value_exp_l in
      let exp_r = unboot_exp value_exp_r in
      Il.LetPr (exp_l, exp_r) $ at
  | _ -> error "@unboot_let_prem"

and unboot_iter_prem (at : region) (values : Value.t list) : Il.prem =
  match values with
  | [ value_prem; value_iterprem ] ->
      let prem = unboot_prem value_prem in
      let iterprem = unboot_iterprem value_iterprem in
      Il.IterPr (prem, iterprem) $ at
  | _ -> error "@unboot_iter_prem"

(* Rule matching and paths *)

and unboot_rulmatch (value_rulmatch : Value.t) : Il.rulematch =
  let vs = Value.Get.(value_rulmatch |>> "exp* `- prem*") in
  let exps = Value.Get.nth 0 vs |> unboot_exps in
  let prems = Value.Get.nth 1 vs |> unboot_prems in
  (exps, [], prems)

and unboot_rulpath (value_rulpath : Value.t) : Il.rulepath =
  let vs = Value.Get.(value_rulpath |>> "id `= exp* `- prem*") in
  let id = Value.Get.nth 0 vs |> unboot_id in
  let exps = Value.Get.nth 1 vs |> unboot_exps in
  let prems = Value.Get.nth 2 vs |> unboot_prems in
  (id, prems, exps)

and unboot_rulpaths (value_rulpaths : Value.t) : Il.rulepath list =
  value_rulpaths |> Value.Get.list |> List.map unboot_rulpath

and unboot_rulgroup (value_rulgroup : Value.t) : Il.rulegroup =
  let at = value_rulgroup.at in
  let vs = Value.Get.(value_rulgroup |>> "id `: rulmatch `= rulpath*") in
  let id = Value.Get.nth 0 vs |> unboot_id in
  let rulmatch = Value.Get.nth 1 vs |> unboot_rulmatch in
  let rulpaths = Value.Get.nth 2 vs |> unboot_rulpaths in
  (id, rulmatch, rulpaths) $ at

and unboot_rulgroups (value_rulgroups : Value.t) : Il.rulegroup list =
  value_rulgroups |> Value.Get.list |> List.map unboot_rulgroup

and unboot_elsgroup (value_elsgroup : Value.t) : Il.elsegroup =
  let at = value_elsgroup.at in
  let vs = Value.Get.(value_elsgroup |>> "id `: rulmatch `= rulpath") in
  let id = Value.Get.nth 0 vs |> unboot_id in
  let rulmatch = Value.Get.nth 1 vs |> unboot_rulmatch in
  let rulpath = Value.Get.nth 2 vs |> unboot_rulpath in
  (id, rulmatch, rulpath) $ at

and unboot_elsgroup_opt (value_elsgroup_opt : Value.t) : Il.elsegroup option =
  value_elsgroup_opt |> Value.Get.opt |> Option.map unboot_elsgroup

(* Clauses *)

and unboot_clause (value_clause : Value.t) : Il.clause =
  let at = value_clause.at in
  let vs = Value.Get.(value_clause |>> "arg* `= exp `- prem*") in
  let args = Value.Get.nth 0 vs |> unboot_args in
  let exp = Value.Get.nth 1 vs |> unboot_exp in
  let prems = Value.Get.nth 2 vs |> unboot_prems in
  (args, exp, prems) $ at

and unboot_clauses (value_clauses : Value.t) : Il.clause list =
  value_clauses |> Value.Get.list |> List.map unboot_clause

and unboot_elsclause (value_elsclause : Value.t) : Il.elseclause =
  unboot_clause value_elsclause

and unboot_elsclause_opt (value_elsclause_opt : Value.t) : Il.elseclause option
    =
  value_elsclause_opt |> Value.Get.opt |> Option.map unboot_elsclause

(* Table rows *)

and unboot_tablerow (value_tablerow : Value.t) : Il.tablerow =
  let at = value_tablerow.at in
  let vs = Value.Get.(value_tablerow |>> "arg* `= exp `- prem*") in
  let args = Value.Get.nth 0 vs |> unboot_args in
  let exp = Value.Get.nth 1 vs |> unboot_exp in
  let prems = Value.Get.nth 2 vs |> unboot_prems in
  ([], args, exp, prems) $ at

and unboot_tablerows (value_tablerows : Value.t) : Il.tablerow list =
  value_tablerows |> Value.Get.list |> List.map unboot_tablerow

(* Definitions *)

let rec unboot_def (value_def : Value.t) : Il.def =
  let at = value_def.at in
  Value.Get.mtch value_def
    [
      ("EXTTYP id", unboot_extern_typ_def at);
      ("TYP id tparam* `= deftyp", unboot_typ_def at);
      ("EXTREL id `: exp* `-> exp", unboot_extern_rel_def at);
      ("REL id `: typ* `-> typ* `= rulgroup* elsgroup?", unboot_rel_def at);
      ("EXTFUNC id tparam* param* `: typ", unboot_extern_func_def at);
      ("BUILTINFUNC id tparam* param* `: typ", unboot_builtin_func_def at);
      ("TABLEFUNC id param* `: typ `= tblrow*", unboot_table_func_def at);
      ("FUNC id tparam* param* `: typ `= clause* elsclause?", unboot_func_def at);
    ]
    (fun _ -> error "@unboot_def")

and unboot_extern_typ_def (at : region) (values : Value.t list) : Il.def =
  match values with
  | [ value_id ] ->
      let id = unboot_id value_id in
      Il.ExternTypD (id, []) $ at
  | _ -> error "@unboot_extern_typ_def"

and unboot_typ_def (at : region) (values : Value.t list) : Il.def =
  match values with
  | [ value_id; value_tparams; value_deftyp ] ->
      let id = unboot_id value_id in
      let tparams = unboot_tparams value_tparams in
      let deftyp = unboot_deftyp value_deftyp in
      Il.TypD (id, tparams, deftyp, []) $ at
  | _ -> error "@unboot_typ_def"

and unboot_extern_rel_def (at : region) (values : Value.t list) : Il.def =
  match values with
  | [ value_id; value_input_typs; value_output_typ ] ->
      let id = unboot_id value_id in
      let input_typs = unboot_typs value_input_typs in
      let output_typ = unboot_typ value_output_typ in
      let typs = input_typs @ [ output_typ ] in
      let input = stub_input_hint (List.length input_typs) in
      let nottyp = (stub_notexp_mixop (List.length typs), typs) $ no_region in
      Il.ExternRelD (id, nottyp, input, []) $ at
  | _ -> error "@unboot_extern_rel_def"

and unboot_rel_def (at : region) (values : Value.t list) : Il.def =
  match values with
  | [
   value_id;
   value_input_typs;
   value_output_typs;
   value_rulgroups;
   value_elsgroup;
  ] ->
      let id = unboot_id value_id in
      let input_typs = unboot_typs value_input_typs in
      let output_typs = unboot_typs value_output_typs in
      let rulgroups = unboot_rulgroups value_rulgroups in
      let elsgroup = unboot_elsgroup_opt value_elsgroup in
      let typs = input_typs @ output_typs in
      let input = stub_input_hint (List.length input_typs) in
      let nottyp = (stub_notexp_mixop (List.length typs), typs) $ no_region in
      Il.RelD (id, nottyp, input, rulgroups, elsgroup, []) $ at
  | _ -> error "@unboot_rel_def"

and unboot_extern_func_def (at : region) (values : Value.t list) : Il.def =
  match values with
  | [ value_id; value_tparams; value_params; value_typ ] ->
      let id = unboot_id value_id in
      let tparams = unboot_tparams value_tparams in
      let params = unboot_params value_params in
      let typ = unboot_typ value_typ in
      Il.ExternDecD (id, tparams, params, typ, []) $ at
  | _ -> error "@unboot_extern_func_def"

and unboot_builtin_func_def (at : region) (values : Value.t list) : Il.def =
  match values with
  | [ value_id; value_tparams; value_params; value_typ ] ->
      let id = unboot_id value_id in
      let tparams = unboot_tparams value_tparams in
      let params = unboot_params value_params in
      let typ = unboot_typ value_typ in
      Il.BuiltinDecD (id, tparams, params, typ, []) $ at
  | _ -> error "@unboot_builtin_func_def"

and unboot_table_func_def (at : region) (values : Value.t list) : Il.def =
  match values with
  | [ value_id; value_params; value_typ; value_tablerows ] ->
      let id = unboot_id value_id in
      let params = unboot_params value_params in
      let typ = unboot_typ value_typ in
      let tablerows = unboot_tablerows value_tablerows in
      Il.TableDecD (id, params, typ, tablerows, []) $ at
  | _ -> error "@unboot_table_func_def"

and unboot_func_def (at : region) (values : Value.t list) : Il.def =
  match values with
  | [
   value_id;
   value_tparams;
   value_params;
   value_typ;
   value_clauses;
   value_elsclause;
  ] ->
      let id = unboot_id value_id in
      let tparams = unboot_tparams value_tparams in
      let params = unboot_params value_params in
      let typ = unboot_typ value_typ in
      let clauses = unboot_clauses value_clauses in
      let elsclause = unboot_elsclause_opt value_elsclause in
      Il.FuncDecD (id, tparams, params, typ, clauses, elsclause, []) $ at
  | _ -> error "@unboot_func_def"

(* Specification *)

let unboot_spec (value_script : Value.t) : Il.spec =
  let value_defs = Value.Get.list value_script in
  List.map unboot_def value_defs
