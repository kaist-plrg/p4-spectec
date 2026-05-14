open Lang
module Value = Runtime.Value
open Mixops
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

(* notexp/nottyp: relation signature mixop is dropped by boot *)

let stub_nottyp (typs_input : Il.typ list) (typs_output : Il.typ list) :
    Il.nottyp =
  let s_mixop =
    (List.init (List.length typs_input) (fun _ -> "x") |> String.concat " ")
    ^ " `-> "
    ^ (List.init (List.length typs_output) (fun _ -> "y") |> String.concat " ")
  in
  let mixop = Value.Mixops.of_string s_mixop in
  (mixop, typs_input @ typs_output) $ no_region

let stub_notexp (exps_input : Il.exp list) (exps_output : Il.exp list) :
    Il.notexp =
  let s_mixop =
    (List.init (List.length exps_input) (fun _ -> "x") |> String.concat " ")
    ^ " `-> "
    ^ (List.init (List.length exps_output) (fun _ -> "y") |> String.concat " ")
  in
  let mixop = Value.Mixops.of_string s_mixop in
  (mixop, exps_input @ exps_output)

(* input hint: boot splits inputs/outputs but doesn't store the hint explicitly.
   Recovered as [0 .. n_inputs - 1]. *)

let stub_input_hint (n_inputs : int) : Hints.Input.t = List.init n_inputs Fun.id

(* exp/path note: exp.note / path.note carries the IL type; lost in boot *)

let stub_exp_note : Il.typ' = Il.BoolT
