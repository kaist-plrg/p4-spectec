open Domain.Lib
open Lang
module Var = Runtime.Dynamic.Var
open Util.Source

(* Helpers *)

let rec is_iter_var_exp (exp : Sl.exp) : Var.t option =
  match exp.it with
  | VarE id_exp -> Some (id_exp, [])
  | IterE (exp_inner, iterexp) -> (
      match is_iter_var_exp exp_inner with
      | Some (id_var, iters_var) -> (
          match iterexp with
          | iter, [ var ] ->
              let id_iter, _, iters_iter = var in
              if Id.eq id_var id_iter && iters_var = iters_iter then
                Some (id_var, iters_var @ [ iter ])
              else None
          | _ -> None)
      | None -> None)
  | _ -> None

(* Raise an Unmatch exception *)

let raise_unmatch (msg : string) : Ml.expr =
  Ml.AppE (Ml.VarE "raise", [ Ml.AppE (Ml.VarE "Unmatch", [ Ml.StrE msg ]) ])
