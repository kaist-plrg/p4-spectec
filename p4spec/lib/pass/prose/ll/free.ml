open Domain.Lib
open Lang
open Sl.Free
open Ast
open Util.Source

(* Identifier set *)

type t = IdSet.t

let empty = IdSet.empty
let singleton = IdSet.singleton
let ( + ) = IdSet.union

(* Collect free identifiers *)

let free_exp (exp : exp) : t = free_exp exp
let free_exps (exps : exp list) : t = free_exps exps
let free_arg (arg : arg) : t = free_arg arg
let free_args (args : arg list) : t = free_args args

let rec free_instr (instr : instr) : t =
  match instr.it with
  | IfI (exp, _, block_then, _) -> free_exp exp + free_block block_then
  | HoldI (_, (_, exps), _, holdcase) -> free_exps exps + free_holdcase holdcase
  | CaseI (exp, cases, _) -> free_exp exp + free_cases cases
  | OtherwiseI block -> free_block block
  | GroupI (_, _, exps, block) -> free_exps exps + free_block block
  | LetI (exp_l, exp_r, _) -> free_exp exp_l + free_exp exp_r
  | RuleI (_, (_, exps), _, _) -> free_exps exps
  | ResultI (_, exps) -> free_exps exps
  | ReturnI exp -> free_exp exp

and free_block (block : block) : t =
  block |> List.map free_instr |> List.fold_left ( + ) empty

and free_holdcase (holdcase : holdcase) : t =
  match holdcase with
  | BothH (block_hold, block_nothold) ->
      free_block block_hold + free_block block_nothold
  | HoldH (block_hold, _) -> free_block block_hold
  | NotHoldH (block_nothold, _) -> free_block block_nothold

and free_case (case : case) : t =
  let guard, block = case in
  free_guard guard + free_block block

and free_cases (cases : case list) : t =
  cases |> List.map free_case |> List.fold_left ( + ) empty
