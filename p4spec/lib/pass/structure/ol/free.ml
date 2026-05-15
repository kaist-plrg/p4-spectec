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

let rec free_case (case : case) : t =
  let guard, block = case in
  free_guard guard + free_block block

and free_cases (cases : case list) : t =
  cases |> List.map free_case |> List.fold_left ( + ) empty

and free_guard (guard : guard) : t =
  match guard with
  | BoolG _ -> empty
  | CmpG (_, _, exp) -> free_exp exp
  | SubG _ -> empty
  | MatchG _ -> empty
  | MemG exp -> free_exp exp

and free_instr (instr : instr) : t =
  match instr.it with
  | IfI (exp, _, block) -> free_exp exp + free_block block
  | HoldI (_, (_, exps), _, block_then, block_else) ->
      free_exps exps + free_block block_then + free_block block_else
  | CaseI (exp, cases, _) -> free_exp exp + free_cases cases
  | GroupI (_, _, exps, block) -> free_exps exps + free_block block
  | LetI (exp_l, exp_r, _, block) ->
      free_exp exp_l + free_exp exp_r + free_block block
  | RuleI (_, (_, exps), _, _, block) -> free_exps exps + free_block block
  | ResultI (_, exps) -> free_exps exps
  | ReturnI exp -> free_exp exp
  | DebugI exp -> free_exp exp

and free_instrs (instrs : instr list) : t =
  instrs |> List.map free_instr |> List.fold_left ( + ) empty

and free_block (block : block) : t = free_instrs block
