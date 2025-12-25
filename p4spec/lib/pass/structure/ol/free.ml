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

let rec free_instr (instr : instr) : t =
  match instr.it with
  | IfI (exp, _, instrs) -> free_exp exp + free_instrs instrs
  | HoldI (_, (_, exps), _, instrs_then, instrs_else) ->
      free_exps exps + free_instrs instrs_then + free_instrs instrs_else
  | CaseI (exp, cases, _) -> free_exp exp + free_cases cases
  | OtherwiseI instr -> free_instr instr
  | GroupI (_, exps, instrs) -> free_exps exps + free_instrs instrs
  | LetI (exp_l, exp_r, _) -> free_exp exp_l + free_exp exp_r
  | RuleI (_, (_, exps), _) -> free_exps exps
  | ResultI exps -> free_exps exps
  | ReturnI exp -> free_exp exp
  | DebugI exp -> free_exp exp

and free_instrs (instrs : instr list) : t =
  instrs |> List.map free_instr |> List.fold_left ( + ) empty

and free_case (case : case) : t =
  let guard, instrs = case in
  free_guard guard + free_instrs instrs

and free_cases (cases : case list) : t =
  cases |> List.map free_case |> List.fold_left ( + ) empty

and free_guard (guard : guard) : t =
  match guard with
  | BoolG _ -> empty
  | CmpG (_, _, exp) -> free_exp exp
  | SubG _ -> empty
  | MatchG _ -> empty
  | MemG exp -> free_exp exp
