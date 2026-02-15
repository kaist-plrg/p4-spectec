open Domain.Lib
open Ast
open Util.Source

(* Identifier set *)

type t = IdSet.t

let empty = IdSet.empty
let singleton = IdSet.singleton
let ( + ) = IdSet.union

(* Collect free identifiers *)

(* Expressions *)

let rec free_exp (exp : exp) : t =
  match exp.it with
  | BoolE _ | NumE _ | TextE _ -> empty
  | VarE id -> singleton id
  | UnE (_, _, exp) -> free_exp exp
  | BinE (_, _, exp_l, exp_r) -> free_exp exp_l + free_exp exp_r
  | CmpE (_, _, exp_l, exp_r) -> free_exp exp_l + free_exp exp_r
  | UpCastE (_, exp) -> free_exp exp
  | DownCastE (_, exp) -> free_exp exp
  | SubE (exp, _) -> free_exp exp
  | MatchE (exp, _) -> free_exp exp
  | TupleE exps -> free_exps exps
  | CaseE (_, exps) -> free_exps exps
  | StrE expfields -> expfields |> List.map snd |> free_exps
  | OptE (Some exp) -> free_exp exp
  | OptE None -> empty
  | ListE exps -> free_exps exps
  | ConsE (exp_h, exp_t) -> free_exp exp_h + free_exp exp_t
  | CatE (exp_l, exp_r) -> free_exp exp_l + free_exp exp_r
  | MemE (exp_e, exp_s) -> free_exp exp_e + free_exp exp_s
  | LenE exp -> free_exp exp
  | DotE (exp, _) -> free_exp exp
  | IdxE (exp_b, exp_i) -> free_exp exp_b + free_exp exp_i
  | SliceE (exp_b, exp_l, exp_h) ->
      free_exp exp_b + free_exp exp_l + free_exp exp_h
  | UpdE (exp_b, path, exp_f) ->
      free_exp exp_b + free_path path + free_exp exp_f
  | CallE (_, _, args) -> free_args args
  | IterE (exp, _) -> free_exp exp

and free_exps (exps : exp list) : t =
  exps |> List.map free_exp |> List.fold_left ( + ) empty

(* Paths *)

and free_path (path : path) : t =
  match path.it with
  | RootP -> empty
  | IdxP (path, exp) -> free_path path + free_exp exp
  | SliceP (path, exp_l, exp_h) ->
      free_path path + free_exp exp_l + free_exp exp_h
  | DotP (path, _) -> free_path path

(* Parameters *)

and free_param (param : param) : t =
  match param.it with ExpP (_, exp) -> free_exp exp | DefP _ -> empty

and free_params (params : param list) : t =
  params |> List.map free_param |> List.fold_left ( + ) empty

(* Arguments *)

and free_arg (arg : arg) : t =
  match arg.it with ExpA exp -> free_exp exp | DefA _ -> empty

and free_args (args : arg list) : t =
  args |> List.map free_arg |> List.fold_left ( + ) empty

let rec free_cases (cases : case list) : t =
  cases
  |> List.map (fun (guard, block) -> free_guard guard + free_block block)
  |> List.fold_left ( + ) empty

and free_guard (guard : guard) : t =
  match guard with
  | BoolG _ | SubG _ | MatchG _ -> empty
  | CmpG (_, _, exp) -> free_exp exp
  | MemG exp -> free_exp exp

and free_instr (instr : instr) : t =
  match instr.it with
  | IfI (exp, _, block, _) -> free_exp exp + free_block block
  | HoldI (_, (_, exps), _, _) -> free_exps exps
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
