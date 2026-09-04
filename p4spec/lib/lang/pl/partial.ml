open Ast
module Mixfix = Domain.Mixfix

(* A construct is partial when its evaluation can fail, i.e. it invokes a
   relation or function that may not match *)

let rec is_partial_exp (exp : exp) : bool =
  match exp.node.it with
  | BoolE _ | NumE _ | TextE _ | VarE _ -> false
  | UnE (_, _, exp) -> is_partial_exp exp
  | BinE (_, _, exp_l, exp_r) | CmpE (_, _, exp_l, exp_r) ->
      is_partial_exp exp_l || is_partial_exp exp_r
  | UpCastE (_, exp) | DownCastE (_, exp) | SubE (exp, _, _) | MatchE (exp, _)
    ->
      is_partial_exp exp
  | TupleE exps -> List.exists is_partial_exp exps
  | CaseE notexp -> notexp |> Mixfix.args |> List.exists is_partial_exp
  | StrE expfields -> expfields |> List.map snd |> List.exists is_partial_exp
  | OptE exp_opt ->
      exp_opt |> Option.map is_partial_exp |> Option.value ~default:false
  | ListE exps -> List.exists is_partial_exp exps
  | ConsE (exp_h, exp_t) -> is_partial_exp exp_h || is_partial_exp exp_t
  | CatE (exp_l, exp_r) -> is_partial_exp exp_l || is_partial_exp exp_r
  | MemE (exp_e, exp_s) -> is_partial_exp exp_e || is_partial_exp exp_s
  | LenE exp -> is_partial_exp exp
  | DotE (exp, _) -> is_partial_exp exp
  | IdxE (exp_b, exp_i) -> is_partial_exp exp_b || is_partial_exp exp_i
  | SliceE (exp_b, exp_l, exp_n) ->
      is_partial_exp exp_b || is_partial_exp exp_l || is_partial_exp exp_n
  | UpdE (exp_b, path, exp_f) ->
      is_partial_exp exp_b || is_partial_path path || is_partial_exp exp_f
  | CallE _ -> true
  | IterE (exp, _) -> is_partial_exp exp

and is_partial_path (path : path) : bool =
  match path.it with
  | RootP -> false
  | IdxP (path_b, exp_i) -> is_partial_path path_b || is_partial_exp exp_i
  | SliceP (path_b, exp_l, exp_n) ->
      is_partial_path path_b || is_partial_exp exp_l || is_partial_exp exp_n
  | DotP (path_b, _) -> is_partial_path path_b

let rec is_partial_case (case : _ case) : bool =
  let guard, _ = case in
  is_partial_guard guard

and is_partial_guard (guard : guard) : bool =
  match guard with
  | BoolG _ -> false
  | CmpG (_, _, exp) -> is_partial_exp exp
  | SubG _ | MatchG _ | MemG _ -> false
  | CheckLetSubG (_, _, exp) | CheckLetMatchG (_, exp) -> is_partial_exp exp

let is_partial_instr_group (instr_group : instr_group) : bool =
  match instr_group with
  | RuleI (_, notexp, _, _) ->
      notexp |> Mixfix.args |> List.exists is_partial_exp
  | ResultI (_, exps) -> List.exists is_partial_exp exps
  | ReturnI exp -> is_partial_exp exp
  | BacktrackI _ -> false

let is_partial_instr_dispatch (instr_dispatch : instr_dispatch) : bool =
  match instr_dispatch with GroupI _ | RouteI _ -> false

let is_partial_instr (is_partial_instr_tier : 'instr_tier -> bool)
    (instr : 'instr_tier instr) : bool =
  match instr.node.it with
  | IfI (exp_cond, _, _, _) -> is_partial_exp exp_cond
  | HoldI _ -> true
  | CaseI (exp, cases, _) ->
      is_partial_exp exp || List.exists is_partial_case cases
  | LetI (_, exp_r, _) -> is_partial_exp exp_r
  | DebugI exp -> is_partial_exp exp
  | DestructI (_, exp) -> is_partial_exp exp
  | CheckLetSubI _ | CheckLetMatchI _ | OptionGetI _ -> true
  | TierI instr_tier -> is_partial_instr_tier instr_tier
