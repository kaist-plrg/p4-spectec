open Domain.Lib
open Lang
open Ol.Ast
open Util.Source

(* Remove dead Let instructions *)

module Defined = struct
  type t = IdSet.t

  let empty : t = IdSet.empty
  let init_exp (exp : exp) : t = Ol.Free.free_exp exp
  let init_exps (exps : exp list) : t = Ol.Free.free_exps exps

  let exclude (defined : t) (defined_exclude : t) : t =
    IdSet.diff defined defined_exclude
end

module Used = struct
  type t = IdSet.t

  let empty : t = IdSet.empty

  let init_exp (defined : Defined.t) (exp : exp) : t =
    Ol.Free.free_exp exp |> IdSet.inter defined

  let init_exps (defined : Defined.t) (exps : exp list) : t =
    Ol.Free.free_exps exps |> IdSet.inter defined

  let init_guard (defined : Defined.t) (guard : guard) : t =
    Ol.Free.free_guard guard |> IdSet.inter defined

  let union (used_a : t) (used_b : t) : t = IdSet.union used_a used_b
end

let rec removable_let (exp_r : exp) : bool =
  match exp_r.it with
  | BoolE _ | NumE _ | TextE _ | VarE _ -> true
  | UnE (_, _, exp) -> removable_let exp
  | BinE (_, _, exp_l, exp_r) | CmpE (_, _, exp_l, exp_r) ->
      removable_let exp_l && removable_let exp_r
  | UpCastE (_, exp) | DownCastE (_, exp) | SubE (exp, _) | MatchE (exp, _) ->
      removable_let exp
  | TupleE exps | CaseE (_, exps) -> List.for_all removable_let exps
  | StrE expfields -> expfields |> List.map snd |> List.for_all removable_let
  | OptE (Some exp) -> removable_let exp
  | OptE None -> true
  | ListE exps -> List.for_all removable_let exps
  | ConsE (exp_h, exp_t) -> removable_let exp_h && removable_let exp_t
  | CatE (exp_l, exp_r) -> removable_let exp_l && removable_let exp_r
  | MemE (exp_e, exp_s) -> removable_let exp_e && removable_let exp_s
  | LenE exp -> removable_let exp
  | DotE (exp_b, _) -> removable_let exp_b
  | IdxE (exp_b, exp_i) -> removable_let exp_b && removable_let exp_i
  | SliceE (exp_b, exp_i, exp_n) ->
      removable_let exp_b && removable_let exp_i && removable_let exp_n
  | UpdE (exp_b, _, exp) -> removable_let exp_b && removable_let exp
  | CallE _ -> false
  | IterE (exp, _) -> removable_let exp

let rec downstream_instr (defined : Defined.t) (instr : instr) : Used.t =
  match instr.it with
  | IfI (exp_cond, _, block_then) ->
      let used = Used.init_exp defined exp_cond in
      let used_then = downstream_block defined block_then in
      Used.union used used_then
  | HoldI (_, (_, exps), _, block_hold, block_nothold) ->
      let used = Used.init_exps defined exps in
      let used_hold = downstream_block defined block_hold in
      let used_nothold = downstream_block defined block_nothold in
      Used.union used (Used.union used_hold used_nothold)
  | CaseI (exp, cases, _) ->
      let used = Used.init_exp defined exp in
      let used_cases =
        List.fold_left
          (fun used_cases (guard, block) ->
            let used_guard = Used.init_guard defined guard in
            let used_block = downstream_block defined block in
            used_cases |> Used.union used_guard |> Used.union used_block)
          Used.empty cases
      in
      Used.union used used_cases
  | GroupI (_, _, exps, block) ->
      let used = Used.init_exps defined exps in
      let used_group = downstream_block defined block in
      Used.union used used_group
  | LetI (_, exp_r, _, block) ->
      let used = Used.init_exp defined exp_r in
      let used_block = downstream_block defined block in
      Used.union used used_block
  | RuleI (_, (_, exps), inputs, _, block) ->
      let exps_input, _ = Hints.Input.split inputs exps in
      let used = Used.init_exps defined exps_input in
      let used_block = downstream_block defined block in
      Used.union used used_block
  | ResultI (_, exps) -> Used.init_exps defined exps
  | ReturnI exp -> Used.init_exp defined exp
  | DebugI exp -> Used.init_exp defined exp

and downstream_block (defined : Defined.t) (block : block) : Used.t =
  match block with
  | [] -> Used.empty
  | ({ it = LetI (exp_l, _, _, _); _ } as instr_h) :: block_t ->
      let used_h = downstream_instr defined instr_h in
      let defined_h = Defined.init_exp exp_l in
      let defined = Defined.exclude defined defined_h in
      let used_t = downstream_block defined block_t in
      Used.union used_h used_t
  | ({ it = RuleI (_, (_, exps), inputs, _, _); _ } as instr_h) :: block_t ->
      let used_h = downstream_instr defined instr_h in
      let _, exps_output = Hints.Input.split inputs exps in
      let defined_h = Defined.init_exps exps_output in
      let defined = Defined.exclude defined defined_h in
      let used_t = downstream_block defined block_t in
      Used.union used_h used_t
  | instr_h :: block_t ->
      let used_h = downstream_instr defined instr_h in
      let used_t = downstream_block defined block_t in
      Used.union used_h used_t

let rec upstream_instr (instr : instr) : block =
  match instr.it with
  | IfI (exp_cond, iterexps, block_then) ->
      let block_then = upstream_block block_then in
      let instr = IfI (exp_cond, iterexps, block_then) $ instr.at in
      [ instr ]
  | HoldI (id, notexp, iterexps, block_hold, block_nothold) ->
      let block_hold = upstream_block block_hold in
      let block_nothold = upstream_block block_nothold in
      let instr =
        HoldI (id, notexp, iterexps, block_hold, block_nothold) $ instr.at
      in
      [ instr ]
  | CaseI (exp, cases, total) ->
      let guards, blocks = List.split cases in
      let blocks = List.map upstream_block blocks in
      let cases = List.combine guards blocks in
      let instr = CaseI (exp, cases, total) $ instr.at in
      [ instr ]
  | GroupI (id, rel_signature, exps, block) ->
      let block = upstream_block block in
      let instr = GroupI (id, rel_signature, exps, block) $ instr.at in
      [ instr ]
  | LetI (exp_l, exp_r, iterinstrs, block) when removable_let exp_r ->
      let defined = Defined.init_exp exp_l in
      let used = downstream_block defined block in
      if IdSet.is_empty used then upstream_block block
      else
        let block = upstream_block block in
        let instr = LetI (exp_l, exp_r, iterinstrs, block) $ instr.at in
        [ instr ]
  | LetI (exp_l, exp_r, iterinstrs, block) ->
      let block = upstream_block block in
      let instr = LetI (exp_l, exp_r, iterinstrs, block) $ instr.at in
      [ instr ]
  | RuleI (id, notexp, inputs, iterinstrs, block) ->
      let block = upstream_block block in
      let instr = RuleI (id, notexp, inputs, iterinstrs, block) $ instr.at in
      [ instr ]
  | _ -> [ instr ]

and upstream_block (block : block) : block =
  match block with
  | [] -> []
  | instr_h :: block_t ->
      let block_h = upstream_instr instr_h in
      let block_t = upstream_block block_t in
      block_h @ block_t

let apply (block : block) : block = upstream_block block
