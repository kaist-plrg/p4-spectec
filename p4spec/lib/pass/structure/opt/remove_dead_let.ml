open Domain.Lib
open Lang
open Ol.Ast
open Runtime.Dynamic_Sl.Envs
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

  let union (used_a : t) (used_b : t) : t = IdSet.union used_a used_b
end

let rec downstream_instr (ihenv : IHEnv.t) (defined : Defined.t) (instr : instr)
    : Used.t =
  match instr.it with
  | IfI (exp_cond, _, instrs_then) ->
      let used = Used.init_exp defined exp_cond in
      let used_then = downstream_instrs ihenv defined instrs_then in
      Used.union used used_then
  | HoldI (_, (_, exps), _, instrs_hold, instrs_nothold) ->
      let used = Used.init_exps defined exps in
      let used_hold = downstream_instrs ihenv defined instrs_hold in
      let used_nothold = downstream_instrs ihenv defined instrs_nothold in
      Used.union used (Used.union used_hold used_nothold)
  | CaseI (exp, cases, _) ->
      let used = Used.init_exp defined exp in
      let used_cases =
        List.fold_left
          (fun acc (_, block) ->
            let used_block = downstream_instrs ihenv defined block in
            Used.union acc used_block)
          Used.empty cases
      in
      Used.union used used_cases
  | OtherwiseI instr -> downstream_instr ihenv defined instr
  | GroupI (_, _, exps, instrs_group) ->
      let used = Used.init_exps defined exps in
      let used_group = downstream_instrs ihenv defined instrs_group in
      Used.union used used_group
  | LetI (_, exp_r, _) -> Used.init_exp defined exp_r
  | RuleI (id, (_, exps), _) ->
      let exps_input, _ =
        let inputs = IHEnv.find id ihenv in
        Hints.Input.split_without_idx inputs exps
      in
      Used.init_exps defined exps_input
  | ResultI (_, exps) -> Used.init_exps defined exps
  | ReturnI exp -> Used.init_exp defined exp
  | DebugI exp -> Used.init_exp defined exp

and downstream_instrs (ihenv : IHEnv.t) (defined : Defined.t)
    (instrs : instr list) : Used.t =
  match instrs with
  | [] -> Used.empty
  | ({ it = LetI (exp_l, _, _); _ } as instr_h) :: instrs_t ->
      let used_h = downstream_instr ihenv defined instr_h in
      let defined_h = Defined.init_exp exp_l in
      let defined = Defined.exclude defined defined_h in
      let used_t = downstream_instrs ihenv defined instrs_t in
      Used.union used_h used_t
  | ({ it = RuleI (id, (_, exps), _); _ } as instr_h) :: instrs_t ->
      let used_h = downstream_instr ihenv defined instr_h in
      let _, exps_output =
        let inputs = IHEnv.find id ihenv in
        Hints.Input.split_without_idx inputs exps
      in
      let defined_h = Defined.init_exps exps_output in
      let defined = Defined.exclude defined defined_h in
      let used_t = downstream_instrs ihenv defined instrs_t in
      Used.union used_h used_t
  | instr_h :: instrs_t ->
      let used_h = downstream_instr ihenv defined instr_h in
      let used_t = downstream_instrs ihenv defined instrs_t in
      Used.union used_h used_t

let rec upstream (ihenv : IHEnv.t) (instrs : instr list) : instr list =
  match instrs with
  | [] -> []
  | { it = IfI (exp_cond, iterexps, instrs_then); at; _ } :: instrs_t ->
      let instrs_then = upstream ihenv instrs_then in
      let instr_h = IfI (exp_cond, iterexps, instrs_then) $ at in
      let instrs_t = upstream ihenv instrs_t in
      instr_h :: instrs_t
  | { it = HoldI (id, notexp, iterexps, instrs_hold, instrs_nothold); at; _ }
    :: instrs_t ->
      let instrs_hold = upstream ihenv instrs_hold in
      let instrs_nothold = upstream ihenv instrs_nothold in
      let instr_h =
        HoldI (id, notexp, iterexps, instrs_hold, instrs_nothold) $ at
      in
      let instrs_t = upstream ihenv instrs_t in
      instr_h :: instrs_t
  | { it = CaseI (exp, cases, total); at; _ } :: instrs_t ->
      let guards, blocks = List.split cases in
      let blocks = List.map (upstream ihenv) blocks in
      let cases = List.combine guards blocks in
      let instr_h = CaseI (exp, cases, total) $ at in
      let instrs_t = upstream ihenv instrs_t in
      instr_h :: instrs_t
  | { it = GroupI (id, rel_signature, exps, instrs_group); at; _ } :: instrs_t
    ->
      let instrs_group = upstream ihenv instrs_group in
      let instr_h = GroupI (id, rel_signature, exps, instrs_group) $ at in
      let instrs_t = upstream ihenv instrs_t in
      instr_h :: instrs_t
  | { it = LetI (exp_l, exp_r, iterexps); at; _ } :: instrs_t ->
      let defined = Defined.init_exp exp_l in
      let used = downstream_instrs ihenv defined instrs_t in
      if IdSet.is_empty used then upstream ihenv instrs_t
      else
        let instr_h = LetI (exp_l, exp_r, iterexps) $ at in
        let instrs_t = upstream ihenv instrs_t in
        instr_h :: instrs_t
  | instr_h :: instrs_t ->
      let instrs_t = upstream ihenv instrs_t in
      instr_h :: instrs_t

let apply (ihenv : IHEnv.t) (instrs : instr list) : instr list =
  upstream ihenv instrs
