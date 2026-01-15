open Domain.Lib
open Lang
open Ol.Ast
open Util.Source

(* Revive underscored ids that are used *)

module Underscore = struct
  type t = IdSet.t

  let empty : t = IdSet.empty
  let init (frees : IdSet.t) : t = frees |> IdSet.filter Id.is_underscored
  let init_exp (exp : exp) : t = Ol.Free.free_exp exp |> init
  let init_exps (exps : exp list) : t = Ol.Free.free_exps exps |> init
  let init_args (args : arg list) : t = Ol.Free.free_args args |> init
  let init_guard (guard : guard) : t = Ol.Free.free_guard guard |> init

  let union (underscore_a : t) (underscore_b : t) : t =
    IdSet.union underscore_a underscore_b

  let revive (renamer_candid : Renamer.t) (underscores_used : t) : t =
    IdSet.inter (Renamer.dom renamer_candid) underscores_used

  let candid_renamer (frees : IdSet.t) (underscores_bound : t) :
      IdSet.t * Renamer.t =
    underscores_bound |> IdSet.to_list
    |> List.fold_left
         (fun (frees, renamer_candid) id_underscore ->
           let id_revive =
             Id.strip_underscore id_underscore |> Il.Fresh.fresh_id frees
           in
           let frees = IdSet.add id_revive frees in
           let renamer_candid =
             Renamer.add id_underscore id_revive renamer_candid
           in
           (frees, renamer_candid))
         (frees, Renamer.empty)

  let include_renamer (renamer : Renamer.t) (underscores_used : IdSet.t) :
      Renamer.t =
    Renamer.filter (fun id _ -> IdSet.mem id underscores_used) renamer

  let exclude_renamer (renamer : Renamer.t) (underscores_bound : IdSet.t) :
      Renamer.t =
    Renamer.filter (fun id _ -> not (IdSet.mem id underscores_bound)) renamer
end

let rec downstream_instr (renamer_candid : Renamer.t) (instr : instr) :
    Underscore.t * instr =
  let at = instr.at in
  match instr.it with
  | IfI (exp_cond, iterexps, instrs_then) ->
      let underscores_used = Underscore.init_exp exp_cond in
      let underscores_revive =
        Underscore.revive renamer_candid underscores_used
      in
      let exp_cond = Renamer.rename_exp renamer_candid exp_cond in
      let iterexps = Renamer.rename_iterexps renamer_candid iterexps in
      let underscores_revive_then, instrs_then =
        downstream_instrs renamer_candid instrs_then
      in
      let underscores_revive =
        Underscore.union underscores_revive underscores_revive_then
      in
      let instr = IfI (exp_cond, iterexps, instrs_then) $ at in
      (underscores_revive, instr)
  | HoldI (id, (mixop, exps), iterexps, instrs_hold, instrs_nothold) ->
      let underscores_used = Underscore.init_exps exps in
      let underscores_revive =
        Underscore.revive renamer_candid underscores_used
      in
      let exps = Renamer.rename_exps renamer_candid exps in
      let iterexps = Renamer.rename_iterexps renamer_candid iterexps in
      let underscores_revive_hold, instrs_hold =
        downstream_instrs renamer_candid instrs_hold
      in
      let underscores_revive_nothold, instrs_nothold =
        downstream_instrs renamer_candid instrs_nothold
      in
      let underscores_revive =
        Underscore.union underscores_revive
          (Underscore.union underscores_revive_hold underscores_revive_nothold)
      in
      let instr =
        HoldI (id, (mixop, exps), iterexps, instrs_hold, instrs_nothold) $ at
      in
      (underscores_revive, instr)
  | CaseI (exp, cases, total) ->
      let underscores_used = Underscore.init_exp exp in
      let underscores_revive =
        Underscore.revive renamer_candid underscores_used
      in
      let exp = Renamer.rename_exp renamer_candid exp in
      let underscores_revive_cases, cases =
        List.fold_left
          (fun (underscores_revive, cases) case ->
            let guard, block = case in
            let underscores_used_guard = Underscore.init_guard guard in
            let underscores_revive_guard =
              Underscore.revive renamer_candid underscores_used_guard
            in
            let guard = Renamer.rename_guard renamer_candid guard in
            let underscores_revive_block, block =
              downstream_instrs renamer_candid block
            in
            let underscores_revive =
              Underscore.union underscores_revive
                (Underscore.union underscores_revive_guard
                   underscores_revive_block)
            in
            let case = (guard, block) in
            (underscores_revive, cases @ [ case ]))
          (Underscore.empty, []) cases
      in
      let underscores_revive =
        Underscore.union underscores_revive underscores_revive_cases
      in
      let instr = CaseI (exp, cases, total) $ at in
      (underscores_revive, instr)
  | OtherwiseI instr ->
      let underscores_revive, instr = downstream_instr renamer_candid instr in
      let instr = OtherwiseI instr $ at in
      (underscores_revive, instr)
  | GroupI (id, rel_signature, exps_signature, instrs_group) ->
      let underscores_used = Underscore.init_exps exps_signature in
      let underscores_revive =
        Underscore.revive renamer_candid underscores_used
      in
      let exps_signature = Renamer.rename_exps renamer_candid exps_signature in
      let underscores_revive_group, instrs_group =
        downstream_instrs renamer_candid instrs_group
      in
      let underscores_revive =
        Underscore.union underscores_revive underscores_revive_group
      in
      let instr =
        GroupI (id, rel_signature, exps_signature, instrs_group) $ at
      in
      (underscores_revive, instr)
  | LetI (exp_l, exp_r, iterinstrs) ->
      let underscores_used = Underscore.init_exp exp_r in
      let underscores_revive =
        Underscore.revive renamer_candid underscores_used
      in
      let exp_r = Renamer.rename_exp renamer_candid exp_r in
      let iterinstrs =
        Renamer.rename_iterinstrs_bound renamer_candid iterinstrs
      in
      let instr = LetI (exp_l, exp_r, iterinstrs) $ at in
      (underscores_revive, instr)
  | RuleI (id, notexp, inputs, iterinstrs) ->
      let mixop, exps = notexp in
      let exps_input, exps_output = Hints.Input.split inputs exps in
      let underscores_used = Underscore.init_exps exps_input in
      let underscores_revive =
        Underscore.revive renamer_candid underscores_used
      in
      let exps_input = Renamer.rename_exps renamer_candid exps_input in
      let exps = Hints.Input.combine inputs exps_input exps_output in
      let notexp = (mixop, exps) in
      let iterinstrs =
        Renamer.rename_iterinstrs_bound renamer_candid iterinstrs
      in
      let instr = RuleI (id, notexp, inputs, iterinstrs) $ at in
      (underscores_revive, instr)
  | ResultI (rel_signature, exps) ->
      let underscores_used = Underscore.init_exps exps in
      let underscores_revive =
        Underscore.revive renamer_candid underscores_used
      in
      let exps = Renamer.rename_exps renamer_candid exps in
      let instr = ResultI (rel_signature, exps) $ at in
      (underscores_revive, instr)
  | ReturnI exp ->
      let underscores_used = Underscore.init_exp exp in
      let underscores_revive =
        Underscore.revive renamer_candid underscores_used
      in
      let exp = Renamer.rename_exp renamer_candid exp in
      let instr = ReturnI exp $ at in
      (underscores_revive, instr)
  | DebugI exp ->
      let underscores_used = Underscore.init_exp exp in
      let underscores_revive =
        Underscore.revive renamer_candid underscores_used
      in
      let exp = Renamer.rename_exp renamer_candid exp in
      let instr = DebugI exp $ at in
      (underscores_revive, instr)

and downstream_instrs (renamer_candid : Renamer.t) (instrs : instr list) :
    Underscore.t * instr list =
  match instrs with
  | [] -> (IdSet.empty, instrs)
  | ({ it = LetI (exp_l, _, _); _ } as instr_h) :: instrs_t ->
      let underscores_revive_h, instr_h =
        downstream_instr renamer_candid instr_h
      in
      let underscores_h = Underscore.init_exp exp_l in
      let renamer_candid =
        Underscore.exclude_renamer renamer_candid underscores_h
      in
      let underscoress_revive_t, instrs_t =
        downstream_instrs renamer_candid instrs_t
      in
      let underscores_revive =
        Underscore.union underscores_revive_h underscoress_revive_t
      in
      (underscores_revive, instr_h :: instrs_t)
  | ({ it = RuleI (_, (_, exps), inputs, _); _ } as instr_h) :: instrs_t ->
      let underscores_revive_h, instr_h =
        downstream_instr renamer_candid instr_h
      in
      let _, exps_output = Hints.Input.split inputs exps in
      let underscores_h = Underscore.init_exps exps_output in
      let renamer_candid =
        Underscore.exclude_renamer renamer_candid underscores_h
      in
      let underscores_revive_t, instrs_t =
        downstream_instrs renamer_candid instrs_t
      in
      let underscores_revive =
        Underscore.union underscores_revive_h underscores_revive_t
      in
      (underscores_revive, instr_h :: instrs_t)
  | instr_h :: instrs_t ->
      let underscores_revive_h, instr_h =
        downstream_instr renamer_candid instr_h
      in
      let underscores_revive_t, instrs_t =
        downstream_instrs renamer_candid instrs_t
      in
      let underscores_revive =
        Underscore.union underscores_revive_h underscores_revive_t
      in
      (underscores_revive, instr_h :: instrs_t)

let rec upstream (frees : IdSet.t) (instrs : instr list) : IdSet.t * instr list
    =
  match instrs with
  | [] -> (frees, [])
  | { it = IfI (exp_cond, iterexps, instrs_then); at; _ } :: instrs_t ->
      let frees, instrs_then = upstream frees instrs_then in
      let instr_h = IfI (exp_cond, iterexps, instrs_then) $ at in
      let frees, instrs_t = upstream frees instrs_t in
      (frees, instr_h :: instrs_t)
  | { it = HoldI (id, notexp, iterexps, instrs_hold, instrs_nothold); at; _ }
    :: instrs_t ->
      let frees, instrs_hold = upstream frees instrs_hold in
      let frees, instrs_nothold = upstream frees instrs_nothold in
      let instr_h =
        HoldI (id, notexp, iterexps, instrs_hold, instrs_nothold) $ at
      in
      let frees, instrs_t = upstream frees instrs_t in
      (frees, instr_h :: instrs_t)
  | { it = CaseI (exp, cases, total); at; _ } :: instrs_t ->
      let free, cases =
        List.fold_left
          (fun (free, cases) case ->
            let guard, block = case in
            let free, block = upstream free block in
            let case = (guard, block) in
            (free, cases @ [ case ]))
          (frees, []) cases
      in
      let instr_h = CaseI (exp, cases, total) $ at in
      let frees, instrs_t = upstream free instrs_t in
      (frees, instr_h :: instrs_t)
  | { it = GroupI (id, rel_signature, exps_signature, instrs_group); at; _ }
    :: instrs_t ->
      let frees, instrs = upstream frees instrs_group in
      let instr_h = GroupI (id, rel_signature, exps_signature, instrs) $ at in
      let frees, instrs_t = upstream frees instrs_t in
      (frees, instr_h :: instrs_t)
  | { it = LetI (exp_l, exp_r, iterinstrs); at; _ } :: instrs_t ->
      let underscores_bound =
        Ol.Free.free_exp exp_l |> IdSet.filter Id.is_underscored
      in
      let frees, renamer_candid =
        Underscore.candid_renamer frees underscores_bound
      in
      let underscores_revive, instrs_t =
        downstream_instrs renamer_candid instrs_t
      in
      let renamer_revive =
        Underscore.include_renamer renamer_candid underscores_revive
      in
      let exp_l = Renamer.rename_exp renamer_revive exp_l in
      let iterinstrs =
        Renamer.rename_iterinstrs_bind renamer_revive iterinstrs
      in
      let instr_h = LetI (exp_l, exp_r, iterinstrs) $ at in
      let frees, instrs_t = upstream frees instrs_t in
      (frees, instr_h :: instrs_t)
  | { it = RuleI (id, notexp, inputs, iterinstrs); at; _ } :: instrs_t ->
      let mixop, exps = notexp in
      let exps_input, exps_output = Hints.Input.split inputs exps in
      let underscores_bound = Underscore.init_exps exps_output in
      let frees, renamer_candid =
        Underscore.candid_renamer frees underscores_bound
      in
      let underscores_revive, instrs_t =
        downstream_instrs renamer_candid instrs_t
      in
      let renamer_revive =
        Underscore.include_renamer renamer_candid underscores_revive
      in
      let notexp =
        let exps_output = Renamer.rename_exps renamer_revive exps_output in
        let exps = Hints.Input.combine inputs exps_input exps_output in
        (mixop, exps)
      in
      let iterinstrs =
        Renamer.rename_iterinstrs_bind renamer_revive iterinstrs
      in
      let instr_h = RuleI (id, notexp, inputs, iterinstrs) $ at in
      let frees, instrs_t = upstream frees instrs_t in
      (frees, instr_h :: instrs_t)
  | instr_h :: instrs_t ->
      let frees, instrs_t = upstream frees instrs_t in
      (frees, instr_h :: instrs_t)

let apply_rel ((exps_match, instrs) : exp list * instr list) :
    exp list * instr list =
  let underscores_bound = Underscore.init_exps exps_match in
  let frees =
    IdSet.union (Ol.Free.free_exps exps_match) (Ol.Free.free_instrs instrs)
  in
  let frees, renamer_candid =
    Underscore.candid_renamer frees underscores_bound
  in
  let underscores_revive, instrs = downstream_instrs renamer_candid instrs in
  let renamer_revive =
    Underscore.include_renamer renamer_candid underscores_revive
  in
  let exps_match = Renamer.rename_exps renamer_revive exps_match in
  let _, instrs = upstream frees instrs in
  (exps_match, instrs)

let apply_func ((args_input, instrs) : arg list * instr list) :
    arg list * instr list =
  let underscores_bound = Underscore.init_args args_input in
  let frees =
    IdSet.union (Ol.Free.free_args args_input) (Ol.Free.free_instrs instrs)
  in
  let frees, renamer_candid =
    Underscore.candid_renamer frees underscores_bound
  in
  let underscores_revive, instrs = downstream_instrs renamer_candid instrs in
  let renamer_revive =
    Underscore.include_renamer renamer_candid underscores_revive
  in
  let args_input = Renamer.rename_args renamer_revive args_input in
  let _, instrs = upstream frees instrs in
  (args_input, instrs)
