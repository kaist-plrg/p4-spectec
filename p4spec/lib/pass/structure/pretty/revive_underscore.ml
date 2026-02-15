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

  let revive (renamer_candid : Re.Renamer.t) (underscores_used : t) : t =
    IdSet.inter (Re.Renamer.dom renamer_candid) underscores_used

  let candid_renamer (frees : IdSet.t) (underscores_bound : t) :
      IdSet.t * Re.Renamer.t =
    underscores_bound |> IdSet.to_list
    |> List.fold_left
         (fun (frees, renamer_candid) id_underscore ->
           let id_revive =
             Id.strip_underscore id_underscore |> Il.Fresh.fresh_id frees
           in
           let frees = IdSet.add id_revive frees in
           let renamer_candid =
             Re.Renamer.add id_underscore id_revive renamer_candid
           in
           (frees, renamer_candid))
         (frees, Re.Renamer.empty)

  let include_renamer (renamer : Re.Renamer.t) (underscores_used : IdSet.t) :
      Re.Renamer.t =
    Re.Renamer.filter (fun id _ -> IdSet.mem id underscores_used) renamer

  let exclude_renamer (renamer : Re.Renamer.t) (underscores_bound : IdSet.t) :
      Re.Renamer.t =
    Re.Renamer.filter (fun id _ -> not (IdSet.mem id underscores_bound)) renamer
end

let rec downstream_instr (renamer_candid : Re.Renamer.t) (instr : instr) :
    Underscore.t * instr =
  let at = instr.at in
  match instr.it with
  | IfI (exp_cond, iterexps, block_then) ->
      let underscores_used = Underscore.init_exp exp_cond in
      let underscores_revive =
        Underscore.revive renamer_candid underscores_used
      in
      let exp_cond = Re.Renamer.rename_exp renamer_candid exp_cond in
      let iterexps = Re.Renamer.rename_iterexps renamer_candid iterexps in
      let underscores_revive_then, block_then =
        downstream_block renamer_candid block_then
      in
      let underscores_revive =
        Underscore.union underscores_revive underscores_revive_then
      in
      let instr = IfI (exp_cond, iterexps, block_then) $ at in
      (underscores_revive, instr)
  | HoldI (id, (mixop, exps), iterexps, block_hold, block_nothold) ->
      let underscores_used = Underscore.init_exps exps in
      let underscores_revive =
        Underscore.revive renamer_candid underscores_used
      in
      let exps = Re.Renamer.rename_exps renamer_candid exps in
      let iterexps = Re.Renamer.rename_iterexps renamer_candid iterexps in
      let underscores_revive_hold, block_hold =
        downstream_block renamer_candid block_hold
      in
      let underscores_revive_nothold, block_nothold =
        downstream_block renamer_candid block_nothold
      in
      let underscores_revive =
        Underscore.union underscores_revive
          (Underscore.union underscores_revive_hold underscores_revive_nothold)
      in
      let instr =
        HoldI (id, (mixop, exps), iterexps, block_hold, block_nothold) $ at
      in
      (underscores_revive, instr)
  | CaseI (exp, cases, total) ->
      let underscores_used = Underscore.init_exp exp in
      let underscores_revive =
        Underscore.revive renamer_candid underscores_used
      in
      let exp = Re.Renamer.rename_exp renamer_candid exp in
      let underscores_revive_cases, cases =
        List.fold_left
          (fun (underscores_revive, cases) case ->
            let guard, block = case in
            let underscores_used_guard = Underscore.init_guard guard in
            let underscores_revive_guard =
              Underscore.revive renamer_candid underscores_used_guard
            in
            let guard = Re.Renamer.rename_guard renamer_candid guard in
            let underscores_revive_block, block =
              downstream_block renamer_candid block
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
  | GroupI (id, rel_signature, exps_signature, block) ->
      let underscores_used = Underscore.init_exps exps_signature in
      let underscores_revive =
        Underscore.revive renamer_candid underscores_used
      in
      let exps_signature =
        Re.Renamer.rename_exps renamer_candid exps_signature
      in
      let underscores_revive_group, block =
        downstream_block renamer_candid block
      in
      let underscores_revive =
        Underscore.union underscores_revive underscores_revive_group
      in
      let instr = GroupI (id, rel_signature, exps_signature, block) $ at in
      (underscores_revive, instr)
  | LetI (exp_l, exp_r, iterinstrs, block) ->
      let underscores_used = Underscore.init_exp exp_r in
      let underscores_revive =
        Underscore.revive renamer_candid underscores_used
      in
      let exp_r = Re.Renamer.rename_exp renamer_candid exp_r in
      let iterinstrs =
        Re.Renamer.rename_iterinstrs_bound renamer_candid iterinstrs
      in
      let underscores_revive_block, block =
        let underscores_l = Underscore.init_exp exp_l in
        let renamer_candid =
          Underscore.exclude_renamer renamer_candid underscores_l
        in
        downstream_block renamer_candid block
      in
      let underscores_revive =
        Underscore.union underscores_revive underscores_revive_block
      in
      let instr = LetI (exp_l, exp_r, iterinstrs, block) $ at in
      (underscores_revive, instr)
  | RuleI (id, notexp, inputs, iterinstrs, block) ->
      let mixop, exps = notexp in
      let exps_input, exps_output = Hints.Input.split inputs exps in
      let underscores_used = Underscore.init_exps exps_input in
      let underscores_revive =
        Underscore.revive renamer_candid underscores_used
      in
      let exps_input = Re.Renamer.rename_exps renamer_candid exps_input in
      let exps = Hints.Input.combine inputs exps_input exps_output in
      let notexp = (mixop, exps) in
      let iterinstrs =
        Re.Renamer.rename_iterinstrs_bound renamer_candid iterinstrs
      in
      let underscores_revive_block, block =
        let underscores_output = Underscore.init_exps exps_output in
        let renamer_candid =
          Underscore.exclude_renamer renamer_candid underscores_output
        in
        downstream_block renamer_candid block
      in
      let underscores_revive =
        Underscore.union underscores_revive underscores_revive_block
      in
      let instr = RuleI (id, notexp, inputs, iterinstrs, block) $ at in
      (underscores_revive, instr)
  | ResultI (rel_signature, exps) ->
      let underscores_used = Underscore.init_exps exps in
      let underscores_revive =
        Underscore.revive renamer_candid underscores_used
      in
      let exps = Re.Renamer.rename_exps renamer_candid exps in
      let instr = ResultI (rel_signature, exps) $ at in
      (underscores_revive, instr)
  | ReturnI exp ->
      let underscores_used = Underscore.init_exp exp in
      let underscores_revive =
        Underscore.revive renamer_candid underscores_used
      in
      let exp = Re.Renamer.rename_exp renamer_candid exp in
      let instr = ReturnI exp $ at in
      (underscores_revive, instr)
  | DebugI exp ->
      let underscores_used = Underscore.init_exp exp in
      let underscores_revive =
        Underscore.revive renamer_candid underscores_used
      in
      let exp = Re.Renamer.rename_exp renamer_candid exp in
      let instr = DebugI exp $ at in
      (underscores_revive, instr)

and downstream_block (renamer_candid : Re.Renamer.t) (block : block) :
    Underscore.t * block =
  match block with
  | [] -> (IdSet.empty, block)
  | instr_h :: block_t ->
      let underscores_revive_h, instr_h =
        downstream_instr renamer_candid instr_h
      in
      let underscores_revive_t, block_t =
        downstream_block renamer_candid block_t
      in
      let underscores_revive =
        Underscore.union underscores_revive_h underscores_revive_t
      in
      (underscores_revive, instr_h :: block_t)

let rec upstream_instr (frees : IdSet.t) (instr : instr) : instr =
  match instr.it with
  | IfI (exp_cond, iterexps, block_then) ->
      let block_then = upstream_block frees block_then in
      IfI (exp_cond, iterexps, block_then) $ instr.at
  | HoldI (id, notexp, iterexps, block_hold, block_nothold) ->
      let block_hold = upstream_block frees block_hold in
      let block_nothold = upstream_block frees block_nothold in
      HoldI (id, notexp, iterexps, block_hold, block_nothold) $ instr.at
  | CaseI (exp, cases, total) ->
      let guards, blocks = List.split cases in
      let blocks = List.map (upstream_block frees) blocks in
      let cases = List.combine guards blocks in
      CaseI (exp, cases, total) $ instr.at
  | GroupI (id, rel_signature, exps_signature, block) ->
      let block = upstream_block frees block in
      GroupI (id, rel_signature, exps_signature, block) $ instr.at
  | LetI (exp_l, exp_r, iterinstrs, block) ->
      let underscores_bound =
        Underscore.init_exp exp_l |> IdSet.filter Id.is_underscored
      in
      let _, renamer_candid =
        Underscore.candid_renamer frees underscores_bound
      in
      let underscores_revive, block = downstream_block renamer_candid block in
      let renamer_revive =
        Underscore.include_renamer renamer_candid underscores_revive
      in
      let exp_l = Re.Renamer.rename_exp renamer_revive exp_l in
      let iterinstrs =
        Re.Renamer.rename_iterinstrs_bind renamer_revive iterinstrs
      in
      let block = Re.Renamer.rename_block renamer_revive block in
      LetI (exp_l, exp_r, iterinstrs, block) $ instr.at
  | RuleI (id, notexp, inputs, iterinstrs, block) ->
      let mixop, exps = notexp in
      let exps_input, exps_output = Hints.Input.split inputs exps in
      let underscores_bound =
        Underscore.init_exps exps_output |> IdSet.filter Id.is_underscored
      in
      let _, renamer_candid =
        Underscore.candid_renamer frees underscores_bound
      in
      let underscores_revive, block = downstream_block renamer_candid block in
      let renamer_revive =
        Underscore.include_renamer renamer_candid underscores_revive
      in
      let notexp =
        let exps_output = Re.Renamer.rename_exps renamer_revive exps_output in
        let exps = Hints.Input.combine inputs exps_input exps_output in
        (mixop, exps)
      in
      let iterinstrs =
        Re.Renamer.rename_iterinstrs_bind renamer_revive iterinstrs
      in
      RuleI (id, notexp, inputs, iterinstrs, block) $ instr.at
  | _ -> instr

and upstream_block (frees : IdSet.t) (block : block) : block =
  match block with
  | [] -> []
  | instr_h :: block_t ->
      let instr_h = upstream_instr frees instr_h in
      let block_t = upstream_block frees block_t in
      instr_h :: block_t

let apply_rel
    ((exps_match, block, elseblock_opt) : exp list * block * elseblock option) :
    exp list * block * elseblock option =
  let underscores_bound = Underscore.init_exps exps_match in
  let frees =
    let frees_exps_match = Ol.Free.free_exps exps_match in
    let frees_block = Ol.Free.free_block block in
    let frees_elseblock_opt =
      elseblock_opt
      |> Option.map Ol.Free.free_block
      |> Option.value ~default:IdSet.empty
    in
    frees_exps_match |> IdSet.union frees_block
    |> IdSet.union frees_elseblock_opt
  in
  let frees, renamer_candid =
    Underscore.candid_renamer frees underscores_bound
  in
  let underscores_revive, block = downstream_block renamer_candid block in
  let underscores_revive, elseblock_opt =
    match elseblock_opt with
    | Some elseblock ->
        let underscores_revive_else, elseblock =
          downstream_block renamer_candid elseblock
        in
        let underscores_revive =
          Underscore.union underscores_revive underscores_revive_else
        in
        (underscores_revive, Some elseblock)
    | None -> (underscores_revive, None)
  in
  let renamer_revive =
    Underscore.include_renamer renamer_candid underscores_revive
  in
  let exps_match = Re.Renamer.rename_exps renamer_revive exps_match in
  let block = upstream_block frees block in
  let elseblock_opt = Option.map (upstream_block frees) elseblock_opt in
  (exps_match, block, elseblock_opt)

let apply_func
    ((args_input, block, elseblock_opt) : arg list * block * elseblock option) :
    arg list * block * elseblock option =
  let underscores_bound = Underscore.init_args args_input in
  let frees =
    let frees_args_input = Ol.Free.free_args args_input in
    let frees_block = Ol.Free.free_block block in
    let frees_elseblock_opt =
      elseblock_opt
      |> Option.map Ol.Free.free_block
      |> Option.value ~default:IdSet.empty
    in
    frees_args_input |> IdSet.union frees_block
    |> IdSet.union frees_elseblock_opt
  in
  let frees, renamer_candid =
    Underscore.candid_renamer frees underscores_bound
  in
  let underscores_revive, block = downstream_block renamer_candid block in
  let underscores_revive, elseblock_opt =
    match elseblock_opt with
    | Some elseblock ->
        let underscores_revive_else, elseblock =
          downstream_block renamer_candid elseblock
        in
        let underscores_revive =
          Underscore.union underscores_revive underscores_revive_else
        in
        (underscores_revive, Some elseblock)
    | None -> (underscores_revive, None)
  in
  let renamer_revive =
    Underscore.include_renamer renamer_candid underscores_revive
  in
  let args_input = Re.Renamer.rename_args renamer_revive args_input in
  let block = upstream_block frees block in
  let elseblock_opt = Option.map (upstream_block frees) elseblock_opt in
  (args_input, block, elseblock_opt)
