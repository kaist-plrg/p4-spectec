open Domain.Lib
open Lang
open Il
open Runtime.Dynamic_Sl.Envs
open Util.Source

(* Revive underscored ids that are used *)

module Underscore = struct
  type t = IdSet.t

  let empty : t = IdSet.empty
  let init (frees : IdSet.t) : t = frees |> IdSet.filter Id.is_underscored
  let init_exp (exp : Ol.Ast.exp) : t = Ol.Free.free_exp exp |> init
  let init_exps (exps : Ol.Ast.exp list) : t = Ol.Free.free_exps exps |> init
  let init_args (args : Ol.Ast.arg list) : t = Ol.Free.free_args args |> init
  let init_guard (guard : Ol.Ast.guard) : t = Ol.Free.free_guard guard |> init

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

let rec revive_underscores_instr_downstream (ihenv : IHEnv.t)
    (renamer_candid : Renamer.t) (instr : Ol.Ast.instr) :
    Underscore.t * Ol.Ast.instr =
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
        revive_underscores_instrs_downstream ihenv renamer_candid instrs_then
      in
      let underscores_revive =
        Underscore.union underscores_revive underscores_revive_then
      in
      let instr = Ol.Ast.IfI (exp_cond, iterexps, instrs_then) $ at in
      (underscores_revive, instr)
  | HoldI (id, (mixop, exps), iterexps, instrs_hold, instrs_nothold) ->
      let underscores_used = Underscore.init_exps exps in
      let underscores_revive =
        Underscore.revive renamer_candid underscores_used
      in
      let exps = Renamer.rename_exps renamer_candid exps in
      let iterexps = Renamer.rename_iterexps renamer_candid iterexps in
      let underscores_revive_hold, instrs_hold =
        revive_underscores_instrs_downstream ihenv renamer_candid instrs_hold
      in
      let underscores_revive_nothold, instrs_nothold =
        revive_underscores_instrs_downstream ihenv renamer_candid instrs_nothold
      in
      let underscores_revive =
        Underscore.union underscores_revive
          (Underscore.union underscores_revive_hold underscores_revive_nothold)
      in
      let instr =
        Ol.Ast.HoldI (id, (mixop, exps), iterexps, instrs_hold, instrs_nothold)
        $ at
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
              revive_underscores_instrs_downstream ihenv renamer_candid block
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
      let instr = Ol.Ast.CaseI (exp, cases, total) $ at in
      (underscores_revive, instr)
  | OtherwiseI instr ->
      let underscores_revive, instr =
        revive_underscores_instr_downstream ihenv renamer_candid instr
      in
      let instr = Ol.Ast.OtherwiseI instr $ at in
      (underscores_revive, instr)
  | GroupI (id, rel_signature, exps_signature, instrs_group) ->
      let underscores_used = Underscore.init_exps exps_signature in
      let underscores_revive =
        Underscore.revive renamer_candid underscores_used
      in
      let exps_signature = Renamer.rename_exps renamer_candid exps_signature in
      let underscores_revive_group, instrs_group =
        revive_underscores_instrs_downstream ihenv renamer_candid instrs_group
      in
      let underscores_revive =
        Underscore.union underscores_revive underscores_revive_group
      in
      let instr =
        Ol.Ast.GroupI (id, rel_signature, exps_signature, instrs_group) $ at
      in
      (underscores_revive, instr)
  | LetI (exp_l, exp_r, iterexps) ->
      let underscores_used = Underscore.init_exp exp_r in
      let underscores_revive =
        Underscore.revive renamer_candid underscores_used
      in
      let exp_r = Renamer.rename_exp renamer_candid exp_r in
      let iterexps = Renamer.rename_iterexps renamer_candid iterexps in
      let instr = Ol.Ast.LetI (exp_l, exp_r, iterexps) $ at in
      (underscores_revive, instr)
  | RuleI (id, notexp, iterexps) ->
      let mixop, exps = notexp in
      let exps_input_indexed, exps_output_indexed =
        let inputs = IHEnv.find id ihenv in
        Hints.Input.split inputs exps
      in
      let idxs_input, exps_input = List.split exps_input_indexed in
      let underscores_used = Underscore.init_exps exps_input in
      let underscores_revive =
        Underscore.revive renamer_candid underscores_used
      in
      let exps_input = Renamer.rename_exps renamer_candid exps_input in
      let exps_input_indexed = List.combine idxs_input exps_input in
      let exps = Hints.Input.combine exps_input_indexed exps_output_indexed in
      let notexp = (mixop, exps) in
      let iterexps = Renamer.rename_iterexps renamer_candid iterexps in
      let instr = Ol.Ast.RuleI (id, notexp, iterexps) $ at in
      (underscores_revive, instr)
  | ResultI (rel_signature, exps) ->
      let underscores_used = Underscore.init_exps exps in
      let underscores_revive =
        Underscore.revive renamer_candid underscores_used
      in
      let exps = Renamer.rename_exps renamer_candid exps in
      let instr = Ol.Ast.ResultI (rel_signature, exps) $ at in
      (underscores_revive, instr)
  | ReturnI exp ->
      let underscores_used = Underscore.init_exp exp in
      let underscores_revive =
        Underscore.revive renamer_candid underscores_used
      in
      let exp = Renamer.rename_exp renamer_candid exp in
      let instr = Ol.Ast.ReturnI exp $ at in
      (underscores_revive, instr)
  | DebugI exp ->
      let underscores_used = Underscore.init_exp exp in
      let underscores_revive =
        Underscore.revive renamer_candid underscores_used
      in
      let exp = Renamer.rename_exp renamer_candid exp in
      let instr = Ol.Ast.DebugI exp $ at in
      (underscores_revive, instr)

and revive_underscores_instrs_downstream (ihenv : IHEnv.t)
    (renamer_candid : Renamer.t) (instrs : Ol.Ast.instr list) :
    Underscore.t * Ol.Ast.instr list =
  match instrs with
  | [] -> (IdSet.empty, instrs)
  | ({ it = LetI (exp_l, _, _); _ } as instr_h) :: instrs_t ->
      let underscores_revive_h, instr_h =
        revive_underscores_instr_downstream ihenv renamer_candid instr_h
      in
      let underscores_bound = Underscore.init_exp exp_l in
      let renamer_candid =
        Underscore.exclude_renamer renamer_candid underscores_bound
      in
      let underscoress_revive_t, instrs_t =
        revive_underscores_instrs_downstream ihenv renamer_candid instrs_t
      in
      let underscores_revive =
        Underscore.union underscores_revive_h underscoress_revive_t
      in
      (underscores_revive, instr_h :: instrs_t)
  | ({ it = RuleI (id, (_, exps), _); _ } as instr_h) :: instrs_t ->
      let underscores_revive_h, instr_h =
        revive_underscores_instr_downstream ihenv renamer_candid instr_h
      in
      let _, exps_output_indexed =
        let inputs = IHEnv.find id ihenv in
        Hints.Input.split inputs exps
      in
      let _, exps_output = List.split exps_output_indexed in
      let underscores_bound =
        exps_output |> Ol.Free.free_exps |> IdSet.filter Id.is_underscored
      in
      let renamer_candid =
        Underscore.exclude_renamer renamer_candid underscores_bound
      in
      let underscores_revive_t, instrs_t =
        revive_underscores_instrs_downstream ihenv renamer_candid instrs_t
      in
      let underscores_revive =
        Underscore.union underscores_revive_h underscores_revive_t
      in
      (underscores_revive, instr_h :: instrs_t)
  | instr_h :: instrs_t ->
      let underscores_revive_h, instr_h =
        revive_underscores_instr_downstream ihenv renamer_candid instr_h
      in
      let underscores_revive_t, instrs_t =
        revive_underscores_instrs_downstream ihenv renamer_candid instrs_t
      in
      let underscores_revive =
        Underscore.union underscores_revive_h underscores_revive_t
      in
      (underscores_revive, instr_h :: instrs_t)

let rec revive_underscores_instrs_upstream (ihenv : IHEnv.t) (frees : IdSet.t)
    (instrs : Ol.Ast.instr list) : IdSet.t * Ol.Ast.instr list =
  match instrs with
  | [] -> (frees, [])
  | { it = IfI (exp_cond, iterexps, instrs_then); at; _ } :: instrs_t ->
      let frees, instrs_then =
        revive_underscores_instrs_upstream ihenv frees instrs_then
      in
      let instr_h = Ol.Ast.IfI (exp_cond, iterexps, instrs_then) $ at in
      let frees, instrs_t =
        revive_underscores_instrs_upstream ihenv frees instrs_t
      in
      (frees, instr_h :: instrs_t)
  | { it = HoldI (id, notexp, iterexps, instrs_hold, instrs_nothold); at; _ }
    :: instrs_t ->
      let frees, instrs_hold =
        revive_underscores_instrs_upstream ihenv frees instrs_hold
      in
      let frees, instrs_nothold =
        revive_underscores_instrs_upstream ihenv frees instrs_nothold
      in
      let instr_h =
        Ol.Ast.HoldI (id, notexp, iterexps, instrs_hold, instrs_nothold) $ at
      in
      let frees, instrs_t =
        revive_underscores_instrs_upstream ihenv frees instrs_t
      in
      (frees, instr_h :: instrs_t)
  | { it = CaseI (exp, cases, total); at; _ } :: instrs_t ->
      let free, cases =
        List.fold_left
          (fun (free, cases) case ->
            let guard, block = case in
            let free, block =
              revive_underscores_instrs_upstream ihenv free block
            in
            let case = (guard, block) in
            (free, cases @ [ case ]))
          (frees, []) cases
      in
      let instr_h = Ol.Ast.CaseI (exp, cases, total) $ at in
      let frees, instrs_t =
        revive_underscores_instrs_upstream ihenv free instrs_t
      in
      (frees, instr_h :: instrs_t)
  | { it = GroupI (id, rel_signature, exps_signature, instrs_group); at; _ }
    :: instrs_t ->
      let frees, instrs =
        revive_underscores_instrs_upstream ihenv frees instrs_group
      in
      let instr_h =
        Ol.Ast.GroupI (id, rel_signature, exps_signature, instrs) $ at
      in
      let frees, instrs_t =
        revive_underscores_instrs_upstream ihenv frees instrs_t
      in
      (frees, instr_h :: instrs_t)
  | { it = LetI (exp_l, exp_r, iterexps); at; _ } :: instrs_t ->
      let underscores_bound =
        Ol.Free.free_exp exp_l |> IdSet.filter Id.is_underscored
      in
      let frees, renamer_candid =
        Underscore.candid_renamer frees underscores_bound
      in
      let underscores_revive, instrs_t =
        revive_underscores_instrs_downstream ihenv renamer_candid instrs_t
      in
      let renamer_revive =
        Underscore.include_renamer renamer_candid underscores_revive
      in
      let exp_l = Renamer.rename_exp renamer_revive exp_l in
      let iterexps = Renamer.rename_iterexps renamer_revive iterexps in
      let instr_h = Ol.Ast.LetI (exp_l, exp_r, iterexps) $ at in
      let frees, instrs_t =
        revive_underscores_instrs_upstream ihenv frees instrs_t
      in
      (frees, instr_h :: instrs_t)
  | { it = RuleI (id, notexp, iterexps); at; _ } :: instrs_t ->
      let mixop, exps = notexp in
      let exps_input_indexed, exps_output_indexed =
        let inputs = IHEnv.find id ihenv in
        Hints.Input.split inputs exps
      in
      let idxs_input, exps_input = List.split exps_input_indexed in
      let underscores_bound = Underscore.init_exps exps_input in
      let frees, renamer_candid =
        Underscore.candid_renamer frees underscores_bound
      in
      let underscores_revive, instrs_t =
        revive_underscores_instrs_downstream ihenv renamer_candid instrs_t
      in
      let renamer_revive =
        Underscore.include_renamer renamer_candid underscores_revive
      in
      let notexp =
        let exps_input = Renamer.rename_exps renamer_revive exps_input in
        let exps_input_indexed = List.combine idxs_input exps_input in
        let exps = Hints.Input.combine exps_input_indexed exps_output_indexed in
        (mixop, exps)
      in
      let iterexps = Renamer.rename_iterexps renamer_revive iterexps in
      let instr_h = Ol.Ast.RuleI (id, notexp, iterexps) $ at in
      let frees, instrs_t =
        revive_underscores_instrs_upstream ihenv frees instrs_t
      in
      (frees, instr_h :: instrs_t)
  | instr_h :: instrs_t ->
      let frees, instrs_t =
        revive_underscores_instrs_upstream ihenv frees instrs_t
      in
      (frees, instr_h :: instrs_t)

let revive_underscores_rel (ihenv : IHEnv.t) (exps_match : exp list)
    (instrs : Ol.Ast.instr list) : exp list * Ol.Ast.instr list =
  let underscores_bound = Underscore.init_exps exps_match in
  let frees =
    IdSet.union (Ol.Free.free_exps exps_match) (Ol.Free.free_instrs instrs)
  in
  let frees, renamer_candid =
    Underscore.candid_renamer frees underscores_bound
  in
  let underscores_revive, instrs =
    revive_underscores_instrs_downstream ihenv renamer_candid instrs
  in
  let renamer_revive =
    Underscore.include_renamer renamer_candid underscores_revive
  in
  let exps_match = Renamer.rename_exps renamer_revive exps_match in
  let _, instrs = revive_underscores_instrs_upstream ihenv frees instrs in
  (exps_match, instrs)

let revive_underscores_func (ihenv : IHEnv.t) (args_input : arg list)
    (instrs : Ol.Ast.instr list) : arg list * Ol.Ast.instr list =
  let underscores_bound = Underscore.init_args args_input in
  let frees =
    IdSet.union (Ol.Free.free_args args_input) (Ol.Free.free_instrs instrs)
  in
  let frees, renamer_candid =
    Underscore.candid_renamer frees underscores_bound
  in
  let underscores_revive, instrs =
    revive_underscores_instrs_downstream ihenv renamer_candid instrs
  in
  let renamer_revive =
    Underscore.include_renamer renamer_candid underscores_revive
  in
  let args_input = Renamer.rename_args renamer_revive args_input in
  let _, instrs = revive_underscores_instrs_upstream ihenv frees instrs in
  (args_input, instrs)

(* Rename ticks in relation input expressions
   and function input arguments, which likely were
   introduced as fresh variables during anti-unification

   def $foo(n''') ...

   will be prettified to

   def $foo(n) ... *)

let count_trailing_ticks (id : Id.t) : int =
  let rec count_trailing_ticks (n_guess : int) =
    let ticks = String.make n_guess '\'' in
    if String.ends_with ~suffix:ticks id.it then
      count_trailing_ticks (n_guess + 1)
    else n_guess - 1
  in
  count_trailing_ticks 1

let strip_trailing_ticks (id : Id.t) : Id.t =
  let n_ticks = count_trailing_ticks id in
  if n_ticks = 0 then id
  else String.sub id.it 0 (String.length id.it - n_ticks) $ id.at

let find_rename_ticks (frees : IdSet.t) (id : Id.t) : Id.t option =
  let id_strip = strip_trailing_ticks id in
  let frees = IdSet.remove id frees in
  let counts_overlap =
    frees |> IdSet.to_list
    |> List.filter_map (fun id_free ->
           if Id.eq (strip_trailing_ticks id_free) id_strip then
             Some (count_trailing_ticks id_free)
           else None)
  in
  let count_min =
    let rec find_count_min n =
      if List.mem n counts_overlap then find_count_min (n + 1) else n
    in
    find_count_min 0
  in
  let id_rename = id_strip.it ^ String.make count_min '\'' $ id.at in
  if Id.eq id id_rename then None else Some id_rename

let rename_ticks_rel (exps_match : exp list) (instrs : Ol.Ast.instr list) :
    exp list * Ol.Ast.instr list =
  let frees_match = Ol.Free.free_exps exps_match in
  let frees_instrs = Ol.Free.free_instrs instrs in
  let _, exps_match, instrs =
    frees_match |> IdSet.to_list
    |> List.fold_left
         (fun (frees_instrs, exps_match, instrs) id_match ->
           match find_rename_ticks frees_instrs id_match with
           | Some id_rename ->
               let frees_instrs = IdSet.remove id_match frees_instrs in
               let frees_instrs = IdSet.add id_rename frees_instrs in
               let renamer = Renamer.singleton id_match id_rename in
               let exps_match = Renamer.rename_exps renamer exps_match in
               let instrs = Renamer.rename_instrs renamer instrs in
               (frees_instrs, exps_match, instrs)
           | None -> (frees_instrs, exps_match, instrs))
         (frees_instrs, exps_match, instrs)
  in
  (exps_match, instrs)

let rename_ticks_func (args_input : arg list) (instrs : Ol.Ast.instr list) :
    arg list * Ol.Ast.instr list =
  let frees_match = Ol.Free.free_args args_input in
  let frees_instrs = Ol.Free.free_instrs instrs in
  let _, args_input, instrs =
    frees_match |> IdSet.to_list
    |> List.fold_left
         (fun (frees_instrs, args_input, instrs) id_match ->
           match find_rename_ticks frees_instrs id_match with
           | Some id_rename ->
               let frees_instrs = IdSet.remove id_match frees_instrs in
               let frees_instrs = IdSet.add id_rename frees_instrs in
               let renamer = Renamer.singleton id_match id_rename in
               let args_input = Renamer.rename_args renamer args_input in
               let instrs = Renamer.rename_instrs renamer instrs in
               (frees_instrs, args_input, instrs)
           | None -> (frees_instrs, args_input, instrs))
         (frees_instrs, args_input, instrs)
  in
  (args_input, instrs)

(* Prettify instructions *)

let pretty_rel (ihenv : IHEnv.t) (exps_match : exp list)
    (instrs : Ol.Ast.instr list) : exp list * Ol.Ast.instr list =
  let exps_match, instrs = revive_underscores_rel ihenv exps_match instrs in
  rename_ticks_rel exps_match instrs

let pretty_func (ihenv : IHEnv.t) (args_input : arg list)
    (instrs : Ol.Ast.instr list) : arg list * Ol.Ast.instr list =
  let args_input, instrs = revive_underscores_func ihenv args_input instrs in
  rename_ticks_func args_input instrs
