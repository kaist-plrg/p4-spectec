open Lang
module Annot = Pl.Annot
module Group = Pl.Group
module Partial = Pl.Partial
open Util.Source

(* Fallthrough stamping :

   Tag each partial instruction with where control goes when it fails,
   which render draws as the "[-> ...]" / "[FAIL]" labels:

   - FallNext:     the next alternative in the block
   - FallElse:     the relation's else block
   - FallFail:     no alternative left; fail
   - FallGroup id: another rulegroup id (for the dispatcher)

   Every partial premise of an arm shares that arm's fallthrough.
   e.g., a block with arms (a), (b) in a relation with an else block:

   - (a) Let x be $f(y).    becomes FallNext,  "[-> b]"
   - (a) Check that x = z.  becomes FallNext,  "[-> b]"
   - (b) Check that z = w.  becomes FallElse,  "[-> ⋅]"

   e.g., a dispatcher routes by constructor into disjoint arms; rulegroup (g),
   when its arm is exhausted, falls past its disjoint siblings to the next arm's
   group (g'), so its premise

   - Check that p holds.  becomes FallGroup g',  "[-> g']" *)

(* Holding conditions (group tier) *)

let rec stamp_group_holdcase (fallthrough : Pl.fallthrough)
    (holdcase : Pl.instr_group Pl.holdcase) : Pl.instr_group Pl.holdcase =
  match holdcase with
  | BothH (block_hold, block_nothold) ->
      let block_hold_stamped = stamp_block_group fallthrough block_hold in
      let block_nothold_stamped = stamp_block_group fallthrough block_nothold in
      Pl.BothH (block_hold_stamped, block_nothold_stamped)
  | HoldH (block_hold, dangle) ->
      let block_hold_stamped = stamp_block_group fallthrough block_hold in
      Pl.HoldH (block_hold_stamped, dangle)
  | NotHoldH (block_nothold, dangle) ->
      let block_nothold_stamped = stamp_block_group fallthrough block_nothold in
      Pl.NotHoldH (block_nothold_stamped, dangle)

(* Case analysis (group tier) *)

and stamp_group_cases (fallthrough : Pl.fallthrough)
    (cases : Pl.instr_group Pl.case list) : Pl.instr_group Pl.case list =
  List.map
    (fun (guard, block) ->
      let block_stamped = stamp_block_group fallthrough block in
      (guard, block_stamped))
    cases

(* Backtracking (group tier) *)

and stamp_group_arms (fallthrough : Pl.fallthrough)
    (arms : Pl.instr_group Pl.arm list) : Pl.instr_group Pl.arm list =
  let fallthroughs =
    match arms with
    | [] -> []
    | _ :: arms_next ->
        List.map (fun _ -> Pl.FallNext) arms_next @ [ fallthrough ]
  in
  List.map2 stamp_block_group fallthroughs arms

(* Instructions (group tier): result/return/rule leaves take the note *)

and stamp_instr_group (fallthrough : Pl.fallthrough)
    (instr : Pl.instr_group Pl.instr) : Pl.instr_group Pl.instr =
  let at = instr.node.at in
  let note : Pl.inote =
    if Partial.is_partial_instr Partial.is_partial_instr_group instr then
      { instr.node.note with fallthrough = Some fallthrough }
    else instr.node.note
  in
  match instr.node.it with
  | IfI (exp, iterexps, block_then, dangle) ->
      let block_then_stamped = stamp_block_group fallthrough block_then in
      let node =
        Pl.IfI (exp, iterexps, block_then_stamped, dangle) $$ (at, note)
      in
      { instr with node }
  | HoldI (id_rel, notexp, iterexps, holdcase) ->
      let holdcase_stamped = stamp_group_holdcase fallthrough holdcase in
      let node =
        Pl.HoldI (id_rel, notexp, iterexps, holdcase_stamped) $$ (at, note)
      in
      { instr with node }
  | CaseI (exp_scrut, cases, dangle) ->
      let cases_stamped = stamp_group_cases fallthrough cases in
      let node = Pl.CaseI (exp_scrut, cases_stamped, dangle) $$ (at, note) in
      { instr with node }
  | TierI (BacktrackI arms) ->
      let arms_stamped = stamp_group_arms fallthrough arms in
      let node = Pl.TierI (Pl.BacktrackI arms_stamped) $$ (at, note) in
      { instr with node }
  | CheckLetSubI (typ, subcheck, exp_l, exp_r, block_then) ->
      let block_then_stamped = stamp_block_group fallthrough block_then in
      let node =
        Pl.CheckLetSubI (typ, subcheck, exp_l, exp_r, block_then_stamped)
        $$ (at, note)
      in
      { instr with node }
  | CheckLetMatchI (pattern, exp_l, exp_r, block_then) ->
      let block_then_stamped = stamp_block_group fallthrough block_then in
      let node =
        Pl.CheckLetMatchI (pattern, exp_l, exp_r, block_then_stamped)
        $$ (at, note)
      in
      { instr with node }
  | OptionGetI (exp_l, exp_r, block_then) ->
      let block_then_stamped = stamp_block_group fallthrough block_then in
      let node =
        Pl.OptionGetI (exp_l, exp_r, block_then_stamped) $$ (at, note)
      in
      { instr with node }
  | TierI (ResultI _ | ReturnI _ | RuleI _) | LetI _ | DebugI _ | DestructI _ ->
      (* leaves and non-branching instrs: only the note is stamped *)
      let node = instr.node.it $$ (at, note) in
      { instr with node }

and stamp_block_group (fallthrough : Pl.fallthrough) (block : Pl.block_group) :
    Pl.block_group =
  List.map (stamp_instr_group fallthrough) block

(* Holding conditions (dispatch tier) *)

let rec stamp_dispatch_holdcase
    (dispatch_fallthroughs : (string * Pl.fallthrough) list)
    (holdcase : Pl.instr_dispatch Pl.holdcase) : Pl.instr_dispatch Pl.holdcase =
  match holdcase with
  | BothH (block_hold, block_nothold) ->
      let block_hold_stamped =
        stamp_block_dispatch dispatch_fallthroughs block_hold
      in
      let block_nothold_stamped =
        stamp_block_dispatch dispatch_fallthroughs block_nothold
      in
      Pl.BothH (block_hold_stamped, block_nothold_stamped)
  | HoldH (block_hold, dangle) ->
      let block_hold_stamped =
        stamp_block_dispatch dispatch_fallthroughs block_hold
      in
      Pl.HoldH (block_hold_stamped, dangle)
  | NotHoldH (block_nothold, dangle) ->
      let block_nothold_stamped =
        stamp_block_dispatch dispatch_fallthroughs block_nothold
      in
      Pl.NotHoldH (block_nothold_stamped, dangle)

(* Case analysis (dispatch tier) *)

and stamp_dispatch_cases
    (dispatch_fallthroughs : (string * Pl.fallthrough) list)
    (cases : Pl.instr_dispatch Pl.case list) : Pl.instr_dispatch Pl.case list =
  List.map
    (fun (guard, block) ->
      let block_stamped = stamp_block_dispatch dispatch_fallthroughs block in
      (guard, block_stamped))
    cases

(* Backtracking (dispatch tier) *)

and stamp_dispatch_arms (dispatch_fallthroughs : (string * Pl.fallthrough) list)
    (arms : Pl.instr_dispatch Pl.arm list) : Pl.instr_dispatch Pl.arm list =
  List.map (stamp_block_dispatch dispatch_fallthroughs) arms

(* Instructions (dispatch tier): a rule group stamps its body with the
   group's own fallthrough, using the group-tier stamper *)

and stamp_instr_dispatch
    (dispatch_fallthroughs : (string * Pl.fallthrough) list)
    (instr : Pl.instr_dispatch Pl.instr) : Pl.instr_dispatch Pl.instr =
  let at = instr.node.at in
  let note = instr.node.note in
  match instr.node.it with
  | IfI (exp, iterexps, block_then, dangle) ->
      let block_then_stamped =
        stamp_block_dispatch dispatch_fallthroughs block_then
      in
      let node =
        Pl.IfI (exp, iterexps, block_then_stamped, dangle) $$ (at, note)
      in
      { instr with node }
  | HoldI (id_rel, notexp, iterexps, holdcase) ->
      let holdcase_stamped =
        stamp_dispatch_holdcase dispatch_fallthroughs holdcase
      in
      let node =
        Pl.HoldI (id_rel, notexp, iterexps, holdcase_stamped) $$ (at, note)
      in
      { instr with node }
  | CaseI (exp_scrut, cases, dangle) ->
      let cases_stamped = stamp_dispatch_cases dispatch_fallthroughs cases in
      let node = Pl.CaseI (exp_scrut, cases_stamped, dangle) $$ (at, note) in
      { instr with node }
  | TierI (RouteI arms) ->
      let arms_stamped = stamp_dispatch_arms dispatch_fallthroughs arms in
      let node = Pl.TierI (Pl.RouteI arms_stamped) $$ (at, note) in
      { instr with node }
  | CheckLetSubI (typ, subcheck, exp_l, exp_r, block_then) ->
      let block_then_stamped =
        stamp_block_dispatch dispatch_fallthroughs block_then
      in
      let node =
        Pl.CheckLetSubI (typ, subcheck, exp_l, exp_r, block_then_stamped)
        $$ (at, note)
      in
      { instr with node }
  | CheckLetMatchI (pattern, exp_l, exp_r, block_then) ->
      let block_then_stamped =
        stamp_block_dispatch dispatch_fallthroughs block_then
      in
      let node =
        Pl.CheckLetMatchI (pattern, exp_l, exp_r, block_then_stamped)
        $$ (at, note)
      in
      { instr with node }
  | OptionGetI (exp_l, exp_r, block_then) ->
      let block_then_stamped =
        stamp_block_dispatch dispatch_fallthroughs block_then
      in
      let node =
        Pl.OptionGetI (exp_l, exp_r, block_then_stamped) $$ (at, note)
      in
      { instr with node }
  | TierI (GroupI (id_rulegroup, id_rel, rel_signature, exps, block)) ->
      let fallthrough_group =
        List.assoc id_rulegroup.it dispatch_fallthroughs
      in
      let block_stamped = stamp_block_group fallthrough_group block in
      let node =
        Pl.TierI
          (Pl.GroupI (id_rulegroup, id_rel, rel_signature, exps, block_stamped))
        $$ (at, note)
      in
      { instr with node }
  | LetI _ | DebugI _ | DestructI _ -> instr

and stamp_block_dispatch
    (dispatch_fallthroughs : (string * Pl.fallthrough) list)
    (block : Pl.block_dispatch) : Pl.block_dispatch =
  List.map (stamp_instr_dispatch dispatch_fallthroughs) block

(* Definitions *)

let collect_dispatch_fallthroughs (fallthrough : Pl.fallthrough)
    (block : Pl.block_dispatch) : (string * Pl.fallthrough) list =
  (* Collect dispatch arms *)
  let arms_dispatch =
    match block with
    | [ { node = { it = TierI (RouteI arms); _ }; _ } ] -> arms
    | _ -> [ block ]
  in
  (* Collect groups within each dispatch arm, dropping empty arms *)
  let groups_dispatch_arms =
    arms_dispatch
    |> List.map Group.collect_groups
    |> List.filter (function [] -> false | _ -> true)
  in
  (* Pair each group with its fallthrough *)
  let id_of_group (group : Group.t) = group.id_rulegroup in
  let rec make_dispatch_fallthroughs groups_dispatch_arms =
    match groups_dispatch_arms with
    | [] -> []
    (* The last group in the last arm falls through
       to the fallthrough of the dispatch *)
    | [ groups_dispatch_arm ] ->
        List.map
          (fun group ->
            let id_rulegroup = id_of_group group in
            (id_rulegroup.it, fallthrough))
          groups_dispatch_arm
    (* Groups in each arm fall through to
       the first group in the next arm *)
    | groups_dispatch_arm
      :: (groups_dispatch_arm_next :: _ as groups_dispatch_arms_rest) ->
        let id_group_next =
          groups_dispatch_arm_next |> List.hd |> id_of_group
        in
        List.map
          (fun group ->
            let id_rulegroup = id_of_group group in
            let fallthrough = Pl.FallGroup id_group_next in
            (id_rulegroup.it, fallthrough))
          groups_dispatch_arm
        @ make_dispatch_fallthroughs groups_dispatch_arms_rest
  in
  make_dispatch_fallthroughs groups_dispatch_arms

let stamp_defined_rel_def (rel : Pl.rel) : Pl.def' =
  let id_rel, rel_signature, exps, block, elseblock_opt = rel in
  let fallthrough =
    match elseblock_opt with Some (_ :: _) -> Pl.FallElse | _ -> Pl.FallFail
  in
  let dispatch_fallthroughs = collect_dispatch_fallthroughs fallthrough block in
  let block_stamped = stamp_block_dispatch dispatch_fallthroughs block in
  Pl.RelD (id_rel, rel_signature, exps, block_stamped, elseblock_opt)

let stamp_defined_func_def (definedfunc : Pl.definedfunc) : Pl.def' =
  let id, tparams, params, typ, block, elseblock_opt = definedfunc in
  let fallthrough =
    match elseblock_opt with Some (_ :: _) -> Pl.FallElse | _ -> Pl.FallFail
  in
  let block_stamped = stamp_block_group fallthrough block in
  Pl.FuncDecD (id, tparams, params, typ, block_stamped, elseblock_opt)

let stamp_def (def : Pl.def) : Pl.def =
  match def.node.it with
  | RelD rel -> { def with node = stamp_defined_rel_def rel $ def.node.at }
  | FuncDecD definedfunc ->
      { def with node = stamp_defined_func_def definedfunc $ def.node.at }
  | _ -> def

let stamp_defs (defs : Pl.def list) : Pl.def list = List.map stamp_def defs
