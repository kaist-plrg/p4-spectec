open Lang
open Ol.Ast
module Typ = Runtime.Dynamic_Sl.Typ
open Runtime.Dynamic_Sl.Envs
open Overlap
open Util.Source

(* Merge consecutive if statements with the same condition

   This handles if statements that are not categorized as case analysis,
   either because the condition itself is complex or because it is iterated *)

let rec merge_identical_if (tdenv : TDEnv.t) (at : region)
    (exp_cond_target : exp) (iterexps_target : iterexp list)
    (block_then_target : block) (block : block) : block option =
  merge_identical_if' tdenv exp_cond_target iterexps_target [] block
  |> Option.map (fun (block_then, block_leftover) ->
         let instr =
           let block_then = Merge.merge_block block_then_target block_then in
           IfI (exp_cond_target, iterexps_target, block_then) $ at
         in
         instr :: block_leftover)

and merge_identical_if' (tdenv : TDEnv.t) (exp_cond_target : exp)
    (iterexps_target : iterexp list) (block_leftover : block) (block : block) :
    (block * block) option =
  match block with
  | ({ it = IfI (exp_cond, iterexps, block_then); _ } as instr_h) :: block_t
    -> (
      let eq_iterexps = Sl.Eq.eq_iterexps iterexps iterexps_target in
      let overlap_exp_cond = overlap_exp tdenv exp_cond_target exp_cond in
      match (eq_iterexps, overlap_exp_cond) with
      | true, Identical ->
          let block_leftover = block_leftover @ block_t in
          Some (block_then, block_leftover)
      | _ ->
          let block_leftover = block_leftover @ [ instr_h ] in
          merge_identical_if' tdenv exp_cond_target iterexps_target
            block_leftover block_t)
  | _ -> None

let rec merge_if (tdenv : TDEnv.t) (block : block) : block =
  match block with
  | [] -> []
  | { it = IfI (exp_cond, iterexps, block_then); at; _ } :: block_t -> (
      match
        merge_identical_if tdenv at exp_cond iterexps block_then block_t
      with
      | Some block -> merge_if tdenv block
      | None ->
          let instr_h =
            let block_then = merge_if tdenv block_then in
            IfI (exp_cond, iterexps, block_then) $ at
          in
          let block_t = merge_if tdenv block_t in
          instr_h :: block_t)
  | { it = HoldI (id, notexp, iterexps, block_hold, block_nothold); at; _ }
    :: block_t ->
      let block_hold = merge_if tdenv block_hold in
      let block_nothold = merge_if tdenv block_nothold in
      let instr_h =
        HoldI (id, notexp, iterexps, block_hold, block_nothold) $ at
      in
      let block_t = merge_if tdenv block_t in
      instr_h :: block_t
  | { it = CaseI (exp, cases, total); at; _ } :: block_t ->
      let instr_h =
        let guards, blocks = List.split cases in
        let blocks = List.map (merge_if tdenv) blocks in
        let cases = List.combine guards blocks in
        CaseI (exp, cases, total) $ at
      in
      let block_t = merge_if tdenv block_t in
      instr_h :: block_t
  | { it = GroupI (id_group, rel_signature, exps_group, block); at; _ }
    :: block_t ->
      let block = merge_if tdenv block in
      let instr_h = GroupI (id_group, rel_signature, exps_group, block) $ at in
      let block_t = merge_if tdenv block_t in
      instr_h :: block_t
  | { it = LetI (exp_l, exp_r, iterinstrs, block); at; _ } :: block_t ->
      let block = merge_if tdenv block in
      let instr_h = LetI (exp_l, exp_r, iterinstrs, block) $ at in
      let block_t = merge_if tdenv block_t in
      instr_h :: block_t
  | { it = RuleI (id, notexp, inputs, iterinstrs, block); at; _ } :: block_t ->
      let block = merge_if tdenv block in
      let instr_h = RuleI (id, notexp, inputs, iterinstrs, block) $ at in
      let block_t = merge_if tdenv block_t in
      instr_h :: block_t
  | instr_h :: block_t ->
      let block_t = merge_if tdenv block_t in
      instr_h :: block_t

let apply (tdenv : TDEnv.t) (block : block) : block = merge_if tdenv block
