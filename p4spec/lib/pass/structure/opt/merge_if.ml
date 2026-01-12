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
    (instrs_then_target : instr list) (instrs : instr list) : instr list option
    =
  merge_identical_if' tdenv exp_cond_target iterexps_target [] instrs
  |> Option.map (fun (instrs_then, instrs_leftover) ->
         let instr =
           let instrs_then = Merge.merge_block instrs_then_target instrs_then in
           IfI (exp_cond_target, iterexps_target, instrs_then) $ at
         in
         instr :: instrs_leftover)

and merge_identical_if' (tdenv : TDEnv.t) (exp_cond_target : exp)
    (iterexps_target : iterexp list) (instrs_leftover : instr list)
    (instrs : instr list) : (instr list * instr list) option =
  match instrs with
  | ({ it = IfI (exp_cond, iterexps, instrs_then); _ } as instr_h) :: instrs_t
    -> (
      let eq_iterexps = Sl.Eq.eq_iterexps iterexps iterexps_target in
      let overlap_exp_cond = overlap_exp tdenv exp_cond_target exp_cond in
      match (eq_iterexps, overlap_exp_cond) with
      | true, Identical ->
          let instrs_leftover = instrs_leftover @ instrs_t in
          Some (instrs_then, instrs_leftover)
      | _ ->
          let instrs_leftover = instrs_leftover @ [ instr_h ] in
          merge_identical_if' tdenv exp_cond_target iterexps_target
            instrs_leftover instrs_t)
  | _ -> None

let rec merge_if (tdenv : TDEnv.t) (instrs : instr list) : instr list =
  match instrs with
  | [] -> []
  | { it = IfI (exp_cond, iterexps, instrs_then); at; _ } :: instrs_t -> (
      match
        merge_identical_if tdenv at exp_cond iterexps instrs_then instrs_t
      with
      | Some instrs -> merge_if tdenv instrs
      | None ->
          let instr_h =
            let instrs_then = merge_if tdenv instrs_then in
            IfI (exp_cond, iterexps, instrs_then) $ at
          in
          let instrs_t = merge_if tdenv instrs_t in
          instr_h :: instrs_t)
  | { it = HoldI (id, notexp, iterexps, instrs_hold, instrs_nothold); at; _ }
    :: instrs_t ->
      let instrs_hold = merge_if tdenv instrs_hold in
      let instrs_nothold = merge_if tdenv instrs_nothold in
      let instr_h =
        HoldI (id, notexp, iterexps, instrs_hold, instrs_nothold) $ at
      in
      let instrs_t = merge_if tdenv instrs_t in
      instr_h :: instrs_t
  | { it = CaseI (exp, cases, total); at; _ } :: instrs_t ->
      let instr_h =
        let guards, blocks = List.split cases in
        let blocks = List.map (merge_if tdenv) blocks in
        let cases = List.combine guards blocks in
        CaseI (exp, cases, total) $ at
      in
      let instrs_t = merge_if tdenv instrs_t in
      instr_h :: instrs_t
  | { it = GroupI (id_group, rel_signature, exps_group, instrs_group); at; _ }
    :: instrs_t ->
      let instrs_group = merge_if tdenv instrs_group in
      let instr_h =
        GroupI (id_group, rel_signature, exps_group, instrs_group) $ at
      in
      let instrs_t = merge_if tdenv instrs_t in
      instr_h :: instrs_t
  | instr_h :: instrs_t ->
      let instrs_t = merge_if tdenv instrs_t in
      instr_h :: instrs_t

let apply (tdenv : TDEnv.t) (instrs : instr list) : instr list =
  merge_if tdenv instrs
