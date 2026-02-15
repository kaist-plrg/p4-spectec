open Lang
open Ol.Ast
module Typ = Runtime.Dynamic_Sl.Typ
open Runtime.Dynamic_Sl.Envs
open Overlap
open Util.Source

(* [1] if-and-if to case analysis *)

let casify_if_if (tdenv : TDEnv.t) (at : region) (exp_cond_target : exp)
    (block_then_target : block) (exp_cond : exp) (block_then : block) :
    instr option =
  let overlap_exp_cond = overlap_exp tdenv exp_cond_target exp_cond in
  match overlap_exp_cond with
  | Disjoint (exp, guard_target, guard) ->
      let cases = [ (guard_target, block_then_target); (guard, block_then) ] in
      let instr = CaseI (exp, cases, false) $ at in
      Some instr
  | Partition (exp, guard_target, guard) ->
      let cases = [ (guard_target, block_then_target); (guard, block_then) ] in
      let instr = CaseI (exp, cases, true) $ at in
      Some instr
  | _ -> None

(* [2] if-and-case to case analysis *)

let rec merge_if_case (tdenv : TDEnv.t) (exp_cond_target : exp)
    (block_then_target : block) (exp : exp) (cases : case list) (total : bool) :
    case list option =
  match exp_as_guard exp exp_cond_target with
  | Some guard_target ->
      merge_if_case' tdenv exp cases total [] guard_target block_then_target
  | None -> None

and merge_if_case' (tdenv : TDEnv.t) (exp : exp) (cases : case list)
    (total : bool) (cases_leftover : case list) (guard_target : guard)
    (block_then_target : block) : case list option =
  match cases with
  | [] when total -> assert false
  | [] ->
      let cases = cases_leftover @ [ (guard_target, block_then_target) ] in
      Some cases
  | case_h :: cases_t -> (
      let guard_h, block_h = case_h in
      let overlap_guard = overlap_guard tdenv exp guard_target guard_h in
      match overlap_guard with
      | Identical ->
          let block_h = Merge.merge_block block_then_target block_h in
          let case_h = (guard_h, block_h) in
          Some (case_h :: cases_t)
      | Disjoint _ | Partition _ ->
          let cases_leftover = cases_leftover @ [ case_h ] in
          merge_if_case' tdenv exp cases_t total cases_leftover guard_target
            block_then_target
      | _ -> None)

let casify_if_case (tdenv : TDEnv.t) (at : region) (exp_cond_target : exp)
    (block_then_target : block) (exp : exp) (cases : case list) (total : bool) :
    instr option =
  let cases_opt =
    merge_if_case tdenv exp_cond_target block_then_target exp cases total
  in
  match cases_opt with
  | Some cases ->
      let instr = CaseI (exp, cases, total) $ at in
      Some instr
  | None -> None

(* [3] case-and-if to case analysis *)

let rec merge_case_if (tdenv : TDEnv.t) (exp_target : exp)
    (cases_target : case list) (total_target : bool) (exp_cond : exp)
    (block : block) : case list option =
  match exp_as_guard exp_target exp_cond with
  | Some guard ->
      merge_case_if' tdenv exp_target cases_target [] total_target guard block
  | None -> None

and merge_case_if' (tdenv : TDEnv.t) (exp_target : exp)
    (cases_target : case list) (cases_target_leftover : case list)
    (total_target : bool) (guard : guard) (block : block) : case list option =
  match cases_target with
  | [] when total_target -> assert false
  | [] ->
      let cases = cases_target_leftover @ [ (guard, block) ] in
      Some cases
  | case_target_h :: cases_target_t -> (
      let guard_target_h, block_target_h = case_target_h in
      let overlap_guard = overlap_guard tdenv exp_target guard_target_h guard in
      match overlap_guard with
      | Identical ->
          let block_target_h = Merge.merge_block block_target_h block in
          let case_target_h = (guard_target_h, block_target_h) in
          Some (case_target_h :: cases_target_t)
      | Disjoint _ | Partition _ ->
          let cases_target_leftover =
            cases_target_leftover @ [ case_target_h ]
          in
          merge_case_if' tdenv exp_target cases_target_t cases_target_leftover
            total_target guard block
      | _ -> None)

let casify_case_if (tdenv : TDEnv.t) (at : region) (exp_target : exp)
    (cases_target : case list) (total_target : bool) (exp_cond : exp)
    (block_then : block) : instr option =
  let cases_opt =
    merge_case_if tdenv exp_target cases_target total_target exp_cond block_then
  in
  match cases_opt with
  | Some cases ->
      let instr = CaseI (exp_target, cases, false) $ at in
      Some instr
  | None -> None

(* [4] case-and-case to case analysis *)

let merge_case_case (tdenv : TDEnv.t) (exp_target : exp)
    (cases_target : case list) (total_target : bool) (exp : exp)
    (cases : case list) : case list option =
  if Sl.Eq.eq_exp exp_target exp then
    List.fold_left
      (fun cases_target_opt (guard, block) ->
        match cases_target_opt with
        | Some cases_target ->
            merge_case_if' tdenv exp_target cases_target [] total_target guard
              block
        | None -> None)
      (Some cases_target) cases
  else None

let casify_case_case (tdenv : TDEnv.t) (at : region) (exp_target : exp)
    (cases_target : case list) (total_target : bool) (exp : exp)
    (cases : case list) : instr option =
  let cases_opt =
    merge_case_case tdenv exp_target cases_target total_target exp cases
  in
  match cases_opt with
  | Some cases ->
      let instr = CaseI (exp_target, cases, total_target) $ at in
      Some instr
  | None -> None

(* [1/2] Casifying from an if statement *)

let rec casify_from_if (tdenv : TDEnv.t) (at : region) (exp_cond_target : exp)
    (iterexps_target : iterexp list) (block_then_target : block) (block : block)
    : block option =
  match iterexps_target with
  | [] -> casify_from_if' tdenv at exp_cond_target block_then_target [] block
  | _ -> None

and casify_from_if' (tdenv : TDEnv.t) (at : region) (exp_cond_target : exp)
    (block_then_target : block) (block_leftover : block) (block : block) :
    block option =
  match block with
  | ({ it = IfI (exp_cond, [], block_then); _ } as instr_h) :: block_t -> (
      let instr_h_opt =
        casify_if_if tdenv at exp_cond_target block_then_target exp_cond
          block_then
      in
      match instr_h_opt with
      | Some instr_h -> Some ([ instr_h ] @ block_leftover @ block_t)
      | None ->
          let block_leftover = block_leftover @ [ instr_h ] in
          casify_from_if' tdenv at exp_cond_target block_then_target
            block_leftover block_t)
  | ({ it = CaseI (exp, cases, total); _ } as instr_h) :: block_t -> (
      let instr_h_opt =
        casify_if_case tdenv at exp_cond_target block_then_target exp cases
          total
      in
      match instr_h_opt with
      | Some instr_h -> Some ([ instr_h ] @ block_leftover @ block_t)
      | None ->
          let block_leftover = block_leftover @ [ instr_h ] in
          casify_from_if' tdenv at exp_cond_target block_then_target
            block_leftover block_t)
  | _ -> None

(* [3/4] Casifying from a case statement *)

let rec casify_from_case (tdenv : TDEnv.t) (at : region) (exp_target : exp)
    (cases_target : case list) (total_target : bool) (block : block) :
    block option =
  casify_from_case' tdenv at exp_target cases_target total_target [] block

and casify_from_case' (tdenv : TDEnv.t) (at : region) (exp_target : exp)
    (cases_target : case list) (total_target : bool) (block_leftover : block)
    (block : block) : block option =
  match block with
  | ({ it = IfI (exp_cond, [], block_then); _ } as instr_h) :: block_t -> (
      let instr_h_opt =
        casify_case_if tdenv at exp_target cases_target total_target exp_cond
          block_then
      in
      match instr_h_opt with
      | Some instr_h -> Some ([ instr_h ] @ block_leftover @ block_t)
      | None ->
          let block_leftover = block_leftover @ [ instr_h ] in
          casify_from_case' tdenv at exp_target cases_target total_target
            block_leftover block_t)
  | ({ it = CaseI (exp, cases, _total); _ } as instr_h) :: block_t -> (
      let instr_h_opt =
        casify_case_case tdenv at exp_target cases_target total_target exp cases
      in
      match instr_h_opt with
      | Some instr_h -> Some ([ instr_h ] @ block_leftover @ block_t)
      | None ->
          let block_leftover = block_leftover @ [ instr_h ] in
          casify_from_case' tdenv at exp_target cases_target total_target
            block_leftover block_t)
  | _ -> None

let rec casify (tdenv : TDEnv.t) (block : block) : block =
  match block with
  | [] -> []
  | { it = IfI (exp_cond, iterexps, block_then); at; _ } :: block_t -> (
      match casify_from_if tdenv at exp_cond iterexps block_then block_t with
      | Some block -> casify tdenv block
      | None ->
          let instr_h =
            let block_then = casify tdenv block_then in
            IfI (exp_cond, iterexps, block_then) $ at
          in
          let block_t = casify tdenv block_t in
          instr_h :: block_t)
  | { it = HoldI (id, notexp, iterexps, block_then, block_else); at; _ }
    :: block_t ->
      let block_then = casify tdenv block_then in
      let block_else = casify tdenv block_else in
      let instr_h = HoldI (id, notexp, iterexps, block_then, block_else) $ at in
      let block_t = casify tdenv block_t in
      instr_h :: block_t
  | { it = CaseI (exp, cases, total); at; _ } :: block_t -> (
      match casify_from_case tdenv at exp cases total block_t with
      | Some block -> casify tdenv block
      | None ->
          let instr_h =
            let guards, blocks = List.split cases in
            let blocks = List.map (casify tdenv) blocks in
            let cases = List.combine guards blocks in
            CaseI (exp, cases, total) $ at
          in
          let block_t = casify tdenv block_t in
          instr_h :: block_t)
  | { it = GroupI (id_group, rel_signature, exps_group, block); at; _ }
    :: block_t ->
      let block = casify tdenv block in
      let instr_h = GroupI (id_group, rel_signature, exps_group, block) $ at in
      let block_t = casify tdenv block_t in
      instr_h :: block_t
  | { it = LetI (exp_l, exp_r, iterinstrs, block); at; _ } :: block_t ->
      let block = casify tdenv block in
      let instr_h = LetI (exp_l, exp_r, iterinstrs, block) $ at in
      let block_t = casify tdenv block_t in
      instr_h :: block_t
  | { it = RuleI (id, notexp, inputs, iterinstrs, block); at; _ } :: block_t ->
      let block = casify tdenv block in
      let instr_h = RuleI (id, notexp, inputs, iterinstrs, block) $ at in
      let block_t = casify tdenv block_t in
      instr_h :: block_t
  | instr_h :: block_t ->
      let block_t = casify tdenv block_t in
      instr_h :: block_t

let apply (tdenv : TDEnv.t) (block : block) : block = casify tdenv block
