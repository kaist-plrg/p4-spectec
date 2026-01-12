open Lang
open Ol.Ast
module Typ = Runtime.Dynamic_Sl.Typ
open Runtime.Dynamic_Sl.Envs
open Overlap
open Util.Source

(* [1] if-and-if to case analysis *)

let casify_if_if (tdenv : TDEnv.t) (at : region) (exp_cond_target : exp)
    (instrs_then_target : instr list) (exp_cond : exp)
    (instrs_then : instr list) : instr option =
  let overlap_exp_cond = overlap_exp tdenv exp_cond_target exp_cond in
  match overlap_exp_cond with
  | Disjoint (exp, guard_target, guard) ->
      let cases =
        [ (guard_target, instrs_then_target); (guard, instrs_then) ]
      in
      let instr = CaseI (exp, cases, false) $ at in
      Some instr
  | Partition (exp, guard_target, guard) ->
      let cases =
        [ (guard_target, instrs_then_target); (guard, instrs_then) ]
      in
      let instr = CaseI (exp, cases, true) $ at in
      Some instr
  | _ -> None

(* [2] if-and-case to case analysis *)

let rec merge_if_case (tdenv : TDEnv.t) (exp_cond_target : exp)
    (instrs_then_target : instr list) (exp : exp) (cases : case list)
    (total : bool) : case list option =
  match exp_as_guard exp exp_cond_target with
  | Some guard_target ->
      merge_if_case' tdenv exp cases total [] guard_target instrs_then_target
  | None -> None

and merge_if_case' (tdenv : TDEnv.t) (exp : exp) (cases : case list)
    (total : bool) (cases_leftover : case list) (guard_target : guard)
    (instrs_then_target : instr list) : case list option =
  match cases with
  | [] when total -> assert false
  | [] ->
      let cases = cases_leftover @ [ (guard_target, instrs_then_target) ] in
      Some cases
  | case_h :: cases_t -> (
      let guard_h, instrs_h = case_h in
      let overlap_guard = overlap_guard tdenv exp guard_target guard_h in
      match overlap_guard with
      | Identical ->
          let instrs_h = Merge.merge_block instrs_then_target instrs_h in
          let case_h = (guard_h, instrs_h) in
          Some (case_h :: cases_t)
      | Disjoint _ | Partition _ ->
          let cases_leftover = cases_leftover @ [ case_h ] in
          merge_if_case' tdenv exp cases_t total cases_leftover guard_target
            instrs_then_target
      | _ -> None)

let casify_if_case (tdenv : TDEnv.t) (at : region) (exp_cond_target : exp)
    (instrs_then_target : instr list) (exp : exp) (cases : case list)
    (total : bool) : instr option =
  let cases_opt =
    merge_if_case tdenv exp_cond_target instrs_then_target exp cases total
  in
  match cases_opt with
  | Some cases ->
      let instr = CaseI (exp, cases, total) $ at in
      Some instr
  | None -> None

(* [3] case-and-if to case analysis *)

let rec merge_case_if (tdenv : TDEnv.t) (exp_target : exp)
    (cases_target : case list) (total_target : bool) (exp_cond : exp)
    (instrs : instr list) : case list option =
  match exp_as_guard exp_target exp_cond with
  | Some guard ->
      merge_case_if' tdenv exp_target cases_target [] total_target guard instrs
  | None -> None

and merge_case_if' (tdenv : TDEnv.t) (exp_target : exp)
    (cases_target : case list) (cases_target_leftover : case list)
    (total_target : bool) (guard : guard) (instrs : instr list) :
    case list option =
  match cases_target with
  | [] when total_target -> assert false
  | [] ->
      let cases = cases_target_leftover @ [ (guard, instrs) ] in
      Some cases
  | case_target_h :: cases_target_t -> (
      let guard_target_h, instrs_target_h = case_target_h in
      let overlap_guard = overlap_guard tdenv exp_target guard_target_h guard in
      match overlap_guard with
      | Identical ->
          let instrs_target_h = Merge.merge_block instrs_target_h instrs in
          let case_target_h = (guard_target_h, instrs_target_h) in
          Some (case_target_h :: cases_target_t)
      | Disjoint _ | Partition _ ->
          let cases_target_leftover =
            cases_target_leftover @ [ case_target_h ]
          in
          merge_case_if' tdenv exp_target cases_target_t cases_target_leftover
            total_target guard instrs
      | _ -> None)

let casify_case_if (tdenv : TDEnv.t) (at : region) (exp_target : exp)
    (cases_target : case list) (total_target : bool) (exp_cond : exp)
    (instrs_then : instr list) : instr option =
  let cases_opt =
    merge_case_if tdenv exp_target cases_target total_target exp_cond
      instrs_then
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
      (fun cases_target_opt (guard, instrs) ->
        match cases_target_opt with
        | Some cases_target ->
            merge_case_if' tdenv exp_target cases_target [] total_target guard
              instrs
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
    (iterexps_target : iterexp list) (instrs_then_target : instr list)
    (instrs : instr list) : instr list option =
  match iterexps_target with
  | [] -> casify_from_if' tdenv at exp_cond_target instrs_then_target [] instrs
  | _ -> None

and casify_from_if' (tdenv : TDEnv.t) (at : region) (exp_cond_target : exp)
    (instrs_then_target : instr list) (instrs_leftover : instr list)
    (instrs : instr list) : instr list option =
  match instrs with
  | ({ it = IfI (exp_cond, [], instrs_then); _ } as instr_h) :: instrs_t -> (
      let instr_h_opt =
        casify_if_if tdenv at exp_cond_target instrs_then_target exp_cond
          instrs_then
      in
      match instr_h_opt with
      | Some instr_h -> Some ([ instr_h ] @ instrs_leftover @ instrs_t)
      | None ->
          let instrs_leftover = instrs_leftover @ [ instr_h ] in
          casify_from_if' tdenv at exp_cond_target instrs_then_target
            instrs_leftover instrs_t)
  | ({ it = CaseI (exp, cases, total); _ } as instr_h) :: instrs_t -> (
      let instr_h_opt =
        casify_if_case tdenv at exp_cond_target instrs_then_target exp cases
          total
      in
      match instr_h_opt with
      | Some instr_h -> Some ([ instr_h ] @ instrs_leftover @ instrs_t)
      | None ->
          let instrs_leftover = instrs_leftover @ [ instr_h ] in
          casify_from_if' tdenv at exp_cond_target instrs_then_target
            instrs_leftover instrs_t)
  | _ -> None

(* [3/4] Casifying from a case statement *)

let rec casify_from_case (tdenv : TDEnv.t) (at : region) (exp_target : exp)
    (cases_target : case list) (total_target : bool) (instrs : instr list) :
    instr list option =
  casify_from_case' tdenv at exp_target cases_target total_target [] instrs

and casify_from_case' (tdenv : TDEnv.t) (at : region) (exp_target : exp)
    (cases_target : case list) (total_target : bool)
    (instrs_leftover : instr list) (instrs : instr list) : instr list option =
  match instrs with
  | ({ it = IfI (exp_cond, [], instrs_then); _ } as instr_h) :: instrs_t -> (
      let instr_h_opt =
        casify_case_if tdenv at exp_target cases_target total_target exp_cond
          instrs_then
      in
      match instr_h_opt with
      | Some instr_h -> Some ([ instr_h ] @ instrs_leftover @ instrs_t)
      | None ->
          let instrs_leftover = instrs_leftover @ [ instr_h ] in
          casify_from_case' tdenv at exp_target cases_target total_target
            instrs_leftover instrs_t)
  | ({ it = CaseI (exp, cases, _total); _ } as instr_h) :: instrs_t -> (
      let instr_h_opt =
        casify_case_case tdenv at exp_target cases_target total_target exp cases
      in
      match instr_h_opt with
      | Some instr_h -> Some ([ instr_h ] @ instrs_leftover @ instrs_t)
      | None ->
          let instrs_leftover = instrs_leftover @ [ instr_h ] in
          casify_from_case' tdenv at exp_target cases_target total_target
            instrs_leftover instrs_t)
  | _ -> None

let rec casify (tdenv : TDEnv.t) (instrs : instr list) : instr list =
  match instrs with
  | [] -> []
  | { it = IfI (exp_cond, iterexps, instrs_then); at; _ } :: instrs_t -> (
      match casify_from_if tdenv at exp_cond iterexps instrs_then instrs_t with
      | Some instrs -> casify tdenv instrs
      | None ->
          let instr_h =
            let instrs_then = casify tdenv instrs_then in
            IfI (exp_cond, iterexps, instrs_then) $ at
          in
          let instrs_t = casify tdenv instrs_t in
          instr_h :: instrs_t)
  | { it = HoldI (id, notexp, iterexps, instrs_then, instrs_else); at; _ }
    :: instrs_t ->
      let instrs_then = casify tdenv instrs_then in
      let instrs_else = casify tdenv instrs_else in
      let instr_h =
        HoldI (id, notexp, iterexps, instrs_then, instrs_else) $ at
      in
      let instrs_t = casify tdenv instrs_t in
      instr_h :: instrs_t
  | { it = CaseI (exp, cases, total); at; _ } :: instrs_t -> (
      match casify_from_case tdenv at exp cases total instrs_t with
      | Some instrs -> casify tdenv instrs
      | None ->
          let instr_h =
            let guards, blocks = List.split cases in
            let blocks = List.map (casify tdenv) blocks in
            let cases = List.combine guards blocks in
            CaseI (exp, cases, total) $ at
          in
          let instrs_t = casify tdenv instrs_t in
          instr_h :: instrs_t)
  | { it = GroupI (id_group, rel_signature, exps_group, instrs_group); at; _ }
    :: instrs_t ->
      let instrs_group = casify tdenv instrs_group in
      let instr_h =
        GroupI (id_group, rel_signature, exps_group, instrs_group) $ at
      in
      let instrs_t = casify tdenv instrs_t in
      instr_h :: instrs_t
  | instr_h :: instrs_t ->
      let instrs_t = casify tdenv instrs_t in
      instr_h :: instrs_t

let apply (tdenv : TDEnv.t) (instrs : instr list) : instr list =
  casify tdenv instrs
