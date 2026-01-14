open Lang
open Ol.Ast
module Typ = Runtime.Dynamic_Sl.Typ
open Util.Source

(* Merge consecutive hold statements with the same holding condition *)

let rec merge_identical_hold (at : region) (id_target : id)
    (notexp_target : notexp) (iterexps_target : iterexp list)
    (instrs_hold_target : instr list) (instrs_nothold_target : instr list)
    (instrs : instr list) : instr list option =
  match instrs with
  | { it = HoldI (id, notexp, iterexps, instrs_hold, instrs_nothold); _ }
    :: instrs_t ->
      let mixop_target, exps_target = notexp_target in
      let mixop, exps = notexp in
      if
        Sl.Eq.eq_id id id_target
        && Sl.Eq.eq_mixop mixop mixop_target
        && Sl.Eq.eq_exps exps exps_target
        && Sl.Eq.eq_iterexps iterexps iterexps_target
      then
        let instrs_hold = Merge.merge_block instrs_hold_target instrs_hold in
        let instrs_nothold =
          Merge.merge_block instrs_nothold_target instrs_nothold
        in
        let instr_h =
          HoldI (id, notexp, iterexps, instrs_hold, instrs_nothold) $ at
        in
        let instrs_t = merge_hold instrs_t in
        Some (instr_h :: instrs_t)
      else None
  | _ -> None

and merge_hold (instrs : instr list) : instr list =
  match instrs with
  | [] -> []
  | { it = IfI (exp_cond, iterexps, instrs_then); at; _ } :: instrs_t ->
      let instrs_then = merge_hold instrs_then in
      let instr_h = IfI (exp_cond, iterexps, instrs_then) $ at in
      let instrs_t = merge_hold instrs_t in
      instr_h :: instrs_t
  | { it = HoldI (id, notexp, iterexps, instrs_hold, instrs_nothold); at; _ }
    :: instrs_t -> (
      match
        merge_identical_hold at id notexp iterexps instrs_hold instrs_nothold
          instrs_t
      with
      | Some instrs -> merge_hold instrs
      | None ->
          let instrs_hold = merge_hold instrs_hold in
          let instrs_nothold = merge_hold instrs_nothold in
          let instr_h =
            HoldI (id, notexp, iterexps, instrs_hold, instrs_nothold) $ at
          in
          let instrs_t = merge_hold instrs_t in
          instr_h :: instrs_t)
  | { it = CaseI (exp, cases, total); at; _ } :: instrs_t ->
      let instr_h =
        let guards, blocks = List.split cases in
        let blocks = List.map merge_hold blocks in
        let cases = List.combine guards blocks in
        CaseI (exp, cases, total) $ at
      in
      let instrs_t = merge_hold instrs_t in
      instr_h :: instrs_t
  | { it = GroupI (id_group, rel_signature, exps_group, instrs_group); at; _ }
    :: instrs_t ->
      let instrs_group = merge_hold instrs_group in
      let instr_h =
        GroupI (id_group, rel_signature, exps_group, instrs_group) $ at
      in
      let instrs_t = merge_hold instrs_t in
      instr_h :: instrs_t
  | instr_h :: instrs_t ->
      let instrs_t = merge_hold instrs_t in
      instr_h :: instrs_t

let apply (instrs : instr list) : instr list = merge_hold instrs
