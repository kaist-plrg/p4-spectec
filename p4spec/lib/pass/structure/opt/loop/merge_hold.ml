open Lang
open Ol.Ast
module Typ = Runtime.Dynamic_Sl.Typ
open Util.Source

(* Merge consecutive hold statements with the same holding condition *)

let rec merge_identical_hold (at : region) (id_target : id)
    (notexp_target : notexp) (iterexps_target : iterexp list)
    (block_hold_target : block) (block_nothold_target : block) (block : block) :
    block option =
  match block with
  | { it = HoldI (id, notexp, iterexps, block_hold, block_nothold); _ }
    :: block_t ->
      let mixop_target, exps_target = notexp_target in
      let mixop, exps = notexp in
      if
        Sl.Eq.eq_id id id_target
        && Sl.Eq.eq_mixop mixop mixop_target
        && Sl.Eq.eq_exps exps exps_target
        && Sl.Eq.eq_iterexps iterexps iterexps_target
      then
        let block_hold = Merge.merge_block block_hold_target block_hold in
        let block_nothold =
          Merge.merge_block block_nothold_target block_nothold
        in
        let instr_h =
          HoldI (id, notexp, iterexps, block_hold, block_nothold) $ at
        in
        let block_t = merge_hold block_t in
        Some (instr_h :: block_t)
      else None
  | _ -> None

and merge_hold (block : block) : block =
  match block with
  | [] -> []
  | { it = IfI (exp_cond, iterexps, block_then); at; _ } :: block_t ->
      let block_then = merge_hold block_then in
      let instr_h = IfI (exp_cond, iterexps, block_then) $ at in
      let block_t = merge_hold block_t in
      instr_h :: block_t
  | { it = HoldI (id, notexp, iterexps, block_hold, block_nothold); at; _ }
    :: block_t -> (
      match
        merge_identical_hold at id notexp iterexps block_hold block_nothold
          block_t
      with
      | Some block -> merge_hold block
      | None ->
          let block_hold = merge_hold block_hold in
          let block_nothold = merge_hold block_nothold in
          let instr_h =
            HoldI (id, notexp, iterexps, block_hold, block_nothold) $ at
          in
          let block_t = merge_hold block_t in
          instr_h :: block_t)
  | { it = CaseI (exp, cases, total); at; _ } :: block_t ->
      let instr_h =
        let guards, blocks = List.split cases in
        let blocks = List.map merge_hold blocks in
        let cases = List.combine guards blocks in
        CaseI (exp, cases, total) $ at
      in
      let block_t = merge_hold block_t in
      instr_h :: block_t
  | { it = GroupI (id_group, rel_signature, exps_group, block); at; _ }
    :: block_t ->
      let block = merge_hold block in
      let instr_h = GroupI (id_group, rel_signature, exps_group, block) $ at in
      let block_t = merge_hold block_t in
      instr_h :: block_t
  | { it = LetI (exp_l, exp_r, iterinstrs, block); at; _ } :: block_t ->
      let block = merge_hold block in
      let instr_h = LetI (exp_l, exp_r, iterinstrs, block) $ at in
      let block_t = merge_hold block_t in
      instr_h :: block_t
  | { it = RuleI (id, notexp, inputs, iterinstrs, block); at; _ } :: block_t ->
      let block = merge_hold block in
      let instr_h = RuleI (id, notexp, inputs, iterinstrs, block) $ at in
      let block_t = merge_hold block_t in
      instr_h :: block_t
  | instr_h :: block_t ->
      let block_t = merge_hold block_t in
      instr_h :: block_t

let apply (block : block) : block = merge_hold block
