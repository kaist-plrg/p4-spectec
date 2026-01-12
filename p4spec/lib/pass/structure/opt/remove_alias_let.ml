open Domain
open Lib
open Lang
open Ol.Ast
open Util.Source

(* Remove redundant, trivial let aliases from the code,

   let y = x; if (y == 0) then { let z = y + y; let y = 1; let k = y + y; ... }

   will be transformed into

   if (x == 0) then { let z = x + x; let y = 1; let k = y + y; ... }

   Notice the stop condition when we meet a shadowing let binding *)

let rec rename (renamer : Renamer.t) (instrs : instr list) : instr list =
  match instrs with
  | [] -> []
  | instr_h :: instrs_t -> (
      match instr_h.it with
      | LetI (exp_l, _, _)
        when not
               (IdSet.is_empty
                  (IdSet.inter (Renamer.dom renamer) (Il.Free.free_exp exp_l)))
        ->
          instr_h :: instrs_t
      | IfI (exp_cond, iterexps, instrs_then) ->
          let exp_cond = Renamer.rename_exp renamer exp_cond in
          let iterexps = Renamer.rename_iterexps renamer iterexps in
          let instrs_then = rename renamer instrs_then in
          let instr_h = IfI (exp_cond, iterexps, instrs_then) $ instr_h.at in
          let instrs_t = rename renamer instrs_t in
          instr_h :: instrs_t
      | HoldI (id, (mixop, exps), iterexps, instrs_hold, instrs_nothold) ->
          let exps = Renamer.rename_exps renamer exps in
          let iterexps = Renamer.rename_iterexps renamer iterexps in
          let instrs_hold = rename renamer instrs_hold in
          let instrs_nothold = rename renamer instrs_nothold in
          let instr_h =
            HoldI (id, (mixop, exps), iterexps, instrs_hold, instrs_nothold)
            $ instr_h.at
          in
          let instrs_t = rename renamer instrs_t in
          instr_h :: instrs_t
      | CaseI (exp, cases, total) ->
          let exp = Renamer.rename_exp renamer exp in
          let cases =
            let guards, instrs = List.split cases in
            let guards = List.map (Renamer.rename_guard renamer) guards in
            let instrs = List.map (rename renamer) instrs in
            List.combine guards instrs
          in
          let instr_h = CaseI (exp, cases, total) $ instr_h.at in
          let instrs_t = rename renamer instrs_t in
          instr_h :: instrs_t
      | GroupI (id_group, rel_signature, exps_group, instrs_group) ->
          let instrs_group = rename renamer instrs_group in
          let exps_group = Renamer.rename_exps renamer exps_group in
          let instr_h =
            GroupI (id_group, rel_signature, exps_group, instrs_group)
            $ instr_h.at
          in
          let instrs_t = rename renamer instrs_t in
          instr_h :: instrs_t
      | _ ->
          let instr_h = Renamer.rename_instr renamer instr_h in
          let instrs_t = rename renamer instrs_t in
          instr_h :: instrs_t)

let rec remove (instrs : instr list) : instr list =
  match instrs with
  | [] -> []
  | instr_h :: instrs_t -> (
      match instr_h.it with
      | LetI ({ it = VarE id_l; _ }, { it = VarE id_r; _ }, _) ->
          let renamer = Renamer.singleton id_l id_r in
          instrs_t |> rename renamer |> remove
      | IfI (exp_cond, iterexps, instrs_then) ->
          let instrs_then = remove instrs_then in
          let instr_h = IfI (exp_cond, iterexps, instrs_then) $ instr_h.at in
          let instrs_t = remove instrs_t in
          instr_h :: instrs_t
      | HoldI (id, notexp, iterexps, instrs_hold, instrs_nothold) ->
          let instrs_hold = remove instrs_hold in
          let instrs_nothold = remove instrs_nothold in
          let instr_h =
            HoldI (id, notexp, iterexps, instrs_hold, instrs_nothold)
            $ instr_h.at
          in
          let instrs_t = remove instrs_t in
          instr_h :: instrs_t
      | CaseI (exp, cases, total) ->
          let cases =
            let guards, blocks = List.split cases in
            let blocks = List.map remove blocks in
            List.combine guards blocks
          in
          let instr_h = CaseI (exp, cases, total) $ instr_h.at in
          let instrs_t = remove instrs_t in
          instr_h :: instrs_t
      | GroupI (id_group, rel_signature, exps_group, instrs_group) ->
          let instrs_group = remove instrs_group in
          let instr_h =
            GroupI (id_group, rel_signature, exps_group, instrs_group)
            $ instr_h.at
          in
          let instrs_t = remove instrs_t in
          instr_h :: instrs_t
      | _ ->
          let instrs_t = remove instrs_t in
          instr_h :: instrs_t)

let apply (instrs : instr list) : instr list = remove instrs
