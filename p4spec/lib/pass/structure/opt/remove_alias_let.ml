open Lang
open Ol.Ast
open Runtime.Dynamic_Sl
open Envs
open Util.Source

(* Remove redundant, trivial let aliases from the code,

   let y = x; if (y == 0) then { let z = y + y; let y = 1; let k = y + y; ... }

   will be transformed into

   if (x == 0) then { let z = x + x; let y = 1; let k = y + y; ... }

   Notice the stop condition when we meet a shadowing let binding

   Other trivial binds include:
      - let y = x*
      - let y = x?
      - let y* = x*
      - let y? = x? *)

let rec remove (ihenv : IHEnv.t) (instrs : instr list) : instr list =
  match instrs with
  | [] -> []
  | instr_h :: instrs_t -> (
      match instr_h.it with
      | LetI ({ it = VarE id_l; _ }, { it = VarE id_r; _ }, _) ->
          let renamer = Renamer.singleton id_l id_r in
          instrs_t |> Renamer.rename_instrs ihenv renamer |> remove ihenv
      | LetI
          ( { it = IterE ({ it = VarE id_l; _ }, (iter_l, _)); _ },
            { it = IterE ({ it = VarE id_r; _ }, (iter_r, _)); _ },
            _ )
        when Il.Eq.eq_iter iter_l iter_r ->
          let renamer = Renamer.singleton id_l id_r in
          instrs_t |> Renamer.rename_instrs ihenv renamer |> remove ihenv
      | LetI
          ( { it = VarE id_l; _ },
            ({ it = IterE ({ it = VarE _; _ }, _); _ } as exp_r),
            _ ) ->
          let replacer = Replacer.singleton id_l exp_r in
          instrs_t |> Replacer.replace_instrs ihenv replacer |> remove ihenv
      | IfI (exp_cond, iterexps, instrs_then) ->
          let instrs_then = remove ihenv instrs_then in
          let instr_h = IfI (exp_cond, iterexps, instrs_then) $ instr_h.at in
          let instrs_t = remove ihenv instrs_t in
          instr_h :: instrs_t
      | HoldI (id, notexp, iterexps, instrs_hold, instrs_nothold) ->
          let instrs_hold = remove ihenv instrs_hold in
          let instrs_nothold = remove ihenv instrs_nothold in
          let instr_h =
            HoldI (id, notexp, iterexps, instrs_hold, instrs_nothold)
            $ instr_h.at
          in
          let instrs_t = remove ihenv instrs_t in
          instr_h :: instrs_t
      | CaseI (exp, cases, total) ->
          let cases =
            let guards, blocks = List.split cases in
            let blocks = List.map (remove ihenv) blocks in
            List.combine guards blocks
          in
          let instr_h = CaseI (exp, cases, total) $ instr_h.at in
          let instrs_t = remove ihenv instrs_t in
          instr_h :: instrs_t
      | GroupI (id_group, rel_signature, exps_group, instrs_group) ->
          let instrs_group = remove ihenv instrs_group in
          let instr_h =
            GroupI (id_group, rel_signature, exps_group, instrs_group)
            $ instr_h.at
          in
          let instrs_t = remove ihenv instrs_t in
          instr_h :: instrs_t
      | _ ->
          let instrs_t = remove ihenv instrs_t in
          instr_h :: instrs_t)

let apply (ihenv : IHEnv.t) (instrs : instr list) : instr list =
  remove ihenv instrs
