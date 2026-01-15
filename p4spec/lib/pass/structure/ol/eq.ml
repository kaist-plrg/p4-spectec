open Ast
open Lang
open Sl.Eq

(* Cases *)

let rec eq_case (case_a : case) (case_b : case) : bool =
  let guard_a, instrs_a = case_a in
  let guard_b, instrs_b = case_b in
  eq_guard guard_a guard_b && eq_instrs instrs_a instrs_b

and eq_cases (cases_a : case list) (cases_b : case list) : bool =
  List.length cases_a = List.length cases_b
  && List.for_all2 eq_case cases_a cases_b

and eq_guard (guard_a : guard) (guard_b : guard) : bool =
  match (guard_a, guard_b) with
  | BoolG b_a, BoolG b_b -> b_a = b_b
  | CmpG (cmpop_a, optyp_a, exp_a), CmpG (cmpop_b, optyp_b, exp_b) ->
      cmpop_a = cmpop_b && optyp_a = optyp_b && eq_exp exp_a exp_b
  | SubG typ_a, SubG typ_b -> eq_typ typ_a typ_b
  | MatchG pattern_a, MatchG pattern_b -> eq_pattern pattern_a pattern_b
  | MemG exp_a, MemG exp_b -> eq_exp exp_a exp_b
  | _ -> false

(* Instructions *)

and eq_instr (instr_a : instr) (instr_b : instr) : bool =
  match (instr_a.it, instr_b.it) with
  | ( IfI (exp_cond_a, iterexps_a, instrs_then_a),
      IfI (exp_cond_b, iterexps_b, instrs_then_b) ) ->
      eq_exp exp_cond_a exp_cond_b
      && eq_iterexps iterexps_a iterexps_b
      && eq_instrs instrs_then_a instrs_then_b
  | ( HoldI
        (id_a, (mixop_a, exps_a), iterexps_a, instrs_hold_a, instrs_nothold_a),
      HoldI
        (id_b, (mixop_b, exps_b), iterexps_b, instrs_hold_b, instrs_nothold_b) )
    ->
      eq_id id_a id_b && eq_mixop mixop_a mixop_b && eq_exps exps_a exps_b
      && eq_iterexps iterexps_a iterexps_b
      && eq_instrs instrs_hold_a instrs_hold_b
      && eq_instrs instrs_nothold_a instrs_nothold_b
  | CaseI (exp_a, cases_a, total_a), CaseI (exp_b, cases_b, total_b) ->
      eq_exp exp_a exp_b && eq_cases cases_a cases_b && total_a = total_b
  | OtherwiseI instr_a, OtherwiseI instr_b -> eq_instr instr_a instr_b
  | ( GroupI (id_group_a, rel_signature_a, exps_group_a, instrs_group_a),
      GroupI (id_group_b, rel_signature_b, exps_group_b, instrs_group_b) ) ->
      eq_id id_group_a id_group_b
      && eq_rel_signature rel_signature_a rel_signature_b
      && eq_exps exps_group_a exps_group_b
      && eq_instrs instrs_group_a instrs_group_b
  | LetI (exp_l_a, exp_r_a, iterinstrs_a), LetI (exp_l_b, exp_r_b, iterinstrs_b)
    ->
      eq_exp exp_l_a exp_l_b && eq_exp exp_r_a exp_r_b
      && eq_iterinstrs iterinstrs_a iterinstrs_b
  | ( RuleI (id_a, (mixop_a, exps_a), inputs_a, iterinstrs_a),
      RuleI (id_b, (mixop_b, exps_b), inputs_b, iterinstrs_b) ) ->
      eq_id id_a id_b && eq_mixop mixop_a mixop_b && eq_exps exps_a exps_b
      && Hints.Input.eq inputs_a inputs_b
      && eq_iterinstrs iterinstrs_a iterinstrs_b
  | ResultI (rel_signature_a, exps_a), ResultI (rel_signature_b, exps_b) ->
      eq_rel_signature rel_signature_a rel_signature_b && eq_exps exps_a exps_b
  | ReturnI exp_a, ReturnI exp_b -> eq_exp exp_a exp_b
  | DebugI exp_a, DebugI exp_b -> eq_exp exp_a exp_b
  | _ -> false

and eq_instrs (instrs_a : instr list) (instrs_b : instr list) : bool =
  List.length instrs_a = List.length instrs_b
  && List.for_all2 eq_instr instrs_a instrs_b
