open Lang
open Ol.Ast
open Util.Source

(* Matchify equals terminal *)

let matchify_exp_eq_terminal (exp : exp) : exp =
  let at, note = (exp.at, exp.note) in
  match exp.it with
  | CmpE (`EqOp, _, exp_l, { it = OptE None; _ }) ->
      Il.MatchE (exp_l, OptP `None) $$ (at, note)
  | CmpE (`EqOp, _, exp_l, { it = CaseE (mixop, []); _ }) ->
      Il.MatchE (exp_l, CaseP mixop) $$ (at, note)
  | CmpE (`EqOp, _, { it = CaseE (mixop, []); _ }, exp_r) ->
      Il.MatchE (exp_r, CaseP mixop) $$ (at, note)
  | CmpE (`NeOp, _, exp_l, { it = OptE None; _ }) ->
      Il.MatchE (exp_l, OptP `Some) $$ (at, note)
  | CmpE (`NeOp, _, exp_l, { it = CaseE (mixop, []); _ }) ->
      let exp = Il.MatchE (exp_l, CaseP mixop) $$ (at, note) in
      Il.UnE (`NotOp, `BoolT, exp) $$ (at, note)
  | CmpE (`NeOp, _, { it = CaseE (mixop, []); _ }, exp_r) ->
      let exp = Il.MatchE (exp_r, CaseP mixop) $$ (at, note) in
      Il.UnE (`NotOp, `BoolT, exp) $$ (at, note)
  | _ -> exp

let rec matchify_if_eq_terminal (instr : instr) : instr =
  let at = instr.at in
  match instr.it with
  | IfI (exp_cond, iterexps, instrs_then) ->
      let exp_cond = matchify_exp_eq_terminal exp_cond in
      let instrs_then = matchify_if_eq_terminals instrs_then in
      IfI (exp_cond, iterexps, instrs_then) $ at
  | HoldI (id, notexp, iterexps, instrs_hold, instrs_nothold) ->
      let instrs_hold = matchify_if_eq_terminals instrs_hold in
      let instrs_nothold = matchify_if_eq_terminals instrs_nothold in
      HoldI (id, notexp, iterexps, instrs_hold, instrs_nothold) $ at
  | CaseI (exp, cases, total) ->
      let cases =
        let guards, blocks = List.split cases in
        let blocks = List.map matchify_if_eq_terminals blocks in
        List.combine guards blocks
      in
      CaseI (exp, cases, total) $ at
  | GroupI (id_group, rel_signature, exps_group, instrs_group) ->
      let instrs_group = matchify_if_eq_terminals instrs_group in
      GroupI (id_group, rel_signature, exps_group, instrs_group) $ at
  | _ -> instr

and matchify_if_eq_terminals (instrs : instr list) : instr list =
  List.map matchify_if_eq_terminal instrs

let apply (instrs : instr list) : instr list = matchify_if_eq_terminals instrs
