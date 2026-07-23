module Mixfix = Domain.Mixfix
open Lang
open Ol.Ast
open Util.Source

(* Matchify equals terminal *)

let matchify_exp (exp : exp) : exp =
  let at, note = (exp.at, exp.note) in
  match exp.it with
  (* x = None and None = x *)
  | CmpE (`EqOp, _, exp_l, { it = OptE None; _ }) ->
      Il.MatchE (exp_l, OptP `None) $$ (at, note)
  | CmpE (`EqOp, _, { it = OptE None; _ }, exp_r) ->
      Il.MatchE (exp_r, OptP `None) $$ (at, note)
  (* x = [] and [] = x *)
  | CmpE (`EqOp, _, exp_l, { it = ListE []; _ }) ->
      Il.MatchE (exp_l, ListP `Nil) $$ (at, note)
  | CmpE (`EqOp, _, { it = ListE []; _ }, exp_r) ->
      Il.MatchE (exp_r, ListP `Nil) $$ (at, note)
  (* x = TERM and TERM = x *)
  | CmpE (`EqOp, _, exp_l, { it = CaseE notexp; _ })
    when Mixfix.arity notexp = 0 ->
      Il.MatchE (exp_l, CaseP (Mixfix.to_mixop notexp)) $$ (at, note)
  | CmpE (`EqOp, _, { it = CaseE notexp; _ }, exp_r)
    when Mixfix.arity notexp = 0 ->
      Il.MatchE (exp_r, CaseP (Mixfix.to_mixop notexp)) $$ (at, note)
  (* x != None and None != x *)
  | CmpE (`NeOp, _, exp_l, { it = OptE None; _ }) ->
      Il.MatchE (exp_l, OptP `Some) $$ (at, note)
  | CmpE (`NeOp, _, { it = OptE None; _ }, exp_r) ->
      Il.MatchE (exp_r, OptP `Some) $$ (at, note)
  (* x != [] and [] != x *)
  | CmpE (`NeOp, _, exp_l, { it = ListE []; _ }) ->
      Il.MatchE (exp_l, ListP `Cons) $$ (at, note)
  | CmpE (`NeOp, _, { it = ListE []; _ }, exp_r) ->
      Il.MatchE (exp_r, ListP `Cons) $$ (at, note)
  (* x != TERM and TERM != x *)
  | CmpE (`NeOp, _, exp_l, { it = CaseE notexp; _ })
    when Mixfix.arity notexp = 0 ->
      let exp =
        Il.MatchE (exp_l, CaseP (Mixfix.to_mixop notexp)) $$ (at, note)
      in
      Il.UnE (`NotOp, `BoolT, exp) $$ (at, note)
  | CmpE (`NeOp, _, { it = CaseE notexp; _ }, exp_r)
    when Mixfix.arity notexp = 0 ->
      let exp =
        Il.MatchE (exp_r, CaseP (Mixfix.to_mixop notexp)) $$ (at, note)
      in
      Il.UnE (`NotOp, `BoolT, exp) $$ (at, note)
  | _ -> exp

let rec matchify_instr (instr : instr) : instr =
  let at = instr.at in
  match instr.it with
  | IfI (exp_cond, iterexps, block_then) ->
      let exp_cond = matchify_exp exp_cond in
      let block_then = matchify_block block_then in
      IfI (exp_cond, iterexps, block_then) $ at
  | HoldI (id, notexp, iterexps, block_hold, block_nothold) ->
      let block_hold = matchify_block block_hold in
      let block_nothold = matchify_block block_nothold in
      HoldI (id, notexp, iterexps, block_hold, block_nothold) $ at
  | CaseI (exp, cases, total) ->
      let cases =
        let guards, blocks = List.split cases in
        let blocks = List.map matchify_block blocks in
        List.combine guards blocks
      in
      CaseI (exp, cases, total) $ at
  | GroupI (id_group, rel_signature, exps_group, block) ->
      let block = matchify_block block in
      GroupI (id_group, rel_signature, exps_group, block) $ at
  | LetI (exp_l, exp_r, iterinstrs, block) ->
      let block = matchify_block block in
      LetI (exp_l, exp_r, iterinstrs, block) $ at
  | RuleI (id, notexp, inputs, iterinstrs, block) ->
      let block = matchify_block block in
      RuleI (id, notexp, inputs, iterinstrs, block) $ at
  | _ -> instr

and matchify_block (block : block) : block = List.map matchify_instr block

let apply (block : block) : block = matchify_block block
