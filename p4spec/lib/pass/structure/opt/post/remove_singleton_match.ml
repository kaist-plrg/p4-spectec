open Ol.Ast
module Typ = Runtime.Dynamic_Sl.Typ
open Runtime.Dynamic_Sl.Envs
open Util.Source

(* Remove redundant match on singleton case

   with type foo = AAA,

   if foo matches pattern AAA then ...

   will be removed *)

let is_singleton_case (tdenv : TDEnv.t) (typ : typ) : bool =
  let typ_unrolled = TDEnv.unroll tdenv typ in
  match typ_unrolled.it with
  | VarT (tid, _) -> (
      let td = TDEnv.find tid tdenv in
      match td with
      | Param | Extern -> false
      | Defined (_, deftyp) -> (
          match deftyp.it with
          | VariantT typcases -> List.length typcases = 1
          | _ -> false))
  | _ -> false

let is_singleton_match (tdenv : TDEnv.t) (exp : exp) : bool =
  match exp.it with
  | MatchE (exp, _) -> is_singleton_case tdenv (exp.note $ exp.at)
  | _ -> false

let rec remove_instr (tdenv : TDEnv.t) (instr : instr) : block =
  match instr.it with
  | IfI (exp_cond, _, block_then) when is_singleton_match tdenv exp_cond ->
      remove_block tdenv block_then
  | IfI (exp_cond, iterexps, block_then) ->
      let block_then = remove_block tdenv block_then in
      let instr = IfI (exp_cond, iterexps, block_then) $ instr.at in
      [ instr ]
  | HoldI (id, notexp, iterexps, block_hold, block_nothold) ->
      let block_hold = remove_block tdenv block_hold in
      let block_nothold = remove_block tdenv block_nothold in
      let instr =
        HoldI (id, notexp, iterexps, block_hold, block_nothold) $ instr.at
      in
      [ instr ]
  | CaseI (exp, cases, total) ->
      let cases =
        let guards, blocks = List.split cases in
        let blocks = List.map (remove_block tdenv) blocks in
        List.combine guards blocks
      in
      let instr = CaseI (exp, cases, total) $ instr.at in
      [ instr ]
  | GroupI (id_group, rel_signature, exps_group, block) ->
      let block = remove_block tdenv block in
      let instr =
        GroupI (id_group, rel_signature, exps_group, block) $ instr.at
      in
      [ instr ]
  | LetI (exp_l, exp_r, iterinstrs, block) ->
      let block = remove_block tdenv block in
      let instr = LetI (exp_l, exp_r, iterinstrs, block) $ instr.at in
      [ instr ]
  | RuleI (id, notexp, inputs, iterinstrs, block) ->
      let block = remove_block tdenv block in
      let instr = RuleI (id, notexp, inputs, iterinstrs, block) $ instr.at in
      [ instr ]
  | _ -> [ instr ]

and remove_block (tdenv : TDEnv.t) (block : block) : block =
  match block with
  | [] -> []
  | instr_h :: block_t ->
      let instr_h = remove_instr tdenv instr_h in
      let block_t = remove_block tdenv block_t in
      instr_h @ block_t

let apply (tdenv : TDEnv.t) (block : block) : block = remove_block tdenv block
