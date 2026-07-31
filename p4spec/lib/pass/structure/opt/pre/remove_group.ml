open Ol.Ast
open Util.Source

(* Remove group instructions *)

let rec remove_instr (instr : instr) : block =
  match instr.it with
  | IfI (exp_cond, iterexps, block_then) ->
      let block_then = remove_block block_then in
      [ IfI (exp_cond, iterexps, block_then) $ instr.at ]
  | HoldI (id, notexp, iterexps, block_hold, block_nothold) ->
      let block_hold = remove_block block_hold in
      let block_nothold = remove_block block_nothold in
      [ HoldI (id, notexp, iterexps, block_hold, block_nothold) $ instr.at ]
  | CaseI (exp, cases, total) ->
      let cases =
        let guards, blocks = List.split cases in
        let blocks = List.map remove_block blocks in
        List.combine guards blocks
      in
      [ CaseI (exp, cases, total) $ instr.at ]
  | GroupI (_, _, _, block) -> remove_block block
  | LetI (exp_l, exp_r, iterinstrs, block) ->
      let block = remove_block block in
      [ LetI (exp_l, exp_r, iterinstrs, block) $ instr.at ]
  | RuleI (id, notexp, inputs, iterinstrs, block) ->
      let block = remove_block block in
      [ RuleI (id, notexp, inputs, iterinstrs, block) $ instr.at ]
  | _ -> [ instr ]

and remove_block (block : block) : block =
  match block with
  | [] -> []
  | instr_h :: block_t ->
      let block_h = remove_instr instr_h in
      let block_t = remove_block block_t in
      block_h @ block_t

let apply (block : block) : block = remove_block block
