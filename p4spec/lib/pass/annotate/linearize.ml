open Lang
open Sl
open Util.Source

(* Linearization *)

let rec linearize_instr (instr : instr) : Ll.Ast.block =
  let at = instr.at in
  let note = instr.note in
  match instr.it with
  | IfI (exp_cond, iterexps, block_then, dangle) ->
      [
        Ll.Ast.IfI (exp_cond, iterexps, linearize_block block_then, dangle)
        $$ (at, note);
      ]
  | HoldI (id, notexp, iterexps, holdcase) ->
      let holdcase_ll =
        match holdcase with
        | BothH (block_hold, block_nothold) ->
            Ll.Ast.BothH
              (linearize_block block_hold, linearize_block block_nothold)
        | HoldH (block_hold, dangle) ->
            Ll.Ast.HoldH (linearize_block block_hold, dangle)
        | NotHoldH (block_nothold, dangle) ->
            Ll.Ast.NotHoldH (linearize_block block_nothold, dangle)
      in
      [ Ll.Ast.HoldI (id, notexp, iterexps, holdcase_ll) $$ (at, note) ]
  | CaseI (exp, cases, dangle) ->
      let cases_ll =
        List.map (fun (guard, block) -> (guard, linearize_block block)) cases
      in
      [ Ll.Ast.CaseI (exp, cases_ll, dangle) $$ (at, note) ]
  | GroupI (id, rel_signature, exps_group, block) ->
      [
        Ll.Ast.GroupI (id, rel_signature, exps_group, linearize_block block)
        $$ (at, note);
      ]
  | LetI (exp_l, exp_r, iterinstrs, block) ->
      let instr_ll = Ll.Ast.LetI (exp_l, exp_r, iterinstrs) $$ (at, note) in
      instr_ll :: linearize_block block
  | RuleI (id, notexp, inputs, iterinstrs, block) ->
      let instr_ll =
        Ll.Ast.RuleI (id, notexp, inputs, iterinstrs) $$ (at, note)
      in
      instr_ll :: linearize_block block
  | ResultI (rel_signature, exps) ->
      [ Ll.Ast.ResultI (rel_signature, exps) $$ (at, note) ]
  | ReturnI exp -> [ Ll.Ast.ReturnI exp $$ (at, note) ]
  | DebugI (exp, instr) ->
      let instr_debug = Ll.Ast.DebugI exp $$ (at, note) in
      instr_debug :: linearize_instr instr

and linearize_block (block : block) : Ll.Ast.block =
  match block with
  | [] -> []
  | [ instr ] -> linearize_instr instr
  | _ ->
      let arms = List.map linearize_instr block in
      let at = block |> List.map Util.Source.at |> over_region in
      [ Ll.Ast.TryI arms $$ (at, { iid = -1 }) ]
