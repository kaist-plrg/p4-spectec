open Lang
open Sl
open Util.Source

(* Linearization *)

let rec linearize_instr (instr : instr) : Ll.Ast.block =
  let at = instr.at in
  let note = instr.note in
  match instr.it with
  | IfI (exp_cond, iterexps, block_then, phantom_opt) ->
      let block_then_ll = linearize_block block_then in
      [
        Ll.Ast.IfI (exp_cond, iterexps, block_then_ll, phantom_opt) $$ (at, note);
      ]
  | HoldI (id, notexp, iterexps, holdcase) ->
      let holdcase_ll =
        match holdcase with
        | BothH (block_hold, block_nothold) ->
            let block_hold_ll = linearize_block block_hold in
            let block_nothold_ll = linearize_block block_nothold in
            Ll.Ast.BothH (block_hold_ll, block_nothold_ll)
        | HoldH (block_hold, phantom_opt) ->
            let block_hold_ll = linearize_block block_hold in
            Ll.Ast.HoldH (block_hold_ll, phantom_opt)
        | NotHoldH (block_nothold, phantom_opt) ->
            let block_nothold_ll = linearize_block block_nothold in
            Ll.Ast.NotHoldH (block_nothold_ll, phantom_opt)
      in
      [ Ll.Ast.HoldI (id, notexp, iterexps, holdcase_ll) $$ (at, note) ]
  | CaseI (exp, cases, phantom_opt) ->
      let cases_ll =
        List.map
          (fun (guard, block) ->
            let block_ll = linearize_block block in
            (guard, block_ll))
          cases
      in
      [ Ll.Ast.CaseI (exp, cases_ll, phantom_opt) $$ (at, note) ]
  | GroupI (id, rel_signature, exps_group, block) ->
      let block_ll = linearize_block block in
      [ Ll.Ast.GroupI (id, rel_signature, exps_group, block_ll) $$ (at, note) ]
  | LetI (exp_l, exp_r, iterinstrs, block) ->
      let block_ll = linearize_block block in
      let instr_ll = Ll.Ast.LetI (exp_l, exp_r, iterinstrs) $$ (at, note) in
      instr_ll :: block_ll
  | RuleI (id, notexp, inputs, iterinstrs, block) ->
      let block_ll = linearize_block block in
      let instr_ll =
        Ll.Ast.RuleI (id, notexp, inputs, iterinstrs) $$ (at, note)
      in
      instr_ll :: block_ll
  | ResultI (rel_signature, exps) ->
      [ Ll.Ast.ResultI (rel_signature, exps) $$ (at, note) ]
  | ReturnI exp -> [ Ll.Ast.ReturnI exp $$ (at, note) ]
  | DebugI _ -> []

and linearize_block (block : block) : Ll.Ast.block =
  block |> List.concat_map linearize_instr

let linearize_elseblock (elseblock : elseblock) : Ll.Ast.block =
  let block_ll = linearize_block elseblock in
  [ Ll.Ast.OtherwiseI block_ll $$ (no_region, { iid = -1 }) ]

let linearize_elseblock_opt (elseblock_opt : elseblock option) : Ll.Ast.block =
  match elseblock_opt with
  | Some elseblock -> linearize_elseblock elseblock
  | None -> []
