open Lang
open Sl
open Util.Source

(* Linearization *)

let rec linearize_instr (instr : instr) : Ll.Ast.block =
  let at = instr.at in
  let note = instr.note in
  match instr.it with
  | IfI (exp_cond, iterexps, block_then, dangle) ->
      let block_then_ll = linearize_block block_then in
      [ Ll.Ast.IfI (exp_cond, iterexps, block_then_ll, dangle) $$ (at, note) ]
  | HoldI (id, notexp, iterexps, holdcase) ->
      let holdcase_ll =
        match holdcase with
        | BothH (block_hold, block_nothold) ->
            let block_hold_ll = linearize_block block_hold in
            let block_nothold_ll = linearize_block block_nothold in
            Ll.Ast.BothH (block_hold_ll, block_nothold_ll)
        | HoldH (block_hold, dangle) ->
            let block_hold_ll = linearize_block block_hold in
            Ll.Ast.HoldH (block_hold_ll, dangle)
        | NotHoldH (block_nothold, dangle) ->
            let block_nothold_ll = linearize_block block_nothold in
            Ll.Ast.NotHoldH (block_nothold_ll, dangle)
      in
      [ Ll.Ast.HoldI (id, notexp, iterexps, holdcase_ll) $$ (at, note) ]
  | CaseI (exp, cases, dangle) ->
      let cases_ll =
        List.map
          (fun (guard, block) ->
            let block_ll = linearize_block block in
            (guard, block_ll))
          cases
      in
      [ Ll.Ast.CaseI (exp, cases_ll, dangle) $$ (at, note) ]
  | GroupI (id, rel_signature, exps_group, block) ->
      let arms = List.map linearize_instr block in
      let block_ll =
        match arms with
        | [] -> []
        | [ single_arm ] -> single_arm
        | _ -> [ Ll.Ast.TryI arms $$ (no_region, { iid = -1 }) ]
      in
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
  | DebugI (exp, instr) ->
      let instr_debug = Ll.Ast.DebugI exp $$ (at, note) in
      instr_debug :: linearize_instr instr

and linearize_block (block : block) : Ll.Ast.block =
  block |> List.concat_map linearize_instr |> wrap_try_arms

and is_branching (instr : Ll.Ast.instr) : bool =
  match instr.it with IfI _ | HoldI _ | CaseI _ -> true | _ -> false

and wrap_try_arms (instrs : Ll.Ast.block) : Ll.Ast.block =
  match split_leading_branches instrs with
  | [], [] -> []
  | [], instr :: instrs -> instr :: wrap_try_arms instrs
  | [ single ], remainder -> single :: wrap_try_arms remainder
  | branches, remainder ->
      let arms = List.map (fun i -> [ i ]) branches in
      let wrapped = Ll.Ast.TryI arms $$ (no_region, { iid = -1 }) in
      wrapped :: wrap_try_arms remainder

and split_leading_branches (instrs : Ll.Ast.block) :
    Ll.Ast.instr list * Ll.Ast.block =
  match instrs with
  | instr :: rest when is_branching instr ->
      let branches, remainder = split_leading_branches rest in
      (instr :: branches, remainder)
  | _ -> ([], instrs)

let linearize_elseblock (elseblock : elseblock) : Ll.Ast.block =
  let block_ll = linearize_block elseblock in
  [ Ll.Ast.OtherwiseI block_ll $$ (no_region, { iid = -1 }) ]

let linearize_elseblock_opt (elseblock_opt : elseblock option) : Ll.Ast.block =
  match elseblock_opt with
  | Some elseblock -> linearize_elseblock elseblock
  | None -> []
