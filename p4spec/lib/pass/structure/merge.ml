open Ol.Ast

let rec merge_block (instrs_a : instr list) (instrs_b : instr list) : instr list
    =
  match (instrs_a, instrs_b) with
  | instr_a :: instrs_a, instr_b :: instrs_b when Ol.Eq.eq_instr instr_a instr_b
    ->
      let instrs = merge_block instrs_a instrs_b in
      instr_a :: instrs
  | _ -> instrs_a @ instrs_b

and merge_blocks (instrs_group : instr list list) : instr list =
  match instrs_group with
  | [] -> []
  | [ instrs ] -> instrs
  | instrs_a :: instrs_b :: instrs_group ->
      let instrs = merge_block instrs_a instrs_b in
      instrs @ merge_blocks instrs_group
