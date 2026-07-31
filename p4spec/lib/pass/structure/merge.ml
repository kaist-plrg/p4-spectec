open Ol.Ast
open Ol.Eq
open Util.Source

let rec merge_block (block_a : instr list) (block_b : instr list) : block =
  match (block_a, block_b) with
  | ( { it = IfI (exp_cond_a, iterexps_a, block_a); at; _ } :: block_a_t,
      { it = IfI (exp_cond_b, iterexps_b, block_b); _ } :: block_b_t )
    when eq_exp exp_cond_a exp_cond_b && eq_iterexps iterexps_a iterexps_b ->
      let block = merge_block block_a block_b in
      let instr_h = IfI (exp_cond_a, iterexps_a, block) $ at in
      let block_a = instr_h :: block_a_t in
      merge_block block_a block_b_t
  | _ -> block_a @ block_b

and merge_blocks (blocks : block list) : block =
  match blocks with
  | [] -> []
  | [ block ] -> block
  | block_a :: block_b :: blocks_t ->
      let block = merge_block block_a block_b in
      merge_blocks (block :: blocks_t)
