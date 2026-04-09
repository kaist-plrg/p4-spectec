open Ol.Ast

(* Prettify instructions *)

let rec pretty_rel (exps_match : exp list) (block : block)
    (elseblock_opt : elseblock option) : exp list * block * elseblock option =
  let exps_match_pretty, block_pretty, elseblock_pretty_opt =
    (exps_match, block, elseblock_opt)
    |> Pretty.Revive_underscore.apply_rel |> Pretty.Rename_tick.apply_rel
  in
  if
    Ol.Eq.eq_exps exps_match exps_match_pretty
    && Ol.Eq.eq_block block block_pretty
    && Ol.Eq.eq_elseblock_opt elseblock_opt elseblock_pretty_opt
  then (exps_match, block, elseblock_opt)
  else pretty_rel exps_match_pretty block_pretty elseblock_pretty_opt

let rec pretty_func (args_input : arg list) (block : block)
    (elseblock_opt : elseblock option) : arg list * block * elseblock option =
  let args_input_pretty, block_pretty, elseblock_pretty_opt =
    (args_input, block, elseblock_opt)
    |> Pretty.Revive_underscore.apply_func |> Pretty.Rename_tick.apply_func
  in
  if
    Ol.Eq.eq_args args_input args_input_pretty
    && Ol.Eq.eq_block block block_pretty
    && Ol.Eq.eq_elseblock_opt elseblock_opt elseblock_pretty_opt
  then (args_input, block, elseblock_opt)
  else pretty_func args_input_pretty block_pretty elseblock_pretty_opt
