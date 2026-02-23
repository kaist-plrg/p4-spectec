open Ol.Ast

(* Prettify instructions *)

let pretty_rel (exps_match : exp list) (block : block)
    (elseblock_opt : elseblock option) : exp list * block * elseblock option =
  (exps_match, block, elseblock_opt)
  |> Pretty.Revive_underscore.apply_rel |> Pretty.Rename_tick.apply_rel

let pretty_func (args_input : arg list) (block : block)
    (elseblock_opt : elseblock option) : arg list * block * elseblock option =
  (args_input, block, elseblock_opt)
  |> Pretty.Revive_underscore.apply_func |> Pretty.Rename_tick.apply_func
