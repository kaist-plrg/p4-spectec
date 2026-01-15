open Ol.Ast

(* Prettify instructions *)

let pretty_rel (exps_match : exp list) (instrs : instr list) :
    exp list * instr list =
  (exps_match, instrs) |> Pretty.Revive_underscore.apply_rel
  |> Pretty.Rename_tick.apply_rel

let pretty_func (args_input : arg list) (instrs : instr list) :
    arg list * instr list =
  (args_input, instrs) |> Pretty.Revive_underscore.apply_func
  |> Pretty.Rename_tick.apply_func
