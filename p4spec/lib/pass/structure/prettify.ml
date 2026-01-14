open Ol.Ast
open Runtime.Dynamic_Sl.Envs

(* Prettify instructions *)

let pretty_rel (ihenv : IHEnv.t) (exps_match : exp list) (instrs : instr list) :
    exp list * instr list =
  (exps_match, instrs)
  |> Pretty.Revive_underscore.apply_rel ihenv
  |> Pretty.Rename_tick.apply_rel ihenv

let pretty_func (ihenv : IHEnv.t) (args_input : arg list) (instrs : instr list)
    : arg list * instr list =
  (args_input, instrs)
  |> Pretty.Revive_underscore.apply_func ihenv
  |> Pretty.Rename_tick.apply_func ihenv
