open Ol.Ast
module Typ = Runtime.Dynamic_Sl.Typ
open Runtime.Dynamic_Sl.Envs

(* Apply optimizations until it reaches a fixed point *)

let optimize_pre (instrs : instr list) : instr list =
  instrs |> Opt.Remove_alias_let.apply |> Opt.Parallelize_if_disjunction.apply
  |> Opt.Matchify_if_eq_terminal.apply

let rec optimize_loop (ihenv : IHEnv.t) (tdenv : TDEnv.t) (instrs : instr list)
    : instr list =
  let instrs_optimized =
    instrs
    |> Opt.Remove_redundant_binding.apply ihenv
    |> Opt.Merge_if.apply tdenv |> Opt.Merge_hold.apply
    |> Opt.Casify.apply tdenv
    |> Opt.Remove_dead_let.apply ihenv
  in
  if Ol.Eq.eq_instrs instrs instrs_optimized then instrs
  else optimize_loop ihenv tdenv instrs_optimized

let optimize_post (tdenv : TDEnv.t) (instrs : instr list) : instr list =
  instrs |> Opt.Remove_singleton_match.apply tdenv

let optimize (ihenv : IHEnv.t) (tdenv : TDEnv.t) (instrs : instr list) :
    instr list =
  instrs |> optimize_pre |> optimize_loop ihenv tdenv |> optimize_post tdenv
