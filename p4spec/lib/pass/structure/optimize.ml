open Ol.Ast
open Runtime.Dynamic_Sl.Envs

(* Apply optimizations until it reaches a fixed point *)

let optimize_pre (block : block) : block =
  block |> Opt.Pre.Remove_alias_let.apply
  |> Opt.Pre.Matchify_if_eq_terminal.apply

let rec optimize_loop (tdenv : TDEnv.t) (block : block) : block =
  let block_optimized =
    block |> Opt.Loop.Merge_binding.apply
    |> Opt.Loop.Merge_if.apply tdenv
    |> Opt.Loop.Merge_hold.apply
    |> Opt.Loop.Casify.apply tdenv
  in
  if Ol.Eq.eq_block block block_optimized then block
  else optimize_loop tdenv block_optimized

let optimize_post (tdenv : TDEnv.t) (block : block) : block =
  block |> Opt.Post.Remove_dead_let.apply
  |> Opt.Post.Remove_singleton_match.apply tdenv

let optimize (tdenv : TDEnv.t) (block : block) : block =
  block |> optimize_pre |> optimize_loop tdenv |> optimize_post tdenv

let optimize_with_else (tdenv : TDEnv.t) (block : block)
    (elseblock_opt : elseblock option) : block * elseblock option =
  let block = optimize tdenv block in
  let elseblock_opt = Option.map (optimize tdenv) elseblock_opt in
  (block, elseblock_opt)

let optimize_without_else (tdenv : TDEnv.t) (block : block) : block =
  optimize tdenv block
