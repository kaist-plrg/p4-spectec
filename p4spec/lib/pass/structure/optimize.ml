open Ol.Ast
open Runtime.Dynamic_Sl.Envs

(* Apply optimizations until it reaches a fixed point *)

let optimize_pre ~(final : bool) (block : block) : block =
  if final then
    block |> Opt.Pre.Remove_group.apply |> Opt.Pre.Remove_let_alias.apply
    |> Opt.Pre.Matchify_if_eq_terminal.apply
  else
    block |> Opt.Pre.Remove_let_alias.apply
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
  block |> Opt.Post.Remove_let_dead.apply
  |> Opt.Post.Remove_match_singleton.apply tdenv

let optimize ~(final : bool) (tdenv : TDEnv.t) (block : block) : block =
  block |> optimize_pre ~final |> optimize_loop tdenv |> optimize_post tdenv

let optimize_with_else ~(final : bool) (tdenv : TDEnv.t) (block : block)
    (elseblock_opt : elseblock option) : block * elseblock option =
  let block = optimize ~final tdenv block in
  let elseblock_opt = Option.map (optimize tdenv ~final) elseblock_opt in
  (block, elseblock_opt)

let optimize_without_else ~(final : bool) (tdenv : TDEnv.t) (block : block) :
    block =
  optimize ~final tdenv block
