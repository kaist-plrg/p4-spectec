open Lang
open Il2
open Util.Source

type t = (iter * var list * var list) list

let empty : t = []

let iterate_prem (prem : prem) (iterctx : t) : prem =
  List.fold_left
    (fun prem (iter, vars_bound, vars_bind) ->
      Il.IterPr (prem, (iter, vars_bound, vars_bind)) $ prem.at)
    prem iterctx
