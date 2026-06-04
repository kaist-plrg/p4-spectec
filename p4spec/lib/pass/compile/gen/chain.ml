(* Continuation representation of a single-threaded OCaml block *)

type t = Ml.expr -> Ml.expr

let nop : t = fun expr_ml -> expr_ml

let make_let (pat_ml : Ml.pat) (expr_ml : Ml.expr) : t =
 fun (expr_body_ml : Ml.expr) -> Ml.LetE (pat_ml, expr_ml, expr_body_ml)

let make_match (expr_scrut_ml : Ml.expr) (pat_then_ml : Ml.pat) : t =
 fun (expr_body_ml : Ml.expr) ->
  let arm_then_ml = (pat_then_ml, expr_body_ml) in
  let arm_else_ml =
    let pat_else_ml = Ml.WildP in
    let expr_else_ml = Common.raise_unmatch "binding pattern failed" in
    (pat_else_ml, expr_else_ml)
  in
  Ml.MatchE (expr_scrut_ml, [ arm_then_ml; arm_else_ml ])

let make_option_map (expr_map_ml : Ml.expr) (id_iter_ml : Ml.id) : t =
 fun (expr_body_ml : Ml.expr) ->
  let expr_fun_ml = Ml.FunE ([ Ml.VarP id_iter_ml ], expr_body_ml) in
  Ml.AppE (Ml.VarE "Option.map", [ expr_fun_ml; expr_map_ml ])

let make_list_map (expr_map_ml : Ml.expr) (id_iter_ml : Ml.id) : t =
 fun (expr_body_ml : Ml.expr) ->
  let expr_fun_ml = Ml.FunE ([ Ml.VarP id_iter_ml ], expr_body_ml) in
  Ml.AppE (Ml.VarE "List.map", [ expr_fun_ml; expr_map_ml ])

let connect (chains : t list) : t =
  List.fold_right
    (fun chain chain_acc (expr : Ml.expr) -> expr |> chain_acc |> chain)
    chains
    (fun expr -> expr)

let apply (chain : t) (expr : Ml.expr) : Ml.expr = chain expr
