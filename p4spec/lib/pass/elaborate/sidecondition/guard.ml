open Domain.Lib
open Lang
open Il
module Typ = Runtime.Type.Typ
open Util.Source

(* Insert explicit guard side conditions for:
    - array access a[n] to n < |a|
    - joint iteration e*{x <- x*, y <- y*, z <- z*} to (|x*| = |y*|) /\ (|y*| = |z*| *)

let default = []
let compose = ( @ )

let iterate_wrap (iterexp : iterexp) (prem : prem) : prem option =
  let iter, vars = iterexp in
  let frees = Free.free_prem prem in
  let vars_iter =
    List.filter
      (fun (id, _, _) -> IdSet.find_opt id frees |> Option.is_some)
      vars
  in
  match vars_iter with
  | [] -> None
  | _ ->
      let iterprem = (iter, vars_iter, []) in
      Some (IterPr (prem, iterprem) $ prem.at)

let iterate_wraps (iterexp : iterexp) (prems : prem list) : prem list =
  prems
  |> List.map (fun prem -> iterate_wrap iterexp prem)
  |> List.filter_map Fun.id

let gen_index_guard (exp : exp) (exp_b : exp) (exp_i : exp) : prem list =
  let exp_l = LenE exp_b $$ (exp.at, Typ.Make.nat') in
  let exp_if = CmpE (`LtOp, `BoolT, exp_i, exp_l) $$ (exp.at, Typ.Make.bool') in
  [ IfPr exp_if $ exp.at ]

let gen_iterexp_guard (iterexp : iterexp) : prem list =
  let iterate_exp (id : id) (typ : typ) (iters : iter list) : exp =
    let rec iterate_exp' (id : id) (typ : typ) (iters : iter list) : exp =
      match iters with
      | [] -> VarE id $$ (id.at, typ.it)
      | iter_h :: iters_t ->
          let exp_t = iterate_exp' id typ iters_t in
          let iterexp_h =
            let var = (id, typ, iters_t) in
            (iter_h, [ var ])
          in
          let typ = Typ.Make.iterate typ iters in
          IterE (exp_t, iterexp_h) $$ (id.at, typ.it)
    in
    iterate_exp' id typ (List.rev iters)
  in
  let iter, vars = iterexp in
  match vars with
  | [] | [ _ ] -> default
  | var_a :: var_b :: vars ->
      let exp_a =
        let id_a, typ_a, iters_a = var_a in
        let exp_a = iterate_exp id_a typ_a (iters_a @ [ iter ]) in
        LenE exp_a $$ (exp_a.at, Typ.Make.nat')
      in
      let exp_b =
        let id_b, typ_b, iters_b = var_b in
        let exp_b = iterate_exp id_b typ_b (iters_b @ [ iter ]) in
        LenE exp_b $$ (exp_b.at, Typ.Make.nat')
      in
      let exp_if =
        CmpE (`EqOp, `BoolT, exp_a, exp_b) $$ (exp_a.at, Typ.Make.bool')
      in
      let _, exp_if =
        List.fold_left
          (fun (exp_prev, exp_if) var ->
            let exp =
              let id, typ, iters = var in
              let exp = iterate_exp id typ (iters @ [ iter ]) in
              LenE exp $$ (exp.at, Typ.Make.nat')
            in
            let exp_cmp =
              CmpE (`EqOp, `BoolT, exp_prev, exp)
              $$ (exp_prev.at, Typ.Make.bool')
            in
            let exp_if =
              BinE (`AndOp, `BoolT, exp_if, exp_cmp)
              $$ (exp_if.at, Typ.Make.bool')
            in
            (exp, exp_if))
          (exp_b, exp_if) vars
      in
      [ IfPr exp_if $ no_region ]

let collector =
  let open Walk.Collect in
  let base = make_base ~default ~compose in
  let collect_exp (c : prem list collector) (exp : exp) : prem list =
    match exp.it with
    | IdxE (exp_b, exp_i) ->
        gen_index_guard exp exp_b exp_i @ default_collect_exp c exp
    | IterE (exp_inner, iterexp) ->
        let children = c.collect_exp c exp_inner in
        let children = iterate_wraps iterexp children in
        compose children (c.collect_iterexp c iterexp)
    | _ -> default_collect_exp c exp
  in
  let collect_iterexp (_c : prem list collector) (iterexp : iterexp) : prem list
      =
    gen_iterexp_guard iterexp
  in
  let collect_iterprem (_c : prem list collector) (iterprem : iterprem) :
      prem list =
    let iter, vars_in, _ = iterprem in
    gen_iterexp_guard (iter, vars_in)
  in
  let collect_prem (c : prem list collector) (prem : prem) : prem list =
    match prem.it with
    | LetPr (_exp_l, exp_r) -> c.collect_exp c exp_r
    | IterPr (inner_prem, iterprem) ->
        let children = c.collect_prem c inner_prem in
        let children =
          let iter, vars_in, _ = iterprem in
          iterate_wraps (iter, vars_in) children
        in
        compose children (c.collect_iterprem c iterprem)
    | _ -> default_collect_prem c prem
  in
  { base with collect_exp; collect_iterexp; collect_iterprem; collect_prem }

(* Entry point *)

let insert_exp (exp : exp) : prem list = Walk.Collect.collect_exp collector exp

let insert_exps (exps : exp list) : prem list =
  exps |> List.map insert_exp |> List.flatten

let insert_prem (prem : prem) : prem list =
  Walk.Collect.collect_prem collector prem

let insert_prems (prems : prem list) : prem list =
  prems |> List.concat_map (fun prem -> insert_prem prem @ [ prem ])

let insert_rulegroup (rulegroup : rulegroup) : rulegroup =
  let id_rulegroup, rulematch, rulepaths = rulegroup.it in
  let rulematch =
    let exps_signature, exps_input, prems = rulematch in
    let prems = insert_prems prems in
    (exps_signature, exps_input, prems)
  in
  let rulepaths =
    List.map
      (fun (id_rulepath, prems, exps_output) ->
        let prems = insert_prems prems in
        let prems_exps = insert_exps exps_output in
        (id_rulepath, prems @ prems_exps, exps_output))
      rulepaths
  in
  (id_rulegroup, rulematch, rulepaths) $ rulegroup.at

let insert_elsegroup (elsegroup : elsegroup) : elsegroup =
  let id_rulegroup, rulematch, rulepath = elsegroup.it in
  let rulematch =
    let exps_signature, exps_input, prems = rulematch in
    let prems = insert_prems prems in
    (exps_signature, exps_input, prems)
  in
  let rulepath =
    let id_rulepath, prems, exps_output = rulepath in
    let prems = insert_prems prems in
    let prems_exps = insert_exps exps_output in
    (id_rulepath, prems @ prems_exps, exps_output)
  in
  (id_rulegroup, rulematch, rulepath) $ elsegroup.at

let insert_clause (clause : clause) : clause =
  let args, exp, prems = clause.it in
  let prems = insert_prems prems in
  let prems_exp = insert_exp exp in
  (args, exp, prems @ prems_exp) $ clause.at
