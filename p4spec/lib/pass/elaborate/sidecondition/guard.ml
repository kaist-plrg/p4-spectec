open Domain.Lib
open Lang
open Il
module Typ = Runtime.Type.Typ
open Error
open Util.Source

(* Insert explicit guard side conditions for:
    - array access a[n] to n < |a|
    - joint iteration e*{x <- x*, y <- y*, z <- z*} to (|x*| = |y*|) /\ (|y*| = |z*| *)

module Result = struct
  (* Premises that must hold, and premises that must be inserted *)

  type must = prem list
  type insert = prem list
  type t = prem list * prem list

  let default : t = ([], [])

  let compose (prems_must_a, prems_insert_a) (prems_must_b, prems_insert_b) : t
      =
    (prems_must_a @ prems_must_b, prems_insert_a @ prems_insert_b)

  let filter (prems_must : prem list) (prems_insert : prem list) : prem list =
    List.filter
      (fun prem_insert -> not (List.exists (Eq.eq_prem prem_insert) prems_must))
      prems_insert

  let iterate_prem (iterexp : iterexp) (prem : prem) : prem option =
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

  let iterate_must (iterexp : iterexp) (t : t) : t =
    let prems_must, prems_insert = t in
    let prems_must = prems_must |> List.filter_map (iterate_prem iterexp) in
    (prems_must, prems_insert)

  let iterate_insert (iterexp : iterexp) (t : t) : t =
    let prems_must, prems_insert = t in
    let prems_insert = prems_insert |> List.filter_map (iterate_prem iterexp) in
    (prems_must, prems_insert)
end

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
  | var_a :: var_b :: vars when iter = List ->
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
  | _ -> []

let collector : Result.t Walk.Collect.collector =
  let open Walk.Collect in
  let base = make_base ~default:Result.default ~compose:Result.compose in
  let collect_exp (c : Result.t collector) (exp : exp) : Result.t =
    match exp.it with
    | IdxE (exp_b, exp_i) ->
        let prems_insert = gen_index_guard exp exp_b exp_i in
        let result_b = c.collect_exp c exp_b in
        let result_i = c.collect_exp c exp_i in
        Result.compose ([], prems_insert) (Result.compose result_b result_i)
    | IterE (exp_inner, iterexp) ->
        let result = c.collect_exp c exp_inner in
        let result = Result.iterate_insert iterexp result in
        Result.compose result (c.collect_iterexp c iterexp)
    | _ -> default_collect_exp c exp
  in
  let collect_iterexp (_ : Result.t collector) (iterexp : iterexp) : Result.t =
    let prems_insert = gen_iterexp_guard iterexp in
    ([], prems_insert)
  in
  let collect_iterprem (_ : Result.t collector) (iterprem : iterprem) : Result.t
      =
    let iter, vars_in, vars_out = iterprem in
    let prems_must = gen_iterexp_guard (iter, vars_out) in
    let prems_insert = gen_iterexp_guard (iter, vars_in) in
    (prems_must, prems_insert)
  in
  let collect_prem (c : Result.t collector) (prem : prem) : Result.t =
    match prem.it with
    | LetPr (exp_l, exp_r) ->
        let result_exp_l = c.collect_exp c exp_l in
        let prems_must_l =
          let prems_must_l, prems_insert_l = result_exp_l in
          prems_must_l @ prems_insert_l
        in
        let result_exp_r = c.collect_exp c exp_r in
        let prems_insert_r =
          match result_exp_r with
          | [], prems_insert_r -> prems_insert_r
          | _ ->
              error exp_r.at
                "unexpected premises generated from let-binding right-hand side"
        in
        (prems_must_l, prems_insert_r)
    | IterPr (prem_inner, iterprem) ->
        let iter, vars_in, vars_out = iterprem in
        let result = c.collect_prem c prem_inner in
        let result = Result.iterate_must (iter, vars_out) result in
        let result = Result.iterate_insert (iter, vars_in) result in
        Result.compose result (c.collect_iterprem c iterprem)
    | _ -> default_collect_prem c prem
  in
  { base with collect_exp; collect_iterexp; collect_iterprem; collect_prem }

(* Entry point *)

let insert_exp_input (exp : exp) : Result.must =
  let prems_must, prems_insert = Walk.Collect.collect_exp collector exp in
  prems_must @ prems_insert

let insert_exps_input (exps : exp list) : Result.must =
  List.fold_left
    (fun prems_must exp -> prems_must @ insert_exp_input exp)
    [] exps

let insert_exp_output (prems_must_prev : prem list) (exp : exp) : Result.insert
    =
  let prems_must, prems_insert = Walk.Collect.collect_exp collector exp in
  match prems_must with
  | [] -> Result.filter prems_must_prev prems_insert
  | _ -> error exp.at "unexpected premises generated from an expression"

let insert_exps_output (prems_must_prev : prem list) (exps : exp list) :
    Result.insert =
  exps |> List.map (insert_exp_output prems_must_prev) |> List.flatten

let insert_arg_input (arg : arg) : Result.must =
  let prems_must, prems_insert = Walk.Collect.collect_arg collector arg in
  prems_must @ prems_insert

let insert_args_input (args : arg list) : Result.must =
  List.fold_left
    (fun prems_must arg -> prems_must @ insert_arg_input arg)
    [] args

let insert_prem (prems_must_prev : prem list) (prem : prem) : Result.t =
  let prems_must, prems_insert = Walk.Collect.collect_prem collector prem in
  let prems_insert = Result.filter prems_must_prev prems_insert in
  let prems_must = prems_must_prev @ prems_must in
  let prems_insert = prems_insert @ [ prem ] in
  (prems_must, prems_insert)

let insert_prems (prems_must_prev : prem list) (prems : prem list) : Result.t =
  List.fold_left
    (fun (prems_must_prev, prems_prev) prem ->
      let prems_must, prems = insert_prem prems_must_prev prem in
      (prems_must, prems_prev @ prems))
    (prems_must_prev, []) prems

let insert_rulegroup (rulegroup : rulegroup) : rulegroup =
  let id_rulegroup, rulematch, rulepaths = rulegroup.it in
  let prems_must, rulematch =
    let exps_signature, exps_input, prems = rulematch in
    let prems_must = insert_exps_input exps_input in
    let prems_must, prems = insert_prems prems_must prems in
    let rulematch = (exps_signature, exps_input, prems) in
    (prems_must, rulematch)
  in
  let rulepaths =
    List.map
      (fun (id_rulepath, prems, exps_output) ->
        let _, prems = insert_prems prems_must prems in
        let prems_insert_exps = insert_exps_output prems_must exps_output in
        (id_rulepath, prems @ prems_insert_exps, exps_output))
      rulepaths
  in
  (id_rulegroup, rulematch, rulepaths) $ rulegroup.at

let insert_elsegroup (elsegroup : elsegroup) : elsegroup =
  let id_rulegroup, rulematch, rulepath = elsegroup.it in
  let prems_must, rulematch =
    let exps_signature, exps_input, prems = rulematch in
    let prems_must = insert_exps_input exps_input in
    let prems_must, prems = insert_prems prems_must prems in
    let rulematch = (exps_signature, exps_input, prems) in
    (prems_must, rulematch)
  in
  let rulepath =
    let id_rulepath, prems, exps_output = rulepath in
    let _, prems = insert_prems prems_must prems in
    let prems_insert_exps = insert_exps_output prems_must exps_output in
    (id_rulepath, prems @ prems_insert_exps, exps_output)
  in
  (id_rulegroup, rulematch, rulepath) $ elsegroup.at

let insert_clause (clause : clause) : clause =
  let args, exp, prems = clause.it in
  let prems_must = insert_args_input args in
  let prems_must, prems = insert_prems prems_must prems in
  let prems_insert_exp = insert_exp_output prems_must exp in
  (args, exp, prems @ prems_insert_exp) $ clause.at
