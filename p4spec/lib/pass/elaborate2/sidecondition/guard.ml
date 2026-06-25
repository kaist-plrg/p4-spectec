open Domain.Lib
open Lang
open Il
module Typ = Runtime.Type.Typ
open Error
open Util.Source

(* Insert explicit guard side conditions for:

    - array access a[n] to n < |a|
    - joint iteration e*{x <- x*, y <- y*, z <- z*} to (|x*| = |y*|) /\ (|y*| = |z*|
    - joint iteration e?{x <- x?, y <- y?} to (x? = eps) <=> (y? = eps)

   Must-premises are generated from the binding sites, e.g.

    - (let (x, y) = z){x -> x*, y -> y*, z <- z*} generates must-premises (|x*| = |y*|) /\ (|y*| = |z*|);
    - this optimizes away redundant guard conditions *)

(* An atomic condition appearing in an if-premise *)

module Cond = struct
  type t = exp

  let eq = Eq.eq_exp
end

(* An equivalence class of conditions *)

module Cls = struct
  type t = Equals of Cond.t list | Equiv of Cond.t list | Singleton of Cond.t
end

(* Equivalence table for equality filtering,
   so that transitively-derivable guards can be filtered out. *)

module Equiv = struct
  (* A flat list of equivalence classes *)
  type table = Cls.t list

  (* Merge the classes of cond_a and cond_b within *)

  let union_kind (extract : Cls.t -> Cond.t list option)
      (wrap : Cond.t list -> Cls.t) (tbl : table) (cond_a : Cond.t)
      (cond_b : Cond.t) : table =
    let find cond =
      List.find_map
        (fun cls ->
          match extract cls with
          | Some cs when List.exists (Cond.eq cond) cs -> Some cs
          | _ -> None)
        tbl
    in
    let drop cs =
      List.filter (fun cls ->
          match extract cls with Some cs' -> cs' != cs | None -> true)
    in
    match (find cond_a, find cond_b) with
    | Some cs_a, Some cs_b when cs_a == cs_b -> tbl
    | Some cs_a, Some cs_b ->
        wrap (cs_a @ cs_b) :: (tbl |> drop cs_a |> drop cs_b)
    | Some cs_a, None -> wrap (cond_b :: cs_a) :: drop cs_a tbl
    | None, Some cs_b -> wrap (cond_a :: cs_b) :: drop cs_b tbl
    | None, None -> wrap [ cond_a; cond_b ] :: tbl

  let union_eq =
    union_kind
      (function Cls.Equals cs -> Some cs | _ -> None)
      (fun cs -> Cls.Equals cs)

  let union_equiv =
    union_kind
      (function Cls.Equiv cs -> Some cs | _ -> None)
      (fun cs -> Cls.Equiv cs)

  (* Update the table with the constraints from an if-expression *)

  let rec of_if_exp (tbl : table) (exp : exp) : table =
    match exp.it with
    | CmpE (`EqOp, _, exp_l, exp_r) -> union_eq tbl exp_l exp_r
    | BinE (`EquivOp, _, exp_l, exp_r) -> union_equiv tbl exp_l exp_r
    | BinE (`AndOp, _, exp_l, exp_r) -> of_if_exp (of_if_exp tbl exp_l) exp_r
    | _ -> Cls.Singleton exp :: tbl

  (* Build a table from a list of must-premises. *)

  let of_prems (prems : prem list) : table =
    List.fold_left
      (fun tbl prem ->
        match prem.it with IfPr exp -> of_if_exp tbl exp | _ -> tbl)
      [] prems

  (* True iff cond_a and cond_b belong to the same class of the given kind. *)

  let mem_kind (extract : Cls.t -> Cond.t list option) (tbl : table)
      (cond_a : Cond.t) (cond_b : Cond.t) : bool =
    Cond.eq cond_a cond_b
    || List.find_map
         (fun cls ->
           match extract cls with
           | Some cs when List.exists (Cond.eq cond_a) cs -> Some cs
           | _ -> None)
         tbl
       |> Option.fold ~none:false ~some:(List.exists (Cond.eq cond_b))

  let mem_eq = mem_kind (function Cls.Equals conds -> Some conds | _ -> None)

  let mem_equiv =
    mem_kind (function Cls.Equiv conds -> Some conds | _ -> None)

  (* True iff exp/prem is already entailed by the table *)

  let rec implies_exp (tbl : table) (exp : exp) : bool =
    match exp.it with
    | CmpE (`EqOp, _, exp_l, exp_r) -> mem_eq tbl exp_l exp_r
    | BinE (`EquivOp, _, exp_l, exp_r) -> mem_equiv tbl exp_l exp_r
    | BinE (`AndOp, _, exp_l, exp_r) ->
        implies_exp tbl exp_l && implies_exp tbl exp_r
    | _ ->
        List.exists
          (function Cls.Singleton cond -> Cond.eq exp cond | _ -> false)
          tbl

  let implies (tbl : table) (prem : prem) : bool =
    match prem.it with IfPr exp -> implies_exp tbl exp | _ -> false
end

(* Result of collecting must-premises and insert-premises from an expression or premise *)

module Result = struct
  type must = prem list
  type insert = prem list
  type t = prem list * prem list

  let default : t = ([], [])

  let compose (must_a, insert_a) (must_b, insert_b) : t =
    (must_a @ must_b, insert_a @ insert_b)

  let filter (must : must) (insert : insert) : insert =
    let equiv = Equiv.of_prems must in
    List.filter
      (fun prem ->
        (not (Equiv.implies equiv prem))
        && not (List.exists (Eq.eq_prem prem) must))
      insert

  let lift (at : region) ((must, inserts) : t) : prem list =
    match must with
    | [] -> inserts
    | _ -> error at "should not produce must-premises"

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

  let iterate (iterexp_must : iterexp) (iterexp_insert : iterexp)
      ((must, insert) : t) : t =
    ( List.filter_map (iterate_prem iterexp_must) must,
      List.filter_map (iterate_prem iterexp_insert) insert )
end

let gen_index_guard (exp : exp) (exp_b : exp) (exp_i : exp) : prem list =
  let exp_l = LenE exp_b $$ (exp.at, Typ.Make.nat') in
  let exp_if = CmpE (`LtOp, `BoolT, exp_i, exp_l) $$ (exp.at, Typ.Make.bool') in
  [ IfPr exp_if $ exp.at ]

let gen_eq_epsilon_exp (iter : iter) (var : var) : exp =
  let id, typ, iters = var in
  let exp = Var.as_exp ~dim:true (id, typ, iters @ [ iter ]) in
  let exp_epsilon = OptE None $$ (exp.at, exp.note) in
  CmpE (`EqOp, `BoolT, exp, exp_epsilon) $$ (exp.at, Typ.Make.bool')

let gen_len_exp (iter : iter) (var : var) : exp =
  let id, typ, iters = var in
  let exp = Var.as_exp ~dim:true (id, typ, iters @ [ iter ]) in
  LenE exp $$ (exp.at, Typ.Make.nat')

let gen_iter_guard (iterexp : iterexp) : prem list =
  let iter, vars = iterexp in
  match vars with
  | var_a :: var_b :: vars when iter = Opt ->
      let exp_a = gen_eq_epsilon_exp iter var_a in
      let exp_b = gen_eq_epsilon_exp iter var_b in
      let exp_if =
        BinE (`EquivOp, `BoolT, exp_a, exp_b) $$ (exp_a.at, Typ.Make.bool')
      in
      let _, exp_if =
        List.fold_left
          (fun (exp_prev, exp_if) var ->
            let exp = gen_eq_epsilon_exp iter var in
            let exp_bin =
              BinE (`EquivOp, `BoolT, exp_prev, exp)
              $$ (exp_prev.at, Typ.Make.bool')
            in
            let exp_if =
              BinE (`AndOp, `BoolT, exp_if, exp_bin)
              $$ (exp_if.at, Typ.Make.bool')
            in
            (exp, exp_if))
          (exp_b, exp_if) vars
      in
      [ IfPr exp_if $ no_region ]
  | var_a :: var_b :: vars when iter = List ->
      let exp_a = gen_len_exp iter var_a in
      let exp_b = gen_len_exp iter var_b in
      let exp_if =
        CmpE (`EqOp, `BoolT, exp_a, exp_b) $$ (exp_a.at, Typ.Make.bool')
      in
      let _, exp_if =
        List.fold_left
          (fun (exp_prev, exp_if) var ->
            let exp = gen_len_exp iter var in
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
        let iter, vars = iterexp in
        let result = c.collect_exp c exp_inner in
        let result = Result.iterate (iter, []) (iter, vars) result in
        Result.compose result (c.collect_iterexp c iterexp)
    | _ -> default_collect_exp c exp
  in
  let collect_iterexp (_ : Result.t collector) (iterexp : iterexp) : Result.t =
    let prems_insert = gen_iter_guard iterexp in
    ([], prems_insert)
  in
  let collect_iterprem (_ : Result.t collector) (iterprem : iterprem) : Result.t
      =
    let iter, vars_in, vars_out = iterprem in
    let prems_must = gen_iter_guard (iter, vars_in @ vars_out) in
    let prems_insert = gen_iter_guard (iter, vars_in) in
    (prems_must, prems_insert)
  in
  let collect_prem (c : Result.t collector) (prem : prem) : Result.t =
    match prem.it with
    | LetPr (exp_l, exp_r) ->
        let prems_must_l = exp_l |> c.collect_exp c |> Result.lift exp_l.at in
        let prems_insert_r = exp_r |> c.collect_exp c |> Result.lift exp_r.at in
        (prems_must_l, prems_insert_r)
    | IterPr (prem_inner, iterprem) ->
        let iter, vars_in, vars_out = iterprem in
        let result = c.collect_prem c prem_inner in
        let result =
          Result.iterate (iter, vars_in @ vars_out) (iter, vars_in) result
        in
        Result.compose result (c.collect_iterprem c iterprem)
    | _ -> default_collect_prem c prem
  in
  { base with collect_exp; collect_iterexp; collect_iterprem; collect_prem }

(* Entry point *)

let must_exp_input (exp : exp) : Result.must =
  Result.lift exp.at (Walk.Collect.collect_exp collector exp)

let must_exps_input (exps : exp list) : Result.must =
  List.fold_left (fun prems_must exp -> prems_must @ must_exp_input exp) [] exps

let insert_exp_output (prems_must : prem list) (exp : exp) : Result.insert =
  Result.lift exp.at (Walk.Collect.collect_exp collector exp)
  |> Result.filter prems_must

let insert_exps_output (prems_must : prem list) (exps : exp list) :
    Result.insert =
  exps |> List.map (insert_exp_output prems_must) |> List.flatten

let must_arg_input (arg : arg) : Result.must =
  Result.lift arg.at (Walk.Collect.collect_arg collector arg)

let must_args_input (args : arg list) : Result.must =
  List.fold_left (fun prems_must arg -> prems_must @ must_arg_input arg) [] args

let insert_prem (prems_must_prev : prem list) (prem : prem) : Result.t =
  let prems_must, prems_insert = Walk.Collect.collect_prem collector prem in
  let prems_insert = Result.filter prems_must_prev prems_insert in
  let prems_insert = prems_insert @ [ prem ] in
  let prems_must = prems_must_prev @ prems_must @ prems_insert in
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
    let prems_must = must_exps_input exps_input in
    let prems_must, prems = insert_prems prems_must prems in
    (prems_must, (exps_signature, exps_input, prems))
  in
  let rulepaths =
    List.map
      (fun (id_rulepath, prems, exps_output) ->
        let prems_must, prems = insert_prems prems_must prems in
        let prems_output = insert_exps_output prems_must exps_output in
        (id_rulepath, prems @ prems_output, exps_output))
      rulepaths
  in
  (id_rulegroup, rulematch, rulepaths) $ rulegroup.at

let insert_elsegroup (elsegroup : elsegroup) : elsegroup =
  let id_rulegroup, rulematch, rulepath = elsegroup.it in
  let prems_must, rulematch =
    let exps_signature, exps_input, prems = rulematch in
    let prems_must = must_exps_input exps_input in
    let prems_must, prems = insert_prems prems_must prems in
    (prems_must, (exps_signature, exps_input, prems))
  in
  let rulepath =
    let id_rulepath, prems, exps_output = rulepath in
    let prems_must, prems = insert_prems prems_must prems in
    let prems_output = insert_exps_output prems_must exps_output in
    (id_rulepath, prems @ prems_output, exps_output)
  in
  (id_rulegroup, rulematch, rulepath) $ elsegroup.at

let insert_clause (clause : clause) : clause =
  let args, exp, prems = clause.it in
  let prems_must = must_args_input args in
  let prems_must, prems = insert_prems prems_must prems in
  let prems_exp = insert_exp_output prems_must exp in
  (args, exp, prems @ prems_exp) $ clause.at

let insert_def (def : def) : def =
  let at = def.at in
  match def.it with
  | RelD (id, nottyp, inputs, rulegroups, elsegroup_opt, hints) ->
      let rulegroups = List.map insert_rulegroup rulegroups in
      let elsegroup_opt = Option.map insert_elsegroup elsegroup_opt in
      RelD (id, nottyp, inputs, rulegroups, elsegroup_opt, hints) $ at
  | FuncDecD (id, tparams, params, typ, clauses, elseclause_opt, hints) ->
      let clauses = List.map insert_clause clauses in
      let elseclause_opt = Option.map insert_clause elseclause_opt in
      FuncDecD (id, tparams, params, typ, clauses, elseclause_opt, hints) $ at
  | _ -> def

let insert_spec (spec : spec) : spec = List.map insert_def spec
