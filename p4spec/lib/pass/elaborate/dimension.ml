open Domain.Lib
open Lang
open Il
module Mixfix = Domain.Mixfix
open Runtime.Static
open Error
open Envs
open Util.Source

(* Dimension analysis :

   For each rule or clause, collect the dimension of all occurrences of
   every identifier. The minimal dimension is determined to be the ambient
   dimension of the identifier in the rule or clause.
    - e.g. -- if n_x* = [ 1, 1 ] ;; n_x : [ n_x* ]
           -- if n_y = 1         ;; n_y : [ n_y ]
           -- if (n_x = n_y)*    ;; n_x : [ n_x* ], n_y : [ n_y* ]
    - Overall, n_x : [ n_x* ] and n_y : [ n_y, n_y* ]
    - Therefore, n_x : n_x* and n_y : n_y

   Annotate iteration constructs with what variables are iterated over.
    - Variables with iterated dimensions at most the ambient dimension
    - Check that iteration is non-empty
    - e.g. -- if n_x*{n_x <- n_x*} = [ 1, 1 ]
           -- if n_y = 1
           -- if (n_x = n_y)*{n_x <- n_x*} *)

(* Context for dimension analysis *)

module Dimctx = struct
  include MakeIdEnv (struct
    type t = Typdim.t list

    let to_string typ =
      "[" ^ String.concat ", " (List.map Typdim.to_string typ) ^ "]"
  end)

  let infer (dctx : t) : VEnv.t =
    mapi
      (fun id typs ->
        let typs_sorted = List.sort Typdim.compare typs in
        let typ_min = List.hd typs_sorted in
        List.iter
          (fun typ ->
            check (Typdim.sub typ_min typ) id.at
              (Format.sprintf
                 "mismatched iteration dimensions for identifier `%s`: %s vs %s"
                 (Id.to_string id) (Typdim.to_string typ_min)
                 (Typdim.to_string typ)))
          (List.tl typs_sorted);
        typ_min)
      dctx
end

(* Expression inference *)

let rec infer_exp (dctx : Dimctx.t) (exp : exp) (iters : iter list) : Dimctx.t =
  let typ = exp.note $ exp.at in
  match exp.it with
  | BoolE _ | NumE _ | TextE _ -> dctx
  | VarE id -> Dimctx.add_to_list id (typ, iters) dctx
  | UnE (_, _, exp)
  | UpCastE (_, exp)
  | DownCastE (_, exp)
  | SubE (exp, _, _)
  | MatchE (exp, _)
  | LenE exp
  | DotE (exp, _) ->
      infer_exp dctx exp iters
  | BinE (_, _, exp_l, exp_r)
  | CmpE (_, _, exp_l, exp_r)
  | ConsE (exp_l, exp_r)
  | CatE (exp_l, exp_r)
  | MemE (exp_l, exp_r)
  | IdxE (exp_l, exp_r) ->
      let dctx = infer_exp dctx exp_l iters in
      infer_exp dctx exp_r iters
  | TupleE exps | ListE exps -> infer_exps dctx exps iters
  | CaseE notexp -> infer_notexp dctx notexp iters
  | StrE expfields ->
      let exps = List.map snd expfields in
      infer_exps dctx exps iters
  | OptE (Some exp) -> infer_exp dctx exp iters
  | OptE None -> dctx
  | SliceE (exp_b, exp_l, exp_h) ->
      let dctx = infer_exp dctx exp_b iters in
      let dctx = infer_exp dctx exp_l iters in
      infer_exp dctx exp_h iters
  | UpdE (exp_b, path, exp_f) ->
      let dctx = infer_exp dctx exp_b iters in
      let dctx = infer_path dctx path iters in
      infer_exp dctx exp_f iters
  | CallE (_, _, args) -> infer_args dctx args iters
  | IterE (exp, (iter, _)) -> infer_exp dctx exp (iter :: iters)

and infer_exps (dctx : Dimctx.t) (exps : exp list) (iters : iter list) :
    Dimctx.t =
  List.fold_left (fun dctx exp -> infer_exp dctx exp iters) dctx exps

and infer_notexp (dctx : Dimctx.t) (notexp : notexp) (iters : iter list) :
    Dimctx.t =
  let exps = Mixfix.args notexp in
  infer_exps dctx exps iters

(* Path inference *)

and infer_path (dctx : Dimctx.t) (path : path) (iters : iter list) : Dimctx.t =
  match path.it with
  | RootP -> dctx
  | IdxP (path, exp) ->
      let dctx = infer_path dctx path iters in
      infer_exp dctx exp iters
  | SliceP (path, exp_l, exp_h) ->
      let dctx = infer_path dctx path iters in
      let dctx = infer_exp dctx exp_l iters in
      infer_exp dctx exp_h iters
  | DotP (path, _) -> infer_path dctx path iters

(* Argument inference *)

and infer_arg (dctx : Dimctx.t) (arg : arg) (iters : iter list) : Dimctx.t =
  match arg.it with ExpA exp -> infer_exp dctx exp iters | DefA _ -> dctx

and infer_args (dctx : Dimctx.t) (args : arg list) (iters : iter list) :
    Dimctx.t =
  List.fold_left (fun dctx arg -> infer_arg dctx arg iters) dctx args

(* Premise inference *)

let rec infer_prem (dctx : Dimctx.t) (prem : prem) (iters : iter list) :
    Dimctx.t =
  match prem.it with
  | RulePr (_, notexp, _) -> infer_notexp dctx notexp iters
  | IfPr exp -> infer_exp dctx exp iters
  | IfHoldPr (_, notexp) -> infer_notexp dctx notexp iters
  | IfNotHoldPr (_, notexp) -> infer_notexp dctx notexp iters
  | LetPr _ ->
      error prem.at "let premise should appear only after the algo pass"
  | IterPr (_, ((_, vars_bound, vars_bind) as iterprem))
    when (not (List.is_empty vars_bound)) || not (List.is_empty vars_bind) ->
      error prem.at
        (Format.asprintf
           "iterated premise should initially have no annotations, but got %s"
           (Il.Print.string_of_iterprem iterprem))
  | IterPr (prem, (iter, _, _)) -> infer_prem dctx prem (iter :: iters)
  | DebugPr exp -> infer_exp dctx exp iters

let infer_prems (dctx : Dimctx.t) (prems : prem list) : Dimctx.t =
  List.fold_left (fun dctx prem -> infer_prem dctx prem []) dctx prems

(* Rule inference *)

let infer_rule (rule : rule) : Dimctx.t =
  let _, notexp, prems = rule.it in
  let dctx = infer_notexp Dimctx.empty notexp [] in
  infer_prems dctx prems

(* Clause inference *)

let infer_clause (clause : clause) : Dimctx.t =
  let args, exp, prems = clause.it in
  let dctx = infer_args Dimctx.empty args [] in
  let dctx = infer_prems dctx prems in
  infer_exp dctx exp []

(* Table row inference *)

let infer_tablerow (tablerow : tablerow) : Dimctx.t =
  let args, exp = tablerow.it in
  let dctx = infer_args Dimctx.empty args [] in
  infer_exp dctx exp []

(* Occurence constructors *)

let empty = VEnv.empty
let singleton id typ = VEnv.add id (typ, []) empty

let union (occurs_a : VEnv.t) (occurs_b : VEnv.t) : VEnv.t =
  VEnv.union
    (fun id (typ_a, iters_a) (typ_b, iters_b) ->
      if not (Eq.eq_typ typ_a typ_b) then
        error id.at
          (Format.asprintf
             "type mismatch for identifier `%s` in union: %s vs %s"
             (Id.to_string id)
             (Print.string_of_typ typ_a)
             (Print.string_of_typ typ_b));
      if List.length iters_a < List.length iters_b then Some (typ_a, iters_a)
      else Some (typ_b, iters_b))
    occurs_a occurs_b

let iterate (occurs : VEnv.t) (itervars : (id * typ * iter list) list)
    (iter : iter) : VEnv.t =
  List.fold_left
    (fun occurs (id, typ, iters) -> VEnv.add id (typ, iters @ [ iter ]) occurs)
    occurs itervars

let collect_itervars (bounds : VEnv.t) (occurs : VEnv.t) (iter : iter) :
    (id * typ * iter list) list =
  occurs |> VEnv.bindings
  |> List.filter_map (fun (id, typ) ->
         let typ_expect = VEnv.find id bounds in
         if Typdim.sub (Typdim.add_iter iter typ) typ_expect then
           let typ, iters = typ in
           Some (id, typ, iters)
         else None)

(* Expression annotation *)

let rec annotate_exp (bounds : VEnv.t) (exp : exp) : VEnv.t * exp =
  let at, note = (exp.at, exp.note) in
  match exp.it with
  | BoolE _ | NumE _ | TextE _ -> (empty, exp)
  | VarE id -> (singleton id (note $ at), exp)
  | UnE (op, optyp, exp) ->
      let occurs, exp = annotate_exp bounds exp in
      let exp = UnE (op, optyp, exp) $$ (at, note) in
      (occurs, exp)
  | BinE (op, bintyp, exp_l, exp_r) ->
      let occurs_l, exp_l = annotate_exp bounds exp_l in
      let occurs_r, exp_r = annotate_exp bounds exp_r in
      let exp = BinE (op, bintyp, exp_l, exp_r) $$ (at, note) in
      let occurs = union occurs_l occurs_r in
      (occurs, exp)
  | CmpE (op, cmptyp, exp_l, exp_r) ->
      let occurs_l, exp_l = annotate_exp bounds exp_l in
      let occurs_r, exp_r = annotate_exp bounds exp_r in
      let exp = CmpE (op, cmptyp, exp_l, exp_r) $$ (at, note) in
      let occurs = union occurs_l occurs_r in
      (occurs, exp)
  | UpCastE (typ, exp) ->
      let occurs, exp = annotate_exp bounds exp in
      let exp = UpCastE (typ, exp) $$ (at, note) in
      (occurs, exp)
  | DownCastE (typ, exp) ->
      let occurs, exp = annotate_exp bounds exp in
      let exp = DownCastE (typ, exp) $$ (at, note) in
      (occurs, exp)
  | MatchE (exp, pattern) ->
      let occurs, exp = annotate_exp bounds exp in
      let exp = MatchE (exp, pattern) $$ (at, note) in
      (occurs, exp)
  | SubE (exp, typ, subcheck) ->
      let occurs, exp = annotate_exp bounds exp in
      let exp = SubE (exp, typ, subcheck) $$ (at, note) in
      (occurs, exp)
  | TupleE exps ->
      let occurs, exps = annotate_exps bounds exps in
      let exp = TupleE exps $$ (at, note) in
      (occurs, exp)
  | CaseE notexp ->
      let occurs, notexp = annotate_notexp bounds notexp in
      let exp = CaseE notexp $$ (at, note) in
      (occurs, exp)
  | StrE expfields ->
      let atoms, exps = List.split expfields in
      let occurs, exps = annotate_exps bounds exps in
      let expfields = List.combine atoms exps in
      let exp = StrE expfields $$ (at, note) in
      (occurs, exp)
  | OptE (Some exp) ->
      let occurs, exp = annotate_exp bounds exp in
      let exp_opt = Some exp in
      let exp = OptE exp_opt $$ (at, note) in
      (occurs, exp)
  | OptE None -> (empty, exp)
  | ListE exps ->
      let occurs, exps = annotate_exps bounds exps in
      let exp = ListE exps $$ (at, note) in
      (occurs, exp)
  | ConsE (exp_l, exp_r) ->
      let occurs_l, exp_l = annotate_exp bounds exp_l in
      let occurs_r, exp_r = annotate_exp bounds exp_r in
      let exp = ConsE (exp_l, exp_r) $$ (at, note) in
      let occurs = union occurs_l occurs_r in
      (occurs, exp)
  | CatE (exp_l, exp_r) ->
      let occurs_l, exp_l = annotate_exp bounds exp_l in
      let occurs_r, exp_r = annotate_exp bounds exp_r in
      let exp = CatE (exp_l, exp_r) $$ (at, note) in
      let occurs = union occurs_l occurs_r in
      (occurs, exp)
  | MemE (exp_l, exp_r) ->
      let occurs_l, exp_l = annotate_exp bounds exp_l in
      let occurs_r, exp_r = annotate_exp bounds exp_r in
      let exp = MemE (exp_l, exp_r) $$ (at, note) in
      let occurs = union occurs_l occurs_r in
      (occurs, exp)
  | LenE exp ->
      let occurs, exp = annotate_exp bounds exp in
      let exp = LenE exp $$ (at, note) in
      (occurs, exp)
  | DotE (exp, atom) ->
      let occurs, exp = annotate_exp bounds exp in
      let exp = DotE (exp, atom) $$ (at, note) in
      (occurs, exp)
  | IdxE (exp_b, exp_i) ->
      let occurs_b, exp_b = annotate_exp bounds exp_b in
      let occurs_i, exp_i = annotate_exp bounds exp_i in
      let exp = IdxE (exp_b, exp_i) $$ (at, note) in
      let occurs = union occurs_b occurs_i in
      (occurs, exp)
  | SliceE (exp_b, exp_l, exp_h) ->
      let occurs_b, exp_b = annotate_exp bounds exp_b in
      let occurs_l, exp_l = annotate_exp bounds exp_l in
      let occurs_h, exp_h = annotate_exp bounds exp_h in
      let exp = SliceE (exp_b, exp_l, exp_h) $$ (at, note) in
      let occurs = union (union occurs_b occurs_l) occurs_h in
      (occurs, exp)
  | UpdE (exp_b, path, exp_f) ->
      let occurs_b, exp_b = annotate_exp bounds exp_b in
      let occurs_f, exp_f = annotate_exp bounds exp_f in
      let occurs_p, path = annotate_path bounds path in
      let exp = UpdE (exp_b, path, exp_f) $$ (at, note) in
      let occurs = union (union occurs_b occurs_f) occurs_p in
      (occurs, exp)
  | CallE (id, targs, args) ->
      let occurs, args = annotate_args bounds args in
      let exp = CallE (id, targs, args) $$ (at, note) in
      (occurs, exp)
  | IterE (_, ((_, _ :: _) as iterexp)) ->
      error at
        (Format.asprintf
           "iterated expression should initially have no annotations, but got \
            %s"
           (Print.string_of_iterexp iterexp))
  | IterE (exp, (iter, [])) -> (
      let occurs, exp = annotate_exp bounds exp in
      let itervars = collect_itervars bounds occurs iter in
      match itervars with
      | [] -> error at "empty iteration"
      | _ ->
          let exp = IterE (exp, (iter, itervars)) $$ (at, note) in
          let occurs =
            List.fold_left
              (fun occurs (id, typ, iters) ->
                VEnv.add id (typ, iters @ [ iter ]) occurs)
              occurs itervars
          in
          (occurs, exp))

and annotate_exps (bounds : VEnv.t) (exps : exp list) : VEnv.t * exp list =
  match exps with
  | [] -> (empty, [])
  | exp :: exps ->
      let occurs_h, exp_h = annotate_exp bounds exp in
      let occurs_t, exps_t = annotate_exps bounds exps in
      let exps = exp_h :: exps_t in
      let occurs = union occurs_h occurs_t in
      (occurs, exps)

and annotate_notexp (bounds : VEnv.t) (notexp : notexp) : VEnv.t * notexp =
  let mixop, exps = Mixfix.split notexp in
  let occurs, exps = annotate_exps bounds exps in
  let notexp = Mixfix.fill mixop exps in
  (occurs, notexp)

(* Path annotation *)

and annotate_path (bounds : VEnv.t) (path : path) : VEnv.t * path =
  let at, note = (path.at, path.note) in
  match path.it with
  | RootP -> (empty, path)
  | IdxP (path, exp) ->
      let occurs_p, path = annotate_path bounds path in
      let occurs_e, exp = annotate_exp bounds exp in
      let path = IdxP (path, exp) $$ (at, note) in
      let occurs = union occurs_p occurs_e in
      (occurs, path)
  | SliceP (path, exp_l, exp_h) ->
      let occurs_p, path = annotate_path bounds path in
      let occurs_l, exp_l = annotate_exp bounds exp_l in
      let occurs_h, exp_h = annotate_exp bounds exp_h in
      let path = SliceP (path, exp_l, exp_h) $$ (at, note) in
      let occurs = union (union occurs_p occurs_l) occurs_h in
      (occurs, path)
  | DotP (path, atom) ->
      let occurs, path = annotate_path bounds path in
      let path = DotP (path, atom) $$ (at, note) in
      (occurs, path)

(* Argument annotation *)

and annotate_arg (bounds : VEnv.t) (arg : arg) : VEnv.t * arg =
  let at = arg.at in
  match arg.it with
  | ExpA exp ->
      let occurs, exp = annotate_exp bounds exp in
      let arg = ExpA exp $ at in
      (occurs, arg)
  | DefA _ -> (empty, arg)

and annotate_args (bounds : VEnv.t) (args : arg list) : VEnv.t * arg list =
  match args with
  | [] -> (empty, [])
  | arg :: args ->
      let occurs_h, arg_h = annotate_arg bounds arg in
      let occurs_t, args_t = annotate_args bounds args in
      let args = arg_h :: args_t in
      let occurs = union occurs_h occurs_t in
      (occurs, args)

(* Premise annotation *)

let rec annotate_prem (bounds : VEnv.t) (prem : prem) : VEnv.t * prem =
  let at = prem.at in
  match prem.it with
  | RulePr (id, notexp, inputs) ->
      let mixop, exps = Mixfix.split notexp in
      let occurs, exps = annotate_exps bounds exps in
      let notexp = Mixfix.fill mixop exps in
      let prem = RulePr (id, notexp, inputs) $ at in
      (occurs, prem)
  | IfPr exp ->
      let occurs, exp = annotate_exp bounds exp in
      let prem = IfPr exp $ at in
      (occurs, prem)
  | IfHoldPr (id, notexp) ->
      let mixop, exps = Mixfix.split notexp in
      let occurs, exps = annotate_exps bounds exps in
      let notexp = Mixfix.fill mixop exps in
      let prem = IfHoldPr (id, notexp) $ at in
      (occurs, prem)
  | IfNotHoldPr (id, notexp) ->
      let mixop, exps = Mixfix.split notexp in
      let occurs, exps = annotate_exps bounds exps in
      let notexp = Mixfix.fill mixop exps in
      let prem = IfNotHoldPr (id, notexp) $ at in
      (occurs, prem)
  | LetPr _ ->
      error prem.at "let premise should appear only after the algo pass"
  | IterPr (_, (_, vars_bound, vars_bind))
    when (not (List.is_empty vars_bound)) || not (List.is_empty vars_bind) ->
      error at "iterated premise should initially have no annotations"
  | IterPr (prem, (iter, _, _)) -> (
      let occurs, prem = annotate_prem bounds prem in
      let itervars = collect_itervars bounds occurs iter in
      match itervars with
      | [] -> error at "empty iteration"
      | _ ->
          let occurs = iterate occurs itervars iter in
          let prem = IterPr (prem, (iter, itervars, [])) $ at in
          (occurs, prem))
  | DebugPr exp ->
      let occurs, exp = annotate_exp bounds exp in
      let prem = DebugPr exp $ at in
      (occurs, prem)

let rec annotate_prems (bounds : VEnv.t) (prems : prem list) :
    VEnv.t * prem list =
  match prems with
  | [] -> (empty, [])
  | prem :: prems ->
      let occurs_h, prem_h = annotate_prem bounds prem in
      let occurs_t, prems_t = annotate_prems bounds prems in
      let occurs = union occurs_h occurs_t in
      let prems = prem_h :: prems_t in
      (occurs, prems)

(* Rule annotation *)

let annotate_rule (bounds : VEnv.t) (rule : rule) : rule =
  let id, notexp, prems = rule.it in
  let _occurs_notexp, notexp = annotate_notexp bounds notexp in
  let _occurs_prems, prems = annotate_prems bounds prems in
  (id, notexp, prems) $ rule.at

(* Clause annotation *)

let annotate_clause (bounds : VEnv.t) (clause : clause) : clause =
  let args, exp, prems = clause.it in
  let _occurs_args, args = annotate_args bounds args in
  let _occurs_prems, prems = annotate_prems bounds prems in
  let _occurs_exp, exp = annotate_exp bounds exp in
  (args, exp, prems) $ clause.at

(* Table row annotation *)

let annotate_tablerow (bounds : VEnv.t) (tablerow : tablerow) : tablerow =
  let args, exp = tablerow.it in
  let _occurs_args, args = annotate_args bounds args in
  let _occurs_exp, exp = annotate_exp bounds exp in
  (args, exp) $ tablerow.at

(* Analysis *)

let analyze_rule (rule : rule) : rule =
  let bounds = Dimctx.infer (infer_rule rule) in
  annotate_rule bounds rule

let analyze_rulegroup (rulegroup : rulegroup) : rulegroup =
  let id, rules = rulegroup.it in
  let rules = List.map analyze_rule rules in
  (id, rules) $ rulegroup.at

let analyze_elsegroup (elsegroup : elsegroup) : elsegroup =
  let id, rule = elsegroup.it in
  let rule = analyze_rule rule in
  (id, rule) $ elsegroup.at

let analyze_tablerow (tablerow : tablerow) : tablerow =
  let bounds = Dimctx.infer (infer_tablerow tablerow) in
  annotate_tablerow bounds tablerow

let analyze_clause (clause : clause) : clause =
  let bounds = Dimctx.infer (infer_clause clause) in
  annotate_clause bounds clause

let analyze_def (def : def) : def =
  match def.it with
  | RelD (id, nottyp, inputs, rulegroups, elsegroup_opt, hints) ->
      let rulegroups = List.map analyze_rulegroup rulegroups in
      let elsegroup_opt = Option.map analyze_elsegroup elsegroup_opt in
      RelD (id, nottyp, inputs, rulegroups, elsegroup_opt, hints) $ def.at
  | TableDecD (id, params, typ, tablerows, hints) ->
      let tablerows = List.map analyze_tablerow tablerows in
      TableDecD (id, params, typ, tablerows, hints) $ def.at
  | FuncDecD (id, tparams, params, typ, clauses, elseclause_opt, hints) ->
      let clauses = List.map analyze_clause clauses in
      let elseclause_opt = Option.map analyze_clause elseclause_opt in
      FuncDecD (id, tparams, params, typ, clauses, elseclause_opt, hints)
      $ def.at
  | _ -> def

let analyze_spec (spec : spec) : spec = List.map analyze_def spec
