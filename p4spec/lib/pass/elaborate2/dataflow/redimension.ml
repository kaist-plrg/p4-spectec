open Domain.Lib
open Lang
open Il
module Mixfix = Domain.Mixfix
open Runtime.Static
open Error
open Envs
open Util.Source

(* Re-dimension analysis (elaborate2 only) :

   In elaborate2, dimension analysis runs *before* binding analysis, so when it
   annotates an iterated premise it can only record every iterated variable as
   `vars_bound` (it has no notion of which variables a premise binds, since
   bindings have not been desugared yet).

   After binding analysis splits an equality/let premise into a sequence of
   sub-premises (guards, extractions, ...), each resulting premise reads and
   writes a different set of variables. This pass recomputes, for every
   [IterPr], the correct (vars_bound, vars_bind) split — exactly as the original
   single-pass dimension analysis does in [elaborate/dataflow/dimension.ml].

   Expression iteration annotations are already correct at this point, so we do
   not touch them; we only read the existing annotations to reconstruct how each
   variable occurs (its dimension) within a premise. *)

(* Occurrence environments (mirrors dimension.ml) *)

let empty = VEnv.empty
let singleton id typ = VEnv.add id (typ, []) VEnv.empty

let diff (occurs_a : VEnv.t) (occurs_b : VEnv.t) : VEnv.t =
  VEnv.fold
    (fun id (typ, iters) acc ->
      if VEnv.mem id occurs_b then acc else VEnv.add id (typ, iters) acc)
    occurs_a VEnv.empty

let union (occurs_a : VEnv.t) (occurs_b : VEnv.t) : VEnv.t =
  VEnv.union
    (fun id (typ_a, iters_a) (typ_b, iters_b) ->
      if not (Il.Eq.eq_typ typ_a typ_b) then
        error id.at
          (Format.asprintf
             "type mismatch for identifier `%s` in union: %s vs %s"
             (Id.to_string id)
             (Il.Print.string_of_typ typ_a)
             (Il.Print.string_of_typ typ_b));
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

(* Reconstruct how variables occur in an already-annotated expression *)

let rec collect_occurs_exp (exp : exp) : VEnv.t =
  match exp.it with
  | BoolE _ | NumE _ | TextE _ -> empty
  | VarE id -> singleton id (exp.note $ exp.at)
  | UnE (_, _, exp)
  | UpCastE (_, exp)
  | DownCastE (_, exp)
  | SubE (exp, _)
  | MatchE (exp, _)
  | LenE exp
  | DotE (exp, _) ->
      collect_occurs_exp exp
  | BinE (_, _, exp_l, exp_r)
  | CmpE (_, _, exp_l, exp_r)
  | ConsE (exp_l, exp_r)
  | CatE (exp_l, exp_r)
  | MemE (exp_l, exp_r)
  | IdxE (exp_l, exp_r) ->
      union (collect_occurs_exp exp_l) (collect_occurs_exp exp_r)
  | TupleE exps | ListE exps -> collect_occurs_exps exps
  | CaseE notexp -> notexp |> Mixfix.args |> collect_occurs_exps
  | StrE expfields -> expfields |> List.map snd |> collect_occurs_exps
  | OptE (Some exp) -> collect_occurs_exp exp
  | OptE None -> empty
  | SliceE (exp_b, exp_l, exp_h) ->
      union
        (union (collect_occurs_exp exp_b) (collect_occurs_exp exp_l))
        (collect_occurs_exp exp_h)
  | UpdE (exp_b, path, exp_f) ->
      union
        (union (collect_occurs_exp exp_b) (collect_occurs_exp exp_f))
        (collect_occurs_path path)
  | CallE (_, _, args) -> collect_occurs_args args
  | IterE (exp, (iter, itervars)) ->
      let occurs = collect_occurs_exp exp in
      iterate occurs itervars iter

and collect_occurs_exps (exps : exp list) : VEnv.t =
  List.fold_left (fun acc exp -> union acc (collect_occurs_exp exp)) empty exps

and collect_occurs_path (path : path) : VEnv.t =
  match path.it with
  | RootP -> empty
  | IdxP (path, exp) ->
      union (collect_occurs_path path) (collect_occurs_exp exp)
  | SliceP (path, exp_l, exp_h) ->
      union
        (union (collect_occurs_path path) (collect_occurs_exp exp_l))
        (collect_occurs_exp exp_h)
  | DotP (path, _) -> collect_occurs_path path

and collect_occurs_arg (arg : arg) : VEnv.t =
  match arg.it with ExpA exp -> collect_occurs_exp exp | DefA _ -> empty

and collect_occurs_args (args : arg list) : VEnv.t =
  List.fold_left (fun acc arg -> union acc (collect_occurs_arg arg)) empty args

(* Premise re-annotation.
   Returns (occurs_bind, occurs, prem) where:
     - occurs_bind : variables this premise binds (writes)
     - occurs      : all variables occurring in this premise *)

let rec reannotate_prem (bounds : VEnv.t) (prem : prem) :
    VEnv.t * VEnv.t * prem =
  let at = prem.at in
  match prem.it with
  | RulePr (id, notexp, inputs) ->
      let _mixop, exps = Mixfix.split notexp in
      let exps_input, exps_output = Hints.Input.split inputs exps in
      let occurs_input = collect_occurs_exps exps_input in
      let occurs_output = collect_occurs_exps exps_output in
      let occurs = union occurs_input occurs_output in
      let prem = RulePr (id, notexp, inputs) $ at in
      (occurs_output, occurs, prem)
  | IfPr exp -> (empty, collect_occurs_exp exp, prem)
  | IfHoldPr (_, notexp) | IfNotHoldPr (_, notexp) ->
      (empty, collect_occurs_exps (Mixfix.args notexp), prem)
  | LetPr (exp_l, exp_r) ->
      let occurs_l = collect_occurs_exp exp_l in
      let occurs_r = collect_occurs_exp exp_r in
      (occurs_l, union occurs_l occurs_r, prem)
  | IterPr (prem_inner, (iter, _, _)) -> (
      let occurs_bind, occurs, prem_inner = reannotate_prem bounds prem_inner in
      let itervars = collect_itervars bounds occurs iter in
      let occurs_bound = diff occurs occurs_bind in
      let itervars_bound = collect_itervars bounds occurs_bound iter in
      let itervars_bind = collect_itervars bounds occurs_bind iter in
      match (itervars_bound, itervars_bind) with
      | [], [] -> error at "empty iteration"
      | _ ->
          let occurs_bind = iterate occurs_bind itervars_bind iter in
          let occurs = iterate occurs itervars iter in
          let prem =
            IterPr (prem_inner, (iter, itervars_bound, itervars_bind)) $ at
          in
          (occurs_bind, occurs, prem))
  | DebugPr exp -> (empty, collect_occurs_exp exp, prem)

let reannotate (bounds : VEnv.t) (prem : prem) : prem =
  let _occurs_bind, _occurs, prem = reannotate_prem bounds prem in
  prem
