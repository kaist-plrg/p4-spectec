open Domain.Lib
module Mixfix = Domain.Mixfix
open Lang
open Il2
module Type = Runtime.Type
open Runtime.Static
open Envs
open Util.Source

(* Helper for identifying singleton case *)

let rec is_singleton_case (dctx : Dctx.t) (typ : typ) : bool =
  match typ.it with
  | VarT (tid, targs) -> (
      let td = Dctx.find_typdef dctx tid in
      match td with
      | Defined (tparams, deftyp) -> (
          match deftyp.it with
          | PlainT typ ->
              let theta = TIdMap.of_lists tparams targs in
              let typ = Type.Subst.subst_typ theta typ in
              is_singleton_case dctx typ
          | StructT _ -> false
          | VariantT typcases -> List.length typcases = 1)
      | _ -> false)
  | _ -> false

(* Rename for an expression *)

module To = struct
  include Var

  let as_exp = Var.as_exp ~dim:true
end

module From = struct
  type t =
    | Bound of { exp_from : exp }
    | Bindmatch of { pattern : pattern; exp_from : exp }
    | Bindsub of { typ_sub : typ; exp_sub : exp; exp_from : exp }
end

module REnv = struct
  type t = (To.t * From.t * Iterctx.t) list

  (* Constructor *)

  let empty : t = []

  (* Adder *)

  let add renv to_ from_ iterctx = (to_, from_, iterctx) :: renv
end

(* Generate premises *)

let gen_prem_bound (dctx : Dctx.t) (to_ : To.t) (exp_from : exp)
    (iterctx : Iterctx.t) : prem =
  let open Il in
  let exp_cond =
    let exp_l = To.as_exp to_ in
    match exp_from.it with
    | CaseE notexp
      when Mixfix.arity notexp = 0
           && not (is_singleton_case dctx (exp_from.note $ exp_from.at)) ->
        MatchE (exp_l, CaseP (Mixfix.to_mixop notexp)) $$ (exp_from.at, BoolT)
    | OptE (Some _) -> MatchE (exp_l, OptP `Some) $$ (exp_from.at, BoolT)
    | OptE None -> MatchE (exp_l, OptP `None) $$ (exp_from.at, BoolT)
    | ListE [] -> MatchE (exp_l, ListP `Nil) $$ (exp_from.at, BoolT)
    | _ ->
        let exp_r = exp_from in
        CmpE (`EqOp, `BoolT, exp_l, exp_r) $$ (exp_from.at, BoolT)
  in
  let sidecondition = IfPr exp_cond $ exp_from.at in
  let iterctx =
    let venv =
      Dimension.infer_exp exp_from [] Dimension.Dimctx.empty
      |> Dimension.Dimctx.infer
    in
    let id, typ, iters = to_ in
    iterctx
    |> Iterctx.filter_bound (fun id typ_iter iters_iter ->
           VEnv.find_opt id venv
           |> Option.map (fun (typ, iters) ->
                  Typdim.sub (typ, iters) (typ_iter, iters_iter))
           |> Option.value ~default:false)
    |> Iterctx.add_var_bound id typ iters
  in
  Iterctx.iterate_prem iterctx sidecondition

let gen_prem_bind_match (to_ : To.t) (pattern : pattern) (exp_from : exp)
    (iterctx : Iterctx.t) : prem list =
  let open Il in
  let id, typ, iters = to_ in
  let exp_to = To.as_exp to_ in
  let sidecondition_guard_match =
    let exp_guard_match = MatchE (exp_to, pattern) $$ (exp_from.at, BoolT) in
    let sidecondition_guard_match = IfPr exp_guard_match $ exp_from.at in
    let iterctx =
      iterctx
      |> List.map (fun (iter, _, _) -> (iter, [], []))
      |> Iterctx.add_var_bound id typ iters
    in
    Iterctx.iterate_prem iterctx sidecondition_guard_match
  in
  let prem_bind =
    let prem_bind = LetPr (exp_from, exp_to) $ exp_from.at in
    let iterctx =
      let venv =
        Dimension.infer_exp exp_from [] Dimension.Dimctx.empty
        |> Dimension.Dimctx.infer
      in
      iterctx
      |> List.map (fun (iter, _, _) -> (iter, [], []))
      |> Iterctx.add_vars_bind venv
      |> Iterctx.add_var_bound id typ iters
    in
    Iterctx.iterate_prem iterctx prem_bind
  in
  [ sidecondition_guard_match; prem_bind ]

let gen_prem_bind_sub (to_ : To.t) (typ_sub : typ) (exp_sub : exp)
    (exp_from : exp) (iterctx : Iterctx.t) : prem list =
  let open Il in
  let id, typ, iters = to_ in
  let exp_to = To.as_exp to_ in
  let sidecondition_guard_sub =
    let exp_guard_sub = SubE (exp_to, typ_sub) $$ (exp_from.at, BoolT) in
    let sidecondition_guard_sub = IfPr exp_guard_sub $ exp_from.at in
    let iterctx =
      iterctx
      |> List.map (fun (iter, _, _) -> (iter, [], []))
      |> Iterctx.add_var_bound id typ iters
    in
    Iterctx.iterate_prem iterctx sidecondition_guard_sub
  in
  let prem_bind =
    let exp_downcast =
      DownCastE (typ_sub, exp_to) $$ (exp_from.at, typ_sub.it)
    in
    let prem_bind = LetPr (exp_sub, exp_downcast) $ exp_from.at in
    let iterctx =
      let venv =
        Dimension.infer_exp exp_from [] Dimension.Dimctx.empty
        |> Dimension.Dimctx.infer
      in
      iterctx
      |> List.map (fun (iter, _, _) -> (iter, [], []))
      |> Iterctx.add_vars_bind venv
      |> Iterctx.add_var_bound id typ iters
    in
    Iterctx.iterate_prem iterctx prem_bind
  in
  [ sidecondition_guard_sub; prem_bind ]

let gen_prem (dctx : Dctx.t) (to_ : To.t) (from : From.t) (iterctx : Iterctx.t)
    : prem list =
  match from with
  | Bound { exp_from } -> [ gen_prem_bound dctx to_ exp_from iterctx ]
  | Bindmatch { pattern; exp_from } ->
      gen_prem_bind_match to_ pattern exp_from iterctx
  | Bindsub { typ_sub; exp_sub; exp_from } ->
      gen_prem_bind_sub to_ typ_sub exp_sub exp_from iterctx

let gen_prems (dctx : Dctx.t) (iterctx_prem : Iterctx.t) (renv : REnv.t) :
    prem list =
  List.concat_map
    (fun (to_, from_, iterctx_exp) ->
      gen_prem dctx to_ from_ (iterctx_exp @ iterctx_prem))
    renv

let rename_exp_bind_match (dctx : Dctx.t) (renv : REnv.t)
    (iterctx_exp : Iterctx.t) (pattern : pattern) (exp_from : exp) :
    Dctx.t * REnv.t * Iterctx.t * exp =
  let to_ = Fresh.var_from_exp dctx.menv dctx.frees exp_from in
  let id_rename, typ, iters = to_ in
  let dctx = Dctx.add_free dctx id_rename in
  let from_ = From.Bindmatch { pattern; exp_from } in
  let renv = REnv.add renv to_ from_ iterctx_exp in
  let iterctx_exp =
    let bounds = Free.free_exp exp_from in
    iterctx_exp
    |> Iterctx.filter_bound (fun id _ _ -> not (IdSet.mem id bounds))
    |> Iterctx.add_var_bound id_rename typ iters
  in
  let exp = To.as_exp to_ in
  (dctx, renv, iterctx_exp, exp)

let rename_exp_bind_sub (dctx : Dctx.t) (renv : REnv.t)
    (iterctx_exp : Iterctx.t) (typ_sub : typ) (exp_sub : exp) (exp_from : exp) :
    Dctx.t * REnv.t * Iterctx.t * exp =
  let to_ = Fresh.var_from_exp dctx.menv dctx.frees exp_from in
  let id_rename, typ, iters = to_ in
  let dctx = Dctx.add_free dctx id_rename in
  let from_ = From.Bindsub { typ_sub; exp_sub; exp_from } in
  let renv = REnv.add renv to_ from_ iterctx_exp in
  let iterctx_exp =
    let bounds = Free.free_exp exp_from in
    iterctx_exp
    |> Iterctx.filter_bound (fun id _ _ -> not (IdSet.mem id bounds))
    |> Iterctx.add_var_bound id_rename typ iters
  in
  let exp = To.as_exp to_ in
  (dctx, renv, iterctx_exp, exp)

let check_upcast_terminal (exp : exp) : bool =
  match exp.it with
  | UpCastE (_, { it = CaseE notexp; _ }) when Mixfix.arity notexp = 0 -> true
  | _ -> false

let rec rename_exp (dctx : Dctx.t) (binds : IdSet.t) (renv : REnv.t)
    (iterctx_exp : Iterctx.t) (exp : exp) : Dctx.t * REnv.t * Iterctx.t * exp =
  let frees = Free.free_exp exp in
  (* If the expression contains no bindings, rename it
     Yet, skip upcast on terminals to enforce case-analysis,
     which helps with the later structuring phase
     e.g., patterns like let typ = (VoidT as typ) *)
  if
    IdSet.inter binds frees |> IdSet.is_empty && not (check_upcast_terminal exp)
  then rename_exp_bound dctx renv iterctx_exp exp
  else rename_exp_bind dctx binds renv iterctx_exp exp

and rename_exp_bound (dctx : Dctx.t) (renv : REnv.t) (iterctx_exp : Iterctx.t)
    (exp : exp) : Dctx.t * REnv.t * Iterctx.t * exp =
  let to_ = Fresh.var_from_exp dctx.menv dctx.frees exp in
  let id_rename, typ, iters = to_ in
  let dctx = Dctx.add_free dctx id_rename in
  let from_ = From.Bound { exp_from = exp } in
  let renv = REnv.add renv to_ from_ iterctx_exp in
  let iterctx_exp =
    let bounds = Free.free_exp exp in
    iterctx_exp
    |> Iterctx.filter_bound (fun id _ _ -> not (IdSet.mem id bounds))
    |> Iterctx.add_var_bound id_rename typ iters
  in
  let exp = To.as_exp to_ in
  (dctx, renv, iterctx_exp, exp)

and rename_exp_bind (dctx : Dctx.t) (binds : IdSet.t) (renv : REnv.t)
    (iterctx_exp : Iterctx.t) (exp : exp) : Dctx.t * REnv.t * Iterctx.t * exp =
  let open Il in
  let at, note = (exp.at, exp.note) in
  match exp.it with
  | UpCastE (typ, exp) ->
      let dctx, renv, iterctx_exp, exp =
        rename_exp dctx binds renv iterctx_exp exp
      in
      let exp_renamed = UpCastE (typ, exp) $$ (at, note) in
      rename_exp_bind_sub dctx renv iterctx_exp (exp.note $ at) exp exp_renamed
  | TupleE exps ->
      let dctx, renv, iterctx_exp, exps =
        rename_exps dctx binds renv iterctx_exp exps
      in
      let exp = TupleE exps $$ (at, note) in
      (dctx, renv, iterctx_exp, exp)
  | CaseE notexp when is_singleton_case dctx (note $ at) ->
      let mixop, exps = Mixfix.split notexp in
      let dctx, renv, iterctx_exp, exps =
        rename_exps dctx binds renv iterctx_exp exps
      in
      let exp = CaseE (Mixfix.fill mixop exps) $$ (at, note) in
      (dctx, renv, iterctx_exp, exp)
  | CaseE notexp ->
      let mixop, exps = Mixfix.split notexp in
      let dctx, renv, iterctx_exp, exps =
        rename_exps dctx binds renv iterctx_exp exps
      in
      let exp_renamed = CaseE (Mixfix.fill mixop exps) $$ (at, note) in
      rename_exp_bind_match dctx renv iterctx_exp (CaseP mixop) exp_renamed
  | StrE expfields ->
      let atoms, exps = List.split expfields in
      let dctx, renv, iterctx_exp, exps =
        rename_exps dctx binds renv iterctx_exp exps
      in
      let expfields = List.combine atoms exps in
      let exp = StrE expfields $$ (at, note) in
      (dctx, renv, iterctx_exp, exp)
  | OptE (Some exp) ->
      let dctx, renv, iterctx_exp, exp =
        rename_exp dctx binds renv iterctx_exp exp
      in
      let exp_renamed = OptE (Some exp) $$ (at, note) in
      rename_exp_bind_match dctx renv iterctx_exp (OptP `Some) exp_renamed
  | OptE None -> rename_exp_bind_match dctx renv iterctx_exp (OptP `None) exp
  | ListE exps ->
      let dctx, renv, iterctx_exp, exps =
        rename_exps dctx binds renv iterctx_exp exps
      in
      let exp_renamed = ListE exps $$ (at, note) in
      let pattern =
        if List.length exps = 0 then ListP `Nil
        else ListP (`Fixed (List.length exps))
      in
      rename_exp_bind_match dctx renv iterctx_exp pattern exp_renamed
  | ConsE (exp_h, exp_t) ->
      let dctx, renv, iterctx_exp, exp_h =
        rename_exp dctx binds renv iterctx_exp exp_h
      in
      let dctx, renv, iterctx_exp, exp_t =
        rename_exp dctx binds renv iterctx_exp exp_t
      in
      let exp_renamed = ConsE (exp_h, exp_t) $$ (at, note) in
      rename_exp_bind_match dctx renv iterctx_exp (ListP `Cons) exp_renamed
  | IterE (exp, (iter, vars)) ->
      let iterctx_exp = (iter, vars, []) :: iterctx_exp in
      let dctx, renv, iterctx_exp, exp =
        rename_exp dctx binds renv iterctx_exp exp
      in
      let iterctx_exp_h, iterctx_exp_t =
        ([ List.hd iterctx_exp ], List.tl iterctx_exp)
      in
      let exp = Iterctx.iterate_exp iterctx_exp_h exp in
      (dctx, renv, iterctx_exp_t, exp)
  | _ -> (dctx, renv, iterctx_exp, exp)

and rename_exps (dctx : Dctx.t) (binds : IdSet.t) (renv : REnv.t)
    (iterctx_exp : Iterctx.t) (exps : exp list) :
    Dctx.t * REnv.t * Iterctx.t * exp list =
  List.fold_left
    (fun (dctx, renv_pre, iterctx_exp, exps) exp ->
      let dctx, renv_post, iterctx_exp_re, exp =
        rename_exp dctx binds REnv.empty iterctx_exp exp
      in
      let iterctx_exp =
        let drop = List.length iterctx_exp_re - List.length iterctx_exp in
        List.filteri (fun i _ -> i >= drop) iterctx_exp_re
      in
      (dctx, renv_pre @ renv_post, iterctx_exp, exps @ [ exp ]))
    (dctx, renv, iterctx_exp, [])
    exps

(* Arguments *)

and rename_arg (dctx : Dctx.t) (binds : IdSet.t) (renv : REnv.t)
    (iterctx_exp : Iterctx.t) (arg : arg) : Dctx.t * REnv.t * Iterctx.t * arg =
  let at = arg.at in
  match arg.it with
  | ExpA exp ->
      let dctx, renv, iterctx_exp, exp =
        rename_exp dctx binds REnv.empty iterctx_exp exp
      in
      let arg = Il.ExpA exp $ at in
      (dctx, renv, iterctx_exp, arg)
  | _ -> (dctx, renv, iterctx_exp, arg)

and rename_args (dctx : Dctx.t) (binds : IdSet.t) (renv : REnv.t)
    (iterctx_exp : Iterctx.t) (args : arg list) :
    Dctx.t * REnv.t * Iterctx.t * arg list =
  List.fold_left
    (fun (dctx, renv_pre, iterctx_exp, args) arg ->
      let dctx, renv_post, iterctx_exp_re, arg =
        rename_arg dctx binds REnv.empty iterctx_exp arg
      in
      let iterctx_exp =
        let drop = List.length iterctx_exp_re - List.length iterctx_exp in
        List.filteri (fun i _ -> i >= drop) iterctx_exp_re
      in
      (dctx, renv_pre @ renv_post, iterctx_exp, args @ [ arg ]))
    (dctx, renv, iterctx_exp, [])
    args
