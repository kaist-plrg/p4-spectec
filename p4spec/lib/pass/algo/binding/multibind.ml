open Domain.Lib
open Lang
open Il
open Runtime.Static
open Envs
module Mixfix = Domain.Mixfix
open Util.Source

module Ids = struct
  include IdSet

  let to_string = to_string ~with_braces:false
end

module REnv = struct
  include MakeIdEnv (Ids)

  let init (benv : Bind.BEnv.t) : t =
    Bind.BEnv.fold
      (fun id bind renv ->
        match bind with
        | Bind.Occ.Multi _ -> add id Ids.empty renv
        | Bind.Occ.Single _ -> renv)
      benv empty
end

let gen_sidecondition (benv : Bind.BEnv.t) (iterctx : Iterctx.t) (id : Id.t)
    (ids_rename : Ids.t) : prem =
  let typ, iters = Bind.BEnv.find id benv |> Bind.Occ.strip in
  let id_rename, ids_rename =
    ids_rename |> IdSet.elements |> fun ids -> (List.hd ids, List.tl ids)
  in
  let exp =
    let exp =
      let exp_l = VarE id $$ (id.at, typ.it) in
      let exp_r = VarE id_rename $$ (id_rename.at, typ.it) in
      CmpE (`EqOp, `BoolT, exp_l, exp_r) $$ (id_rename.at, BoolT)
    in
    List.fold_left
      (fun exp_l id_rename ->
        let exp_r =
          let exp_l = VarE id $$ (id.at, typ.it) in
          let exp_r = VarE id_rename $$ (id_rename.at, typ.it) in
          CmpE (`EqOp, `BoolT, exp_l, exp_r) $$ (id_rename.at, BoolT)
        in
        BinE (`AndOp, `BoolT, exp_l, exp_r) $$ (id_rename.at, BoolT))
      exp ids_rename
  in
  let sidecondition = IfPr exp $ id.at in
  let iterctx =
    let iters = iters @ Iterctx.iters_of iterctx in
    let venv =
      List.map (fun id -> (id, (typ, []))) (id :: id_rename :: ids_rename)
      |> VEnv.of_list
    in
    List.map (fun iter -> (iter, [], [])) iters |> Iterctx.add_vars_bound venv
  in
  Iterctx.iterate_prem iterctx sidecondition

let gen_sideconditions (benv : Bind.BEnv.t) (iterctx : Iterctx.t)
    (renv : REnv.t) : prem list =
  let renv = REnv.mapi Ids.remove renv in
  REnv.fold
    (fun id ids_rename sideconditions ->
      if Ids.is_empty ids_rename then sideconditions
      else
        let sidecondition = gen_sidecondition benv iterctx id ids_rename in
        sideconditions @ [ sidecondition ])
    renv []

let rec rename_exp (ctx : Ctx.t) (renv : REnv.t) (exp : exp) :
    Ctx.t * REnv.t * exp =
  let at, note = (exp.at, exp.note) in
  match exp.it with
  | VarE id -> (
      match REnv.find_opt id renv with
      (* Leftmost binding occurrence *)
      | Some ids_rename when IdSet.is_empty ids_rename ->
          let exp = VarE id $$ (at, note) in
          let renv =
            let ids_rename = IdSet.singleton id in
            REnv.add id ids_rename renv
          in
          (ctx, renv, exp)
      (* Parallel binding occurrences *)
      | Some ids_rename ->
          let id_rename = Fresh.id ctx.frees id in
          let ctx = Ctx.add_free ctx id_rename in
          let renv =
            let ids_rename = IdSet.add id_rename ids_rename in
            REnv.add id ids_rename renv
          in
          let exp = VarE id_rename $$ (at, note) in
          (ctx, renv, exp)
      | None -> (ctx, renv, exp))
  | UpCastE (typ, exp) ->
      let ctx, renv, exp = rename_exp ctx renv exp in
      let exp = UpCastE (typ, exp) $$ (at, note) in
      (ctx, renv, exp)
  | TupleE exps ->
      let ctx, renv, exps = rename_exps ctx renv exps in
      let exp = TupleE exps $$ (at, note) in
      (ctx, renv, exp)
  | CaseE notexp ->
      let mixop, exps = Mixfix.split notexp in
      let ctx, renv, exps = rename_exps ctx renv exps in
      let exp = CaseE (Mixfix.fill mixop exps) $$ (at, note) in
      (ctx, renv, exp)
  | StrE expfields ->
      let atoms, exps = List.split expfields in
      let ctx, renv, exps = rename_exps ctx renv exps in
      let expfields = List.combine atoms exps in
      let exp = StrE expfields $$ (at, note) in
      (ctx, renv, exp)
  | OptE (Some exp) ->
      let ctx, renv, exp = rename_exp ctx renv exp in
      let exp = OptE (Some exp) $$ (at, note) in
      (ctx, renv, exp)
  | OptE None -> (ctx, renv, exp)
  | ListE exps ->
      let ctx, renv, exps = rename_exps ctx renv exps in
      let exp = ListE exps $$ (at, note) in
      (ctx, renv, exp)
  | ConsE (exp_h, exp_t) ->
      let ctx, renv, exp_h = rename_exp ctx renv exp_h in
      let ctx, renv, exp_t = rename_exp ctx renv exp_t in
      let exp = ConsE (exp_h, exp_t) $$ (at, note) in
      (ctx, renv, exp)
  | IterE (exp, (iter, vars)) ->
      let ctx, renv, exp = rename_exp ctx renv exp in
      let vars =
        let frees = Free.free_exp exp in
        vars
        |> List.map (fun (id, typ, iters) ->
               match REnv.find_opt id renv with
               | None -> [ (id, typ, iters) ]
               | Some ids_rename when IdSet.is_empty ids_rename ->
                   [ (id, typ, iters) ]
               | Some ids_rename ->
                   let ids = IdSet.inter frees ids_rename in
                   ids |> IdSet.elements
                   |> List.map (fun id_rename -> (id_rename, typ, iters)))
        |> List.flatten
      in
      let exp = IterE (exp, (iter, vars)) $$ (at, note) in
      (ctx, renv, exp)
  | _ -> (ctx, renv, exp)

and rename_exps (ctx : Ctx.t) (renv : REnv.t) (exps : exp list) :
    Ctx.t * REnv.t * exp list =
  List.fold_left
    (fun (ctx, renv, exps) exp ->
      let ctx, renv, exp = rename_exp ctx renv exp in
      (ctx, renv, exps @ [ exp ]))
    (ctx, renv, []) exps

and rename_arg (ctx : Ctx.t) (renv : REnv.t) (arg : arg) : Ctx.t * REnv.t * arg
    =
  let at = arg.at in
  match arg.it with
  | ExpA exp ->
      let ctx, renv, exp = rename_exp ctx renv exp in
      let arg = ExpA exp $ at in
      (ctx, renv, arg)
  | DefA _ -> (ctx, renv, arg)

and rename_args (ctx : Ctx.t) (renv : REnv.t) (args : arg list) :
    Ctx.t * REnv.t * arg list =
  List.fold_left
    (fun (ctx, renv, args) arg ->
      let ctx, renv, arg = rename_arg ctx renv arg in
      (ctx, renv, args @ [ arg ]))
    (ctx, renv, []) args
