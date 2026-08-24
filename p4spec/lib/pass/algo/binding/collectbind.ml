open Lang
open Il
module Mixfix = Domain.Mixfix
open Error
open Runtime.Static.Envs
open Util.Source

(* Collect binding identifiers,
   while enforcing the invariant that binding identifiers
   can only occur in invertible constructs *)

let collect_noninvertible (at : region) (construct : string)
    (benv : Bind.BEnv.t) : unit =
  if not (Bind.BEnv.is_empty benv) then
    error at
      (Format.asprintf "invalid binding position(s) for %s in non-invertible %s"
         (Bind.BEnv.to_string benv) construct)

let rec collect_exp (ctx : Ctx.t) (exp : exp) : Bind.BEnv.t =
  match exp.it with
  | BoolE _ | NumE _ | TextE _ -> Bind.BEnv.empty
  | VarE id ->
      if VEnv.mem id ctx.venv then Bind.BEnv.empty
      else Bind.BEnv.singleton id (exp.note $ exp.at)
  | UnE (_, _, exp) ->
      let binds = collect_exp ctx exp in
      collect_noninvertible exp.at "unary operator" binds;
      Bind.BEnv.empty
  | BinE (_, _, exp_l, exp_r) ->
      let binds_l = collect_exp ctx exp_l in
      let binds_r = collect_exp ctx exp_r in
      let binds = Bind.BEnv.union binds_l binds_r in
      collect_noninvertible exp.at "binary operator" binds;
      Bind.BEnv.empty
  | CmpE (_, _, exp_l, exp_r) ->
      let binds_l = collect_exp ctx exp_l in
      let binds_r = collect_exp ctx exp_r in
      let binds = Bind.BEnv.union binds_l binds_r in
      collect_noninvertible exp.at "comparison operator" binds;
      Bind.BEnv.empty
  | UpCastE (_, exp) -> collect_exp ctx exp
  | DownCastE (_, exp) ->
      let binds = collect_exp ctx exp in
      collect_noninvertible exp.at "downcast operator" binds;
      Bind.BEnv.empty
  | SubE (exp, _, _) ->
      let binds = collect_exp ctx exp in
      collect_noninvertible exp.at "subtype check operator" binds;
      Bind.BEnv.empty
  | MatchE (exp, _) ->
      let binds = collect_exp ctx exp in
      collect_noninvertible exp.at "match check operator" binds;
      Bind.BEnv.empty
  | TupleE exps -> collect_exps ctx exps
  | CaseE notexp -> notexp |> Mixfix.args |> collect_exps ctx
  | StrE expfields -> expfields |> List.map snd |> collect_exps ctx
  | OptE exp_opt ->
      exp_opt
      |> Option.map (collect_exp ctx)
      |> Option.value ~default:Bind.BEnv.empty
  | ListE exps -> collect_exps ctx exps
  | ConsE (exp_l, exp_r) ->
      let binds_l = collect_exp ctx exp_l in
      let binds_r = collect_exp ctx exp_r in
      Bind.BEnv.union binds_l binds_r
  | CatE (exp_l, exp_r) ->
      let binds_l = collect_exp ctx exp_l in
      let binds_r = collect_exp ctx exp_r in
      let binds = Bind.BEnv.union binds_l binds_r in
      collect_noninvertible exp.at "concatenation operator" binds;
      Bind.BEnv.empty
  | MemE (exp_l, exp_r) ->
      let binds_l = collect_exp ctx exp_l in
      let binds_r = collect_exp ctx exp_r in
      let binds = Bind.BEnv.union binds_l binds_r in
      collect_noninvertible exp.at "set membership operator" binds;
      Bind.BEnv.empty
  | LenE exp ->
      let binds = collect_exp ctx exp in
      collect_noninvertible exp.at "length operator" binds;
      Bind.BEnv.empty
  | DotE (exp, _) ->
      let binds = collect_exp ctx exp in
      collect_noninvertible exp.at "dot operator" binds;
      Bind.BEnv.empty
  | IdxE (exp_b, exp_i) ->
      let binds_b = collect_exp ctx exp_b in
      let binds_i = collect_exp ctx exp_i in
      let binds = Bind.BEnv.union binds_b binds_i in
      collect_noninvertible exp.at "indexing operator" binds;
      Bind.BEnv.empty
  | SliceE (exp_b, exp_l, exp_h) ->
      let binds_b = collect_exp ctx exp_b in
      let binds_l = collect_exp ctx exp_l in
      let binds_h = collect_exp ctx exp_h in
      let binds = Bind.BEnv.union binds_b binds_l in
      let binds = Bind.BEnv.union binds binds_h in
      collect_noninvertible exp.at "slicing operator" binds;
      Bind.BEnv.empty
  | UpdE (exp_b, path, exp_f) ->
      let binds_b = collect_exp ctx exp_b in
      let binds_p = collect_path ctx path in
      let binds_f = collect_exp ctx exp_f in
      let binds = Bind.BEnv.union binds_b binds_f in
      let binds = Bind.BEnv.union binds binds_p in
      collect_noninvertible exp.at "update operator" binds;
      Bind.BEnv.empty
  | CallE (_, _, args) ->
      let binds = collect_args ctx args in
      collect_noninvertible exp.at "call operator" binds;
      Bind.BEnv.empty
  | IterE (exp, (iter, _vars)) ->
      let binds = collect_exp ctx exp in
      let binds = Bind.BEnv.map (Bind.Occ.add_iter iter) binds in
      binds

and collect_exps (ctx : Ctx.t) (exps : exp list) : Bind.BEnv.t =
  match exps with
  | [] -> Bind.BEnv.empty
  | exp :: exps ->
      let binds_h = collect_exp ctx exp in
      let binds_t = collect_exps ctx exps in
      Bind.BEnv.union binds_h binds_t

and collect_path (ctx : Ctx.t) (path : path) : Bind.BEnv.t =
  match path.it with
  | RootP -> Bind.BEnv.empty
  | IdxP (path, exp) ->
      let binds_p = collect_path ctx path in
      let binds_e = collect_exp ctx exp in
      Bind.BEnv.union binds_p binds_e
  | SliceP (path, exp_l, exp_h) ->
      let binds_p = collect_path ctx path in
      let binds_l = collect_exp ctx exp_l in
      let binds_h = collect_exp ctx exp_h in
      let binds = Bind.BEnv.union binds_p binds_l in
      Bind.BEnv.union binds binds_h
  | DotP (path, _) -> collect_path ctx path

and collect_arg (ctx : Ctx.t) (arg : arg) : Bind.BEnv.t =
  match arg.it with
  | ExpA exp -> collect_exp ctx exp
  | DefA _ -> Bind.BEnv.empty

and collect_args (ctx : Ctx.t) (args : arg list) : Bind.BEnv.t =
  match args with
  | [] -> Bind.BEnv.empty
  | arg :: args ->
      let binds_h = collect_arg ctx arg in
      let binds_t = collect_args ctx args in
      Bind.BEnv.union binds_h binds_t
