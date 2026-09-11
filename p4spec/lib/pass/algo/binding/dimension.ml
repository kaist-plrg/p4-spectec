open Lang
open Il
module Mixfix = Domain.Mixfix
open Runtime.Static
open Envs
open Util.Source

(* Dimension inference :

   A simplified version of elaborate/dimension.ml.
   Finds free variables in expressions and records their dimensions.
   If a variable appears multiple times, the minimal dimension is kept. *)

let rec infer_exp' (exp : exp) (iters : iter list) (venv : VEnv.t) : VEnv.t =
  let typ = exp.note $ exp.at in
  match exp.it with
  | BoolE _ | NumE _ | TextE _ -> venv
  | VarE id -> (
      match VEnv.find_opt id venv with
      | None -> VEnv.add id (typ, iters) venv
      | Some (typ_prev, iters_prev) ->
          if Typdim.sub (typ, iters) (typ_prev, iters_prev) then
            VEnv.add id (typ, iters) venv
          else venv)
  | UnE (_, _, exp)
  | UpCastE (_, exp)
  | DownCastE (_, exp)
  | SubE (exp, _, _)
  | MatchE (exp, _)
  | LenE exp
  | DotE (exp, _) ->
      infer_exp' exp iters venv
  | BinE (_, _, exp_l, exp_r)
  | CmpE (_, _, exp_l, exp_r)
  | ConsE (exp_l, exp_r)
  | CatE (exp_l, exp_r)
  | MemE (exp_l, exp_r)
  | IdxE (exp_l, exp_r) ->
      venv |> infer_exp' exp_l iters |> infer_exp' exp_r iters
  | TupleE exps | ListE exps -> infer_exps' exps iters venv
  | CaseE notexp -> infer_notexp' notexp iters venv
  | StrE expfields ->
      let exps = List.map snd expfields in
      infer_exps' exps iters venv
  | OptE (Some exp) -> infer_exp' exp iters venv
  | OptE None -> venv
  | SliceE (exp_b, exp_l, exp_h) ->
      venv |> infer_exp' exp_b iters |> infer_exp' exp_l iters
      |> infer_exp' exp_h iters
  | UpdE (exp_b, path, exp_f) ->
      venv |> infer_exp' exp_b iters |> infer_path' path iters
      |> infer_exp' exp_f iters
  | CallE (_, _, args) -> infer_args' args iters venv
  | IterE (exp, (iter, _)) -> infer_exp' exp (iter :: iters) venv

and infer_exps' (exps : exp list) (iters : iter list) (venv : VEnv.t) : VEnv.t =
  List.fold_left (fun venv exp -> infer_exp' exp iters venv) venv exps

and infer_notexp' (notexp : notexp) (iters : iter list) (venv : VEnv.t) : VEnv.t
    =
  let exps = Mixfix.args notexp in
  infer_exps' exps iters venv

and infer_path' (path : path) (iters : iter list) (venv : VEnv.t) : VEnv.t =
  match path.it with
  | RootP -> venv
  | IdxP (path, exp) -> venv |> infer_path' path iters |> infer_exp' exp iters
  | SliceP (path, exp_l, exp_h) ->
      venv |> infer_path' path iters |> infer_exp' exp_l iters
      |> infer_exp' exp_h iters
  | DotP (path, _) -> infer_path' path iters venv

and infer_arg' (arg : arg) (iters : iter list) (venv : VEnv.t) : VEnv.t =
  match arg.it with ExpA exp -> infer_exp' exp iters venv | DefA _ -> venv

and infer_args' (args : arg list) (iters : iter list) (venv : VEnv.t) : VEnv.t =
  List.fold_left (fun venv arg -> infer_arg' arg iters venv) venv args

let infer_exp (exp : exp) : VEnv.t = infer_exp' exp [] VEnv.empty
let infer_exps (exps : exp list) : VEnv.t = infer_exps' exps [] VEnv.empty
