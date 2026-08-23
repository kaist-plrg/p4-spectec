open Ast
module Mixfix = Domain.Mixfix
open Util.Source

(* Collector interface *)

type 'a collector = {
  default : 'a;
  compose : 'a -> 'a -> 'a;
  collect_typ : 'a collector -> typ -> 'a;
  collect_exp : 'a collector -> exp -> 'a;
  collect_iterexp : 'a collector -> iterexp -> 'a;
  collect_path : 'a collector -> path -> 'a;
  collect_arg : 'a collector -> arg -> 'a;
  collect_prem : 'a collector -> prem -> 'a;
  collect_iterprem : 'a collector -> iterprem -> 'a;
}

(* Entry points *)

let collect_typ (c : 'a collector) (typ : typ) = c.collect_typ c typ
let collect_exp (c : 'a collector) (exp : exp) = c.collect_exp c exp

let collect_iterexp (c : 'a collector) (iterexp : iterexp) =
  c.collect_iterexp c iterexp

let collect_path (c : 'a collector) (path : path) = c.collect_path c path
let collect_arg (c : 'a collector) (arg : arg) = c.collect_arg c arg
let collect_prem (c : 'a collector) (prem : prem) = c.collect_prem c prem

let collect_iterprem (c : 'a collector) (iterprem : iterprem) =
  c.collect_iterprem c iterprem

(* List helpers call through the dispatch functions above, so any override of
   collect_exp etc. is automatically respected when visiting lists. *)

let rec collect_typs (c : 'a collector) = function
  | [] -> c.default
  | typ_h :: typs_t -> c.compose (collect_typ c typ_h) (collect_typs c typs_t)

let rec collect_exps (c : 'a collector) = function
  | [] -> c.default
  | exp_h :: exps_t -> c.compose (collect_exp c exp_h) (collect_exps c exps_t)

let rec collect_args (c : 'a collector) = function
  | [] -> c.default
  | arg_h :: args_t -> c.compose (collect_arg c arg_h) (collect_args c args_t)

let rec collect_prems (c : 'a collector) = function
  | [] -> c.default
  | prem_h :: prems_t ->
      c.compose (collect_prem c prem_h) (collect_prems c prems_t)

(* Default implementations — standard recursive descent.
   These are exposed so users can call them from their own overrides. *)

let default_collect_typ (c : 'a collector) (typ : typ) : 'a =
  match typ.it with
  | VarT (_, targs) -> collect_typs c targs
  | TupleT typs -> collect_typs c typs
  | IterT (typ, _) -> collect_typ c typ
  | _ -> c.default

let default_collect_exp (c : 'a collector) (exp : exp) : 'a =
  let ( $@ ) = c.compose in
  match exp.it with
  | BoolE _ | NumE _ | TextE _ | VarE _ -> c.default
  | UnE (_, _, exp) -> collect_exp c exp
  | BinE (_, _, exp_l, exp_r) | CmpE (_, _, exp_l, exp_r) ->
      collect_exp c exp_l $@ collect_exp c exp_r
  | UpCastE (typ, exp) | DownCastE (typ, exp) ->
      collect_typ c typ $@ collect_exp c exp
  | SubE (exp, typ, _) -> collect_exp c exp $@ collect_typ c typ
  | MatchE (exp, _) -> collect_exp c exp
  | TupleE exps -> collect_exps c exps
  | CaseE notexp -> collect_exps c (Mixfix.args notexp)
  | StrE expfields -> expfields |> List.map snd |> collect_exps c
  | OptE (Some exp) -> collect_exp c exp
  | OptE None -> c.default
  | ListE exps -> collect_exps c exps
  | ConsE (exp_l, exp_r) | CatE (exp_l, exp_r) | MemE (exp_l, exp_r) ->
      collect_exp c exp_l $@ collect_exp c exp_r
  | LenE exp | DotE (exp, _) -> collect_exp c exp
  | IdxE (exp_l, exp_r) -> collect_exp c exp_l $@ collect_exp c exp_r
  | SliceE (exp_l, exp_m, exp_r) ->
      collect_exp c exp_l $@ collect_exp c exp_m $@ collect_exp c exp_r
  | UpdE (exp_l, path, exp_r) ->
      collect_exp c exp_l $@ collect_path c path $@ collect_exp c exp_r
  | CallE (_, targs, args) -> collect_typs c targs $@ collect_args c args
  | IterE (exp, iterexp) -> collect_exp c exp $@ collect_iterexp c iterexp

let default_collect_iterexp (c : 'a collector) (_iterexp : iterexp) : 'a =
  c.default

let default_collect_path (c : 'a collector) (path : path) : 'a =
  let ( $@ ) = c.compose in
  match path.it with
  | RootP -> c.default
  | IdxP (path, exp) -> collect_path c path $@ collect_exp c exp
  | SliceP (path, exp_l, exp_r) ->
      collect_path c path $@ collect_exp c exp_l $@ collect_exp c exp_r
  | DotP (path, _) -> collect_path c path

let default_collect_arg (c : 'a collector) (arg : arg) : 'a =
  match arg.it with ExpA exp -> collect_exp c exp | DefA _ -> c.default

let default_collect_prem (c : 'a collector) (prem : prem) : 'a =
  let ( $@ ) = c.compose in
  match prem.it with
  | RulePr (_, notexp, _) -> collect_exps c (Mixfix.args notexp)
  | IfPr exp -> collect_exp c exp
  | IfHoldPr (_, notexp) | IfNotHoldPr (_, notexp) ->
      collect_exps c (Mixfix.args notexp)
  | LetPr (exp_l, exp_r) -> collect_exp c exp_l $@ collect_exp c exp_r
  | IterPr (prem, iterprem) ->
      collect_prem c prem $@ collect_iterprem c iterprem
  | DebugPr exp -> collect_exp c exp

let default_collect_iterprem (c : 'a collector) (_iterprem : iterprem) : 'a =
  c.default

let make_base ~default ~compose =
  {
    default;
    compose;
    collect_typ = default_collect_typ;
    collect_exp = default_collect_exp;
    collect_iterexp = default_collect_iterexp;
    collect_path = default_collect_path;
    collect_arg = default_collect_arg;
    collect_prem = default_collect_prem;
    collect_iterprem = default_collect_iterprem;
  }
