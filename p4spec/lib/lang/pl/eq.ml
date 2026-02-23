open Ast
open Xl
include Il.Eq

(* Call prose using hints *)

let rec eq_func_call (func_call_a : func_call) (func_call_b : func_call) : bool
    =
  match (func_call_a, func_call_b) with
  | ( ProseFuncCall (`Check (id_a, _, _, targs_a, args_a)),
      ProseFuncCall (`Check (id_b, _, _, targs_b, args_b)) )
  | ( ProseFuncCall (`Yield (id_a, _, targs_a, args_a)),
      ProseFuncCall (`Yield (id_b, _, targs_b, args_b)) )
  | MathFuncCall (id_a, targs_a, args_a), MathFuncCall (id_b, targs_b, args_b)
    ->
      eq_id id_a id_b && eq_typs targs_a targs_b && eq_args args_a args_b
  | _ -> false

(* Expressions *)

and eq_exp (exp_a : exp) (exp_b : exp) : bool =
  match (exp_a.it, exp_b.it) with
  | BoolE b_a, BoolE b_b -> b_a = b_b
  | NumE n_a, NumE n_b -> Num.eq n_a n_b
  | TextE t_a, TextE t_b -> t_a = t_b
  | VarE id_a, VarE id_b -> eq_id id_a id_b
  | UnE (unop_a, optyp_a, exp_a), UnE (unop_b, optyp_b, exp_b) ->
      unop_a = unop_b && optyp_a = optyp_b && eq_exp exp_a exp_b
  | ( BinE (binop_a, optyp_a, exp_l_a, exp_r_a),
      BinE (binop_b, optyp_b, exp_l_b, exp_r_b) ) ->
      binop_a = binop_b && optyp_a = optyp_b && eq_exp exp_l_a exp_l_b
      && eq_exp exp_r_a exp_r_b
  | ( CmpE (cmpop_a, optyp_a, exp_l_a, exp_r_a),
      CmpE (cmpop_b, optyp_b, exp_l_b, exp_r_b) ) ->
      cmpop_a = cmpop_b && optyp_a = optyp_b && eq_exp exp_l_a exp_l_b
      && eq_exp exp_r_a exp_r_b
  | UpCastE (typ_a, exp_a), UpCastE (typ_b, exp_b)
  | DownCastE (typ_a, exp_a), DownCastE (typ_b, exp_b)
  | SubE (exp_a, typ_a), SubE (exp_b, typ_b) ->
      eq_exp exp_a exp_b && eq_typ typ_a typ_b
  | MatchE (exp_a, pattern_a), MatchE (exp_b, pattern_b) ->
      eq_exp exp_a exp_b && eq_pattern pattern_a pattern_b
  | TupleE exps_a, TupleE exps_b -> eq_exps exps_a exps_b
  | ( CaseE (id_a, mixop_a, exps_a, _hint_a),
      CaseE (id_b, mixop_b, exps_b, _hint_b) ) ->
      eq_id id_a id_b && eq_mixop mixop_a mixop_b && eq_exps exps_a exps_b
  | StrE expfields_a, StrE expfields_b ->
      List.length expfields_a = List.length expfields_b
      && List.for_all2
           (fun (atom_a, exp_a) (atom_b, exp_b) ->
             eq_atom atom_a atom_b && eq_exp exp_a exp_b)
           expfields_a expfields_b
  | OptE (Some exp_a), OptE (Some exp_b) -> eq_exp exp_a exp_b
  | OptE None, OptE None -> true
  | ListE exps_a, ListE exps_b -> eq_exps exps_a exps_b
  | ConsE (exp_h_a, exp_t_a), ConsE (exp_h_b, exp_t_b) ->
      eq_exp exp_h_a exp_h_b && eq_exp exp_t_a exp_t_b
  | CatE (exp_l_a, exp_r_a), CatE (exp_l_b, exp_r_b) ->
      eq_exp exp_l_a exp_l_b && eq_exp exp_r_a exp_r_b
  | MemE (exp_e_a, exp_s_a), MemE (exp_e_b, exp_s_b) ->
      eq_exp exp_e_a exp_e_b && eq_exp exp_s_a exp_s_b
  | LenE exp_a, LenE exp_b -> eq_exp exp_a exp_b
  | DotE (exp_a, atom_a), DotE (exp_b, atom_b) ->
      eq_exp exp_a exp_b && eq_atom atom_a atom_b
  | IdxE (exp_b_a, exp_i_a), IdxE (exp_b_b, exp_i_b) ->
      eq_exp exp_b_a exp_b_b && eq_exp exp_i_a exp_i_b
  | SliceE (exp_b_a, exp_l_a, exp_h_a), SliceE (exp_b_b, exp_l_b, exp_h_b) ->
      eq_exp exp_b_a exp_b_b && eq_exp exp_l_a exp_l_b && eq_exp exp_h_a exp_h_b
  | UpdE (exp_b_a, path_a, exp_f_a), UpdE (exp_b_b, path_b, exp_f_b) ->
      eq_exp exp_b_a exp_b_b && eq_path path_a path_b && eq_exp exp_f_a exp_f_b
  | CallE func_call_a, CallE func_call_b -> eq_func_call func_call_a func_call_b
  | IterE (exp_a, iterexp_a), IterE (exp_b, iterexp_b) ->
      eq_exp exp_a exp_b && eq_iterexp iterexp_a iterexp_b
  | _ -> false

and eq_exps (exps_a : exp list) (exps_b : exp list) : bool =
  List.length exps_a = List.length exps_b && List.for_all2 eq_exp exps_a exps_b

(* Paths *)

and eq_path (path_a : path) (path_b : path) : bool =
  match (path_a.it, path_b.it) with
  | RootP, RootP -> true
  | IdxP (path_a, exp_a), IdxP (path_b, exp_b) ->
      eq_path path_a path_b && eq_exp exp_a exp_b
  | SliceP (path_a, exp_l_a, exp_h_a), SliceP (path_b, exp_l_b, exp_h_b) ->
      eq_path path_a path_b && eq_exp exp_l_a exp_l_b && eq_exp exp_h_a exp_h_b
  | DotP (path_a, atom_a), DotP (path_b, atom_b) ->
      eq_path path_a path_b && eq_atom atom_a atom_b
  | _ -> false

(* Arguments *)

and eq_arg (arg_a : arg) (arg_b : arg) : bool =
  match (arg_a.it, arg_b.it) with
  | ExpA exp_a, ExpA exp_b -> eq_exp exp_a exp_b
  | DefA id_a, DefA id_b -> eq_id id_a id_b
  | _ -> false

and eq_args (args_a : arg list) (args_b : arg list) : bool =
  List.length args_a = List.length args_b && List.for_all2 eq_arg args_a args_b
