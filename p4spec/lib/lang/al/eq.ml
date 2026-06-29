open Ast

(* Identifiers *)

let eq_id (id_a : id) (id_b : id) : bool = Il.Eq.eq_id id_a id_b

(* Atoms *)

let eq_atom (atom_a : atom) (atom_b : atom) : bool = Il.Eq.eq_atom atom_a atom_b

let eq_atoms (atoms_a : atom list) (atoms_b : atom list) : bool =
  Il.Eq.eq_atoms atoms_a atoms_b

(* Mixfix operators *)

let eq_mixop (mixop_a : mixop) (mixop_b : mixop) : bool =
  Il.Eq.eq_mixop mixop_a mixop_b

(* Iterators *)

let eq_iter (iter_a : iter) (iter_b : iter) : bool = Il.Eq.eq_iter iter_a iter_b

let eq_iters (iters_a : iter list) (iters_b : iter list) : bool =
  Il.Eq.eq_iters iters_a iters_b

(* Variables *)

let eq_var (var_a : var) (var_b : var) : bool = Il.Eq.eq_var var_a var_b

let eq_vars (vars_a : var list) (vars_b : var list) : bool =
  Il.Eq.eq_vars vars_a vars_b

(* Types *)

let eq_typ (typ_a : typ) (typ_b : typ) : bool = Il.Eq.eq_typ typ_a typ_b

let eq_typs (typs_a : typ list) (typs_b : typ list) : bool =
  Il.Eq.eq_typs typs_a typs_b

let eq_nottyp (nottyp_a : nottyp) (nottyp_b : nottyp) : bool =
  Il.Eq.eq_nottyp nottyp_a nottyp_b

(* Values *)

let eq_value ?(dbg = false) (value_a : value) (value_b : value) : bool =
  Il.Eq.eq_value ~dbg value_a value_b

let eq_values ?(dbg = false) (values_a : value list) (values_b : value list) :
    bool =
  Il.Eq.eq_values ~dbg values_a values_b

(* Expressions *)

let eq_exp (exp_a : exp) (exp_b : exp) : bool = Il.Eq.eq_exp exp_a exp_b

let eq_exps (exps_a : exp list) (exps_b : exp list) : bool =
  Il.Eq.eq_exps exps_a exps_b

let eq_iterexp (iterexp_a : iterexp) (iterexp_b : iterexp) : bool =
  Il.Eq.eq_iterexp iterexp_a iterexp_b

let eq_iterexps (iterexps_a : iterexp list) (iterexps_b : iterexp list) : bool =
  Il.Eq.eq_iterexps iterexps_a iterexps_b

(* Patterns *)

let eq_pattern (pattern_a : pattern) (pattern_b : pattern) : bool =
  Il.Eq.eq_pattern pattern_a pattern_b

(* Paths *)

let eq_path (path_a : path) (path_b : path) : bool = Il.Eq.eq_path path_a path_b

(* Type parameters *)

let eq_tparam (tparam_a : tparam) (tparam_b : tparam) : bool =
  Il.Eq.eq_tparam tparam_a tparam_b

let eq_tparams (tparams_a : tparam list) (tparams_b : tparam list) : bool =
  Il.Eq.eq_tparams tparams_a tparams_b

(* Arguments *)

let eq_arg (arg_a : arg) (arg_b : arg) : bool = Il.Eq.eq_arg arg_a arg_b

let eq_args (args_a : arg list) (args_b : arg list) : bool =
  Il.Eq.eq_args args_a args_b

(* Type arguments *)

let eq_targ (targ_a : targ) (targ_b : targ) : bool = Il.Eq.eq_targ targ_a targ_b

let eq_targs (targs_a : targ list) (targs_b : targ list) : bool =
  Il.Eq.eq_targs targs_a targs_b

(* Premises *)

let eq_prem (prem_a : prem) (prem_b : prem) : bool = Il.Eq.eq_prem prem_a prem_b
