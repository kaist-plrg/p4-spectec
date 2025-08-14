open Domain.Lib
open Xl
open El.Ast
open Runtime_static
open Error
open Util.Source

(* Type equivalence and subtyping *)

let rec equiv_typ (tdenv : Envs.TDEnv.t) (typ_a : typ) (typ_b : typ) : bool =
  match (typ_a, typ_b) with
  | PlainT plaintyp_a, PlainT plaintyp_b ->
      equiv_plaintyp tdenv plaintyp_a plaintyp_b
  | NotationT nottyp_a, NotationT nottyp_b ->
      equiv_nottyp tdenv nottyp_a nottyp_b
  | _ -> false

and equiv_plaintyp (tdenv : Envs.TDEnv.t) (plaintyp_a : plaintyp)
    (plaintyp_b : plaintyp) : bool =
  let plaintyp_a = Plaintyp.expand_plaintyp tdenv plaintyp_a in
  let plaintyp_b = Plaintyp.expand_plaintyp tdenv plaintyp_b in
  match (plaintyp_a.it, plaintyp_b.it) with
  | BoolT, BoolT -> true
  | NumT numtyp_a, NumT numtyp_b -> Num.equiv numtyp_a numtyp_b
  | TextT, TextT -> true
  | VarT (tid_a, targs_a), VarT (tid_b, targs_b) ->
      tid_a.it = tid_b.it
      && List.length targs_a = List.length targs_b
      && List.for_all2 (equiv_plaintyp tdenv) targs_a targs_b
  | ParenT plaintyp_a, _ -> equiv_plaintyp tdenv plaintyp_a plaintyp_b
  | _, ParenT plaintyp_b -> equiv_plaintyp tdenv plaintyp_a plaintyp_b
  | TupleT plaintyps_a, TupleT plaintyps_b ->
      List.length plaintyps_a = List.length plaintyps_b
      && List.for_all2 (equiv_plaintyp tdenv) plaintyps_a plaintyps_b
  | IterT (plaintyp_a, iter_a), IterT (plaintyp_b, iter_b) ->
      equiv_plaintyp tdenv plaintyp_a plaintyp_b && iter_a = iter_b
  | _ -> false

and equiv_nottyp (tdenv : Envs.TDEnv.t) (nottyp_a : nottyp) (nottyp_b : nottyp)
    : bool =
  match (nottyp_a.it, nottyp_b.it) with
  | AtomT atom_a, AtomT atom_b -> atom_a.it = atom_b.it
  | SeqT typs_a, SeqT typs_b ->
      List.length typs_a = List.length typs_b
      && List.for_all2 (equiv_typ tdenv) typs_a typs_b
  | InfixT (typ_l_a, atom_a, typ_r_a), InfixT (typ_l_b, atom_b, typ_r_b) ->
      equiv_typ tdenv typ_l_a typ_l_b
      && atom_a.it = atom_b.it
      && equiv_typ tdenv typ_r_a typ_r_b
  | BrackT (atom_l_a, typ_a, atom_r_a), BrackT (atom_l_b, typ_b, atom_r_b) ->
      atom_l_a.it = atom_l_b.it
      && equiv_typ tdenv typ_a typ_b
      && atom_r_a.it = atom_r_b.it
  | _ -> false

and equiv_param (tdenv : Envs.TDEnv.t) (param_a : param) (param_b : param) :
    bool =
  match (param_a.it, param_b.it) with
  | ExpP plaintyp_a, ExpP plaintyp_b ->
      equiv_plaintyp tdenv plaintyp_a plaintyp_b
  | ( DefP (_, tparams_a, params_a, plaintyp_a),
      DefP (_, tparams_b, params_b, plaintyp_b) ) ->
      equiv_functyp tdenv param_a.at tparams_a params_a plaintyp_a tparams_b
        params_b plaintyp_b
  | _ -> false

and equiv_functyp (tdenv : Envs.TDEnv.t) (at : region) (tparams_a : tparam list)
    (params_a : param list) (plaintyp_a : plaintyp) (tparams_b : tparam list)
    (params_b : param list) (plaintyp_b : plaintyp) : bool =
  check
    (List.length tparams_a = List.length tparams_b)
    no_region "type parameters do not match";
  let tdenv, theta_a, theta_b =
    List.fold_left2
      (fun (tdenv, theta_a, theta_b) tparam_a tparam_b ->
        let tid_fresh = "__FRESH" ^ string_of_int (Ctx.fresh ()) $ no_region in
        let plaintyp_fresh = VarT (tid_fresh, []) $ no_region in
        let tdenv = Envs.TDEnv.add tid_fresh Typdef.Param tdenv in
        let theta_a = TIdMap.add tparam_a plaintyp_fresh theta_a in
        let theta_b = TIdMap.add tparam_b plaintyp_fresh theta_b in
        (tdenv, theta_a, theta_b))
      (tdenv, TIdMap.empty, TIdMap.empty)
      tparams_a tparams_b
  in
  check
    (List.length params_a = List.length params_b)
    at "parameters do not match";
  let params_a = Plaintyp.subst_params theta_a params_a in
  let params_b = Plaintyp.subst_params theta_b params_b in
  let plaintyp_a = Plaintyp.subst_plaintyp theta_a plaintyp_a in
  let plaintyp_b = Plaintyp.subst_plaintyp theta_b plaintyp_b in
  List.for_all2 (equiv_param tdenv) params_a params_b
  && equiv_plaintyp tdenv plaintyp_a plaintyp_b
