open Domain.Lib
open Lang
open Xl
open Il
open Util.Source

let rec sub_typ (find_typdef_opt : TId.t -> Typdef.t option) (typ_a : typ)
    (typ_b : typ) : bool =
  Equiv.equiv_typ find_typdef_opt typ_a typ_b
  || sub_typ' find_typdef_opt typ_a typ_b

and sub_typ' (find_typdef_opt : TId.t -> Typdef.t option) (typ_a : typ)
    (typ_b : typ) : bool =
  let typ_a = Expand.expand_typ find_typdef_opt typ_a in
  let typ_b = Expand.expand_typ find_typdef_opt typ_b in
  match (typ_a.it, typ_b.it) with
  | NumT numtyp_a, NumT numtyp_b -> Num.sub numtyp_a numtyp_b
  | VarT (tid_a, targs_a), VarT (tid_b, targs_b) -> (
      let td_opt_a = find_typdef_opt tid_a in
      let td_opt_b = find_typdef_opt tid_b in
      match (td_opt_a, td_opt_b) with
      | ( Some (Defined (tparams_a, deftyp_a)),
          Some (Defined (tparams_b, deftyp_b)) ) -> (
          match (deftyp_a, deftyp_b) with
          | `Variant (typcases_a, _), `Variant (typcases_b, _) ->
              let theta_a = TIdMap.of_lists tparams_a targs_a in
              let theta_b = TIdMap.of_lists tparams_b targs_b in
              let nottyps_a =
                typcases_a
                |> List.map (fun (nottyp_a, _, _) ->
                       Subst.subst_nottyp theta_a nottyp_a)
              in
              let nottyps_b =
                typcases_b
                |> List.map (fun (nottyp_b, _, _) ->
                       Subst.subst_nottyp theta_b nottyp_b)
              in
              List.for_all
                (fun nottyp_a ->
                  List.exists
                    (Equiv.equiv_nottyp find_typdef_opt nottyp_a)
                    nottyps_b)
                nottyps_a
          | _, _ -> false)
      | _ -> false)
  | TupleT typs_a, TupleT typs_b ->
      List.length typs_a = List.length typs_b
      && List.for_all2 (sub_typ find_typdef_opt) typs_a typs_b
  | IterT (typ_a, iter_a), IterT (typ_b, iter_b) when iter_a = iter_b ->
      sub_typ find_typdef_opt typ_a typ_b
  | IterT (typ_a, Opt), IterT (typ_b, List) ->
      sub_typ find_typdef_opt typ_a typ_b
  | _, IterT (typ_b, Opt) -> sub_typ find_typdef_opt typ_a typ_b
  | _, IterT (typ_b, List) -> sub_typ find_typdef_opt typ_a typ_b
  | _ -> false
