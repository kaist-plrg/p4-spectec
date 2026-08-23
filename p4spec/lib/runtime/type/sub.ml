open Domain.Lib
module Mixfix = Domain.Mixfix
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
          match (deftyp_a.it, deftyp_b.it) with
          | VariantT typcases_a, VariantT typcases_b ->
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

(* Optimization of subtype checks :

    syntax value = NUM nat | TEXT text
    syntax number = NUM nat

    (value * value list) <: (number * number list)

    becomes

    TupleSC [ MixopSC [NUM]; IterSC (List, MixopSC [NUM]) ]

   Assumptions:

    - A SubE operand evaluates to a value of its static source type
      - Thus, every NUM payload above is already a nat *)

let rec optimize (find_typdef_opt : TId.t -> Typdef.t option)
    ~(typ_source : typ) ~(typ_target : typ) : subcheck =
  if sub_typ find_typdef_opt typ_source typ_target then SkipSC
  else
    let typ_source_expanded = Expand.expand_typ find_typdef_opt typ_source in
    let typ_target_expanded = Expand.expand_typ find_typdef_opt typ_target in
    match (typ_source_expanded.it, typ_target_expanded.it) with
    | TupleT typs_source, TupleT typs_target
      when List.length typs_source = List.length typs_target ->
        let subchecks =
          List.map2
            (fun typ_source typ_target ->
              optimize find_typdef_opt ~typ_source ~typ_target)
            typs_source typs_target
        in
        TupleSC subchecks
    | IterT (typ_source, iter_source), IterT (typ_target, iter_target)
      when iter_source = iter_target ->
        let subcheck = optimize find_typdef_opt ~typ_source ~typ_target in
        IterSC (iter_source, subcheck)
    | VarT (tid_source, _), VarT (tid_target, _)
      when sub_typ find_typdef_opt typ_target typ_source -> (
        match (find_typdef_opt tid_source, find_typdef_opt tid_target) with
        | Some (Defined (_, deftyp_source)), Some (Defined (_, deftyp_target))
          -> (
            match (deftyp_source.it, deftyp_target.it) with
            | VariantT _, VariantT typcases_target ->
                let mixops_target =
                  List.map
                    (fun (nottyp, _, _) -> Mixfix.to_mixop nottyp.it)
                    typcases_target
                in
                MixopSC mixops_target
            | _ -> RecurseSC typ_target)
        | _ -> RecurseSC typ_target)
    | _ -> RecurseSC typ_target
