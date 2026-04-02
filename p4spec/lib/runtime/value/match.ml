open Domain
open Lib
open Lang
open Il
open Error
open Util.Source

(* Whether a value belongs to a type (including subtyping) *)

type func_signature = tparam list * typ list * typ

let rec sub (type_finder : TId.t -> Type.Typdef.t)
    (func_finder : FId.t -> func_signature) (typ : typ) (value : value) : bool =
  match typ.it with
  | BoolT -> ( match value.it with BoolV _ -> true | _ -> false)
  | NumT `NatT -> (
      match value.it with
      | NumV (`Nat _) -> true
      | NumV (`Int i) -> Bigint.(i >= zero)
      | _ -> false)
  | NumT `IntT -> ( match value.it with NumV _ -> true | _ -> false)
  | TextT -> ( match value.it with TextV _ -> true | _ -> false)
  | VarT (tid, targs) -> (
      let td = type_finder tid in
      match td with
      | Param | Defining _ -> error typ.at "unexpected type variable"
      | Extern -> ( match value.it with ExternV _ -> true | _ -> false)
      | Defined (tparams, deftyp) -> (
          let theta = List.combine tparams targs |> TIdMap.of_list in
          match (deftyp.it, value.it) with
          | PlainT typ, _ ->
              let typ = Type.Subst.subst_typ theta typ in
              sub type_finder func_finder typ value
          | StructT typfields, StructV valuefields
            when List.length typfields = List.length valuefields ->
              List.for_all2
                (fun (atom_t, typ) (atom_v, value) ->
                  Atom.eq atom_t.it atom_v.it
                  &&
                  let typ = Type.Subst.subst_typ theta typ in
                  sub type_finder func_finder typ value)
                typfields valuefields
          | VariantT typcases, CaseV (mixop_v, values_inner) ->
              List.exists
                (fun typcase ->
                  let nottyp, _, _ = typcase in
                  let mixop_t, typs_inner = nottyp.it in
                  Mixop.eq mixop_t mixop_v
                  &&
                  let typs_inner =
                    List.map (Type.Subst.subst_typ theta) typs_inner
                  in
                  subs type_finder func_finder typs_inner values_inner)
                typcases
          | _ -> false))
  | TupleT typs -> (
      match value.it with
      | TupleV values ->
          List.length typs = List.length values
          && List.for_all2 (sub type_finder func_finder) typs values
      | _ -> false)
  | IterT (typ_inner, Opt) -> (
      match value.it with
      | OptV value_opt -> (
          match value_opt with
          | Some value_inner ->
              sub type_finder func_finder typ_inner value_inner
          | None -> true)
      | _ -> true)
  | IterT (typ_inner, List) -> (
      match value.it with
      | ListV values ->
          List.for_all (sub type_finder func_finder typ_inner) values
      | _ -> false)
  | FuncT (_tparams_t, _typs_param_t, _typ_ret_t) -> (
      match value.it with
      | FuncV fid ->
          let _tparams_v, _typs_param_v, _typ_ret_v = func_finder fid in
          true
      | _ -> false)

and subs (type_finder : TId.t -> Type.Typdef.t)
    (func_finder : FId.t -> func_signature) (typs : typ list)
    (values : value list) : bool =
  List.length typs = List.length values
  && List.for_all2 (sub type_finder func_finder) typs values
