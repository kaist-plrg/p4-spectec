open Domain
open Lib
open Lang
open Il
open Error
open Util.Source

(* Whether a value belongs to a type (including subtyping) *)

let rec sub_ (find_typdef_opt : TId.t -> Type.Typdef.t option)
    (find_func : FId.t -> tparam list * typ list * typ) (typ : typ)
    (value : value) : bool =
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
      let td = find_typdef_opt tid |> Option.get in
      match td with
      | Param | Defining _ -> error typ.at "unexpected type variable"
      | Extern -> ( match value.it with ExternV _ -> true | _ -> false)
      | Defined (tparams, deftyp) -> (
          match (deftyp, value.it) with
          | `Plain typ, _ ->
              let theta = TIdMap.of_lists tparams targs in
              let typ = Type.Subst.subst_typ theta typ in
              sub_ find_typdef_opt find_func typ value
          | `Struct typfields, StructV valuefields
            when List.length typfields = List.length valuefields ->
              let theta = TIdMap.of_lists tparams targs in
              List.for_all2
                (fun (atom_t, typ) (atom_v, value) ->
                  Atom.eq atom_t.it atom_v.it
                  &&
                  let typ = Type.Subst.subst_typ theta typ in
                  sub_ find_typdef_opt find_func typ value)
                typfields valuefields
          | `Variant (typcases, _), CaseV (mixop_v, values_inner) ->
              let theta = TIdMap.of_lists tparams targs in
              List.exists
                (fun typcase ->
                  let nottyp, _, _ = typcase in
                  let mixop_t, typs_inner = nottyp.it in
                  Mixop.eq mixop_t mixop_v
                  &&
                  let typs_inner = Type.Subst.subst_typs theta typs_inner in
                  subs_ find_typdef_opt find_func typs_inner values_inner)
                typcases
          | _ -> false))
  | TupleT typs -> (
      match value.it with
      | TupleV values ->
          List.length typs = List.length values
          && List.for_all2 (sub_ find_typdef_opt find_func) typs values
      | _ -> false)
  | IterT (typ_inner, Opt) -> (
      match value.it with
      | OptV value_opt -> (
          match value_opt with
          | Some value_inner ->
              sub_ find_typdef_opt find_func typ_inner value_inner
          | None -> true)
      | _ -> true)
  | IterT (typ_inner, List) -> (
      match value.it with
      | ListV values ->
          List.for_all (sub_ find_typdef_opt find_func typ_inner) values
      | _ -> false)
  | FuncT (tparams_t, typs_params_t, typ_ret_t) -> (
      match value.it with
      | FuncV fid ->
          let tparams_v, typs_params_v, typ_ret_v = find_func fid in
          Type.Equiv.equiv_functyp find_typdef_opt typ.at tparams_t
            typs_params_t typ_ret_t tparams_v typs_params_v typ_ret_v
      | _ -> false)

and subs_ (find_typdef_opt : TId.t -> Type.Typdef.t option)
    (find_func : FId.t -> tparam list * typ list * typ) (typs : typ list)
    (values : value list) : bool =
  List.length typs = List.length values
  && List.for_all2 (sub_ find_typdef_opt find_func) typs values

(* Entry point *)

let cache find_typdef_opt =
  let cache : (string, Type.Typdef.t option) Hashtbl.t = Hashtbl.create 8 in
  fun tid ->
    match Hashtbl.find_opt cache tid.it with
    | Some td_opt -> td_opt
    | None ->
        let td_opt = find_typdef_opt tid in
        Hashtbl.add cache tid.it td_opt;
        td_opt

let sub find_typdef_opt find_func typ value =
  sub_ (cache find_typdef_opt) find_func typ value

let subs find_typdef_opt find_func typs values =
  subs_ (cache find_typdef_opt) find_func typs values
