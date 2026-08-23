open Domain
open Lib
module Mixfix = Domain.Mixfix
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
          match (deftyp.it, value.it) with
          | PlainT typ, _ ->
              let theta = TIdMap.of_lists tparams targs in
              let typ = Type.Subst.subst_typ theta typ in
              sub_ find_typdef_opt find_func typ value
          | StructT typfields, StructV valuefields
            when List.length typfields = List.length valuefields ->
              let theta = TIdMap.of_lists tparams targs in
              List.for_all2
                (fun (atom_t, typ) (atom_v, value) ->
                  Atom.eq atom_t.it atom_v.it
                  &&
                  let typ = Type.Subst.subst_typ theta typ in
                  sub_ find_typdef_opt find_func typ value)
                typfields valuefields
          | VariantT typcases, CaseV valuecase ->
              let theta = TIdMap.of_lists tparams targs in
              let values = Mixfix.args valuecase in
              List.exists
                (fun (nottyp, _, _) ->
                  Mixfix.eq_mixop nottyp.it valuecase
                  &&
                  let nottyp = Type.Subst.subst_nottyp theta nottyp in
                  let typs = Mixfix.args nottyp.it in
                  subs_ find_typdef_opt find_func typs values)
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

(* Caches *)

(* Caching subtyping of type variables to values,
   using the pair of type variable and value id as key *)

type cache_sub_var = (string * int, bool) Hashtbl.t

(* Caching type definition finder *)

let cache_find_typdef_opt find_typdef_opt =
  let cache : (string, Type.Typdef.t option) Hashtbl.t = Hashtbl.create 8 in
  fun (tid : TId.t) ->
    match Hashtbl.find_opt cache tid.it with
    | Some td_opt -> td_opt
    | None ->
        let td_opt = find_typdef_opt tid in
        Hashtbl.add cache tid.it td_opt;
        td_opt

(* Sub-check with caching of type variables *)

let sub cache_sub_var find_typdef_opt find_func typ value =
  match typ.it with
  | VarT (tid, []) -> (
      let key = (tid.it, value.note.vid) in
      match Hashtbl.find_opt cache_sub_var key with
      | Some res -> res
      | None ->
          let res =
            sub_ (cache_find_typdef_opt find_typdef_opt) find_func typ value
          in
          Hashtbl.add cache_sub_var key res;
          res)
  | _ -> sub_ (cache_find_typdef_opt find_typdef_opt) find_func typ value

let subs find_typdef_opt find_func typs values =
  subs_ (cache_find_typdef_opt find_typdef_opt) find_func typs values

(* Entry point *)

let rec check cache_sub_var find_typdef_opt find_func subcheck value =
  match (subcheck, value.it) with
  | SkipSC, _ -> true
  | MixopSC mixops, CaseV valuecase ->
      List.exists (fun mixop -> Mixfix.eq_mixop mixop valuecase) mixops
  | TupleSC subchecks, TupleV values ->
      List.length subchecks = List.length values
      && List.for_all2
           (check cache_sub_var find_typdef_opt find_func)
           subchecks values
  | IterSC (Opt, _), OptV None -> true
  | IterSC (Opt, subcheck), OptV (Some value) ->
      check cache_sub_var find_typdef_opt find_func subcheck value
  | IterSC (List, subcheck), ListV values ->
      List.for_all
        (check cache_sub_var find_typdef_opt find_func subcheck)
        values
  | RecurseSC typ, _ -> sub cache_sub_var find_typdef_opt find_func typ value
  | _ -> false
