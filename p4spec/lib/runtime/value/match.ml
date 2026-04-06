open Domain
open Lib
open Lang
open Il
open Error
open Util.Source

(* Whether a value belongs to a type (including subtyping)

   Using type annotations on values is used to short-circuit the subtyping check,
   but may be unsound if the type annotations are incorrect
   Short-circuiting is used as a compromise to avoid expensive subtyping checks
   at the cost of potentially unsound behavior *)

let rec sub_short_circuit (typ_a : typ') (typ_b : typ') : bool =
  match (typ_a, typ_b) with
  | BoolT, BoolT | TextT, TextT -> true
  | NumT numtyp_a, NumT numtyp_b -> numtyp_a = numtyp_b
  | VarT (id_a, targs_a), VarT (id_b, targs_b) ->
      id_a.it = id_b.it
      && List.compare_lengths targs_a targs_b = 0
      && List.for_all2
           (fun typ_a typ_b -> sub_short_circuit typ_a.it typ_b.it)
           targs_a targs_b
  | TupleT typs_a, TupleT typs_b ->
      List.compare_lengths typs_a typs_b = 0
      && List.for_all2
           (fun typ_a typ_b -> sub_short_circuit typ_a.it typ_b.it)
           typs_a typs_b
  | IterT (typ_a, iter_a), IterT (typ_b, iter_b) ->
      iter_a = iter_b && sub_short_circuit typ_a.it typ_b.it
  | _ -> false

let rec sub (find_typdef_opt : TId.t -> Type.Typdef.t option)
    (find_func : FId.t -> tparam list * typ list * typ) (typ : typ)
    (value : value) : bool =
  sub_short_circuit value.note.typ typ.it
  ||
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
          let theta = TIdMap.of_lists tparams targs in
          match (deftyp.it, value.it) with
          | PlainT typ, _ ->
              let typ = Type.Subst.subst_typ theta typ in
              sub find_typdef_opt find_func typ value
          | StructT typfields, StructV valuefields
            when List.length typfields = List.length valuefields ->
              List.for_all2
                (fun (atom_t, typ) (atom_v, value) ->
                  Atom.eq atom_t.it atom_v.it
                  &&
                  let typ = Type.Subst.subst_typ theta typ in
                  sub find_typdef_opt find_func typ value)
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
                  subs find_typdef_opt find_func typs_inner values_inner)
                typcases
          | _ -> false))
  | TupleT typs -> (
      match value.it with
      | TupleV values ->
          List.length typs = List.length values
          && List.for_all2 (sub find_typdef_opt find_func) typs values
      | _ -> false)
  | IterT (typ_inner, Opt) -> (
      match value.it with
      | OptV value_opt -> (
          match value_opt with
          | Some value_inner ->
              sub find_typdef_opt find_func typ_inner value_inner
          | None -> true)
      | _ -> true)
  | IterT (typ_inner, List) -> (
      match value.it with
      | ListV values ->
          List.for_all (sub find_typdef_opt find_func typ_inner) values
      | _ -> false)
  | FuncT (tparams_t, typs_params_t, typ_ret_t) -> (
      match value.it with
      | FuncV fid ->
          let tparams_v, typs_params_v, typ_ret_v = find_func fid in
          Type.Equiv.equiv_functyp find_typdef_opt typ.at tparams_t
            typs_params_t typ_ret_t tparams_v typs_params_v typ_ret_v
      | _ -> false)

and subs (find_typdef_opt : TId.t -> Type.Typdef.t option)
    (find_func : FId.t -> tparam list * typ list * typ) (typs : typ list)
    (values : value list) : bool =
  List.length typs = List.length values
  && List.for_all2 (sub find_typdef_opt find_func) typs values
