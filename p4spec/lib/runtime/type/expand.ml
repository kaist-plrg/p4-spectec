open Domain.Lib
open Lang
open Il
open Error
open Util.Source

(* Type expansion *)

let rec expand_typ (find_typdef_opt : TId.t -> Typdef.t option) (typ : typ) :
    typ =
  match typ.it with
  | VarT (tid, targs) -> (
      let td_opt = find_typdef_opt tid in
      match td_opt with
      | Some (Defined (tparams, deftyp)) -> (
          match deftyp with
          | `Plain _ when List.length targs <> List.length tparams ->
              error typ.at "type arguments do not match"
          | `Plain typ ->
              let theta = TIdMap.of_lists tparams targs in
              let typ = Subst.subst_typ theta typ in
              expand_typ find_typdef_opt typ
          | _ -> typ)
      | Some _ -> typ
      | None -> error typ.at ("type variable " ^ tid.it ^ " is not defined"))
  | _ -> typ
