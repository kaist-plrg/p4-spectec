open Domain.Lib
open Lang
open Il
open Error
open Util.Source

(* Type expansion *)

let rec expand_typ (finder : TId.t -> Typdef.t option) (typ : typ) : typ =
  match typ.it with
  | VarT (tid, targs) -> (
      let td_opt = finder tid in
      match td_opt with
      | Some (Defined (tparams, deftyp)) -> (
          match deftyp.it with
          | PlainT _ when List.length targs <> List.length tparams ->
              error typ.at "type arguments do not match"
          | PlainT typ ->
              let theta = List.combine tparams targs |> TIdMap.of_list in
              let typ = Subst.subst_typ theta typ in
              expand_typ finder typ
          | _ -> typ)
      | Some _ -> typ
      | None -> error typ.at ("type variable " ^ tid.it ^ " is not defined"))
  | _ -> typ
