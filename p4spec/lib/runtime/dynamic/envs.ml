open Domain.Lib
open Lang
open Util.Source

(* Variable environment functor *)

module VarMap = struct
  include Map.Make (Var)
end

module MakeVarEnv (V : sig
  type t

  val to_string : t -> string
end) =
struct
  include VarMap

  type t = V.t VarMap.t

  let to_string env =
    let to_string_binding (var, v) =
      Var.to_string var ^ " : " ^ V.to_string v
    in
    let bindings = bindings env in
    String.concat ", " (List.map to_string_binding bindings)
end

module VEnv = MakeVarEnv (Value)

(* Type definition environment *)

module TDEnv = struct
  include MakeTIdEnv (Typdef)

  let rec unroll (tdenv : t) (typ : Il.typ) : Il.typ =
    match typ.it with
    | VarT (tid, targs) -> (
        let td = find tid tdenv in
        match td with
        | Param | Extern -> typ
        | Defined (tparams, deftyp) -> (
            let theta = List.combine tparams targs |> TIdMap.of_list in
            match deftyp.it with
            | PlainT typ ->
                let typ = Typ.subst_typ theta typ in
                unroll tdenv typ
            | _ -> typ))
    | _ -> typ
end

module TDTbl = MakeTIdTbl (Typdef)
