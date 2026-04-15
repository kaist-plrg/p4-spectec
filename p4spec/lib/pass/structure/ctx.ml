open Domain.Lib
open Lang
open Il
open Runtime.Type
open Runtime.Static
open Envs
open Error
open Util.Source

(* Error *)

let error_undef (at : region) (kind : string) (id : string) =
  error at (Format.asprintf "%s `%s` is undefined" kind id)

let error_dup (at : region) (kind : string) (id : string) =
  error at (Format.asprintf "%s `%s` was already defined" kind id)

(* Context *)

type t = {
  (* Map from syntax ids to type definitions *)
  tdenv : TDEnv.t;
  (* Map from meta-variable ids to types *)
  menv : MEnv.t;
}

let empty : t = { tdenv = TDEnv.empty; menv = MEnv.empty }

let init () : t =
  let menv =
    MEnv.empty
    |> MEnv.add ("bool" $ no_region) (Il.BoolT $ no_region)
    |> MEnv.add ("nat" $ no_region) (Il.NumT `NatT $ no_region)
    |> MEnv.add ("int" $ no_region) (Il.NumT `IntT $ no_region)
    |> MEnv.add ("text" $ no_region) (Il.TextT $ no_region)
  in
  { empty with menv }

(* Finders *)

(* Finders for type definitions *)

let find_typdef_opt (ctx : t) (tid : TId.t) : Typdef.t option =
  TDEnv.find_opt tid ctx.tdenv

let find_typdef (ctx : t) (tid : TId.t) : Typdef.t =
  match find_typdef_opt ctx tid with
  | Some td -> td
  | None -> error_undef tid.at "type" tid.it

let bound_typdef (ctx : t) (tid : TId.t) : bool =
  find_typdef_opt ctx tid |> Option.is_some

(* Finders for meta-variables *)

let find_metavar_opt (ctx : t) (tid : TId.t) : Typ.t option =
  MEnv.find_opt tid ctx.menv

let find_metavar (ctx : t) (tid : TId.t) : Typ.t =
  match find_metavar_opt ctx tid with
  | Some typ -> typ
  | None -> error_undef tid.at "meta-variable" tid.it

let bound_metavar (ctx : t) (tid : TId.t) : bool =
  find_metavar_opt ctx tid |> Option.is_some

(* Adders *)

(* Adders for meta-variables *)

let add_metavar (ctx : t) (tid : TId.t) (typ : Typ.t) : t =
  if bound_metavar ctx tid then error_dup tid.at "meta-variable" tid.it;
  let menv = MEnv.add tid typ ctx.menv in
  { ctx with menv }

(* Adders for type definitions *)

let add_typdef (ctx : t) (tid : TId.t) (td : Typdef.t) : t =
  if bound_typdef ctx tid then error_dup tid.at "type" tid.it;
  let tdenv = TDEnv.add tid td ctx.tdenv in
  { ctx with tdenv }

(* Load type definitions *)

let load_def (ctx : t) (def : def) : t =
  match def.it with
  | ExternTypD (id, _hints) ->
      let typ = Typ.Make.var id [] in
      let ctx = add_metavar ctx id typ in
      let td = Typdef.Extern in
      add_typdef ctx id td
  | TypD (id, tparams, deftyp, _hints) ->
      let ctx =
        if tparams = [] then
          let typ = Typ.Make.var id [] in
          add_metavar ctx id typ
        else ctx
      in
      let td = Typdef.of_deftyp tparams deftyp in
      add_typdef ctx id td
  | VarD (id, typ, _hints) -> add_metavar ctx id typ
  | _ -> ctx

let load_spec (ctx : t) (spec : spec) : t = List.fold_left load_def ctx spec
