open Domain.Lib
open Lang
open Il
open Runtime.Type
open Runtime.Static
open Envs
open Util.Source

(* Context for binding analysis *)

type t = {
  (* Free identifiers over the entire definition *)
  frees : IdSet.t;
  (* Bound variables so far *)
  venv : VEnv.t;
  (* Typedefs so far *)
  tdenv : TDEnv.t;
  (* Metavariables so far *)
  menv : MEnv.t;
}

(* Constructors *)

let empty =
  {
    frees = IdSet.empty;
    venv = VEnv.empty;
    tdenv = TDEnv.empty;
    menv = MEnv.empty;
  }

let init () : t =
  let menv =
    MEnv.empty
    |> MEnv.add ("bool" $ no_region) (Il.BoolT $ no_region)
    |> MEnv.add ("nat" $ no_region) (Il.NumT `NatT $ no_region)
    |> MEnv.add ("int" $ no_region) (Il.NumT `IntT $ no_region)
    |> MEnv.add ("text" $ no_region) (Il.TextT $ no_region)
  in
  { empty with menv }

(* Adders *)

let add_free (ctx : t) (id : Id.t) : t =
  let frees = IdSet.add id ctx.frees in
  { ctx with frees }

let add_frees (ctx : t) (ids : IdSet.t) : t =
  let frees = IdSet.union ids ctx.frees in
  { ctx with frees }

let add_bounds (ctx : t) (venv : VEnv.t) : t =
  let venv = VEnv.union (fun _ typ _ -> Some typ) ctx.venv venv in
  { ctx with venv }

(* Finders *)

let find_typdef_opt (ctx : t) (tid : TId.t) : Typdef.t option =
  TDEnv.find_opt tid ctx.tdenv

let find_typdef (ctx : t) (tid : TId.t) : Typdef.t = TDEnv.find tid ctx.tdenv

(* Load type definitions *)

let load_def (ctx : t) (def : def) : t =
  match def.it with
  | ExternTypD (id, _) ->
      let tdenv = TDEnv.add id Typdef.Extern ctx.tdenv in
      { ctx with tdenv }
  | TypD (id, tparams, deftyp, _) ->
      let tdenv = TDEnv.add id (Typdef.Defined (tparams, deftyp)) ctx.tdenv in
      { ctx with tdenv }
  | VarD (id, typ, _) ->
      let menv = MEnv.add id typ ctx.menv in
      { ctx with menv }
  | _ -> ctx

let load_spec (ctx : t) (spec : spec) : t = List.fold_left load_def ctx spec
