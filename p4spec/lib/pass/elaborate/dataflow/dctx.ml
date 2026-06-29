open Domain.Lib
open Lang
open Il
open Runtime.Type
open Runtime.Static
open Envs
open Util.Source

(* Context for dataflow analysis *)

type t = {
  (* Free identifiers over the entire definition *)
  frees : IdSet.t;
  (* Bound variables so far *)
  bounds : VEnv.t;
  (* Metavariables so far *)
  menv : MEnv.t;
  (* Typedefs so far *)
  tdenv : TDEnv.t;
}

(* Constructors *)

let empty =
  {
    frees = IdSet.empty;
    bounds = VEnv.empty;
    menv = MEnv.empty;
    tdenv = TDEnv.empty;
  }

let init (spec : spec) : t =
  List.fold_left
    (fun dctx def ->
      match def.it with
      | VarD (id, typ, _) -> { dctx with menv = MEnv.add id typ dctx.menv }
      | TypD (id, tparams, deftyp, _) ->
          {
            dctx with
            tdenv = TDEnv.add id (Typdef.Defined (tparams, deftyp)) dctx.tdenv;
          }
      | ExternTypD (id, _) ->
          { dctx with tdenv = TDEnv.add id Typdef.Extern dctx.tdenv }
      | _ -> dctx)
    empty spec

(* Adders *)

let add_free (dctx : t) (id : Id.t) : t =
  let frees = IdSet.add id dctx.frees in
  { dctx with frees }

let add_frees (dctx : t) (ids : IdSet.t) : t =
  let frees = IdSet.union ids dctx.frees in
  { dctx with frees }

let add_bounds (dctx : t) (venv : VEnv.t) : t =
  let bounds = VEnv.union (fun _ typ _ -> Some typ) dctx.bounds venv in
  { dctx with bounds }

(* Finders *)

let find_typdef (dctx : t) (tid : TId.t) : Typdef.t = TDEnv.find tid dctx.tdenv
