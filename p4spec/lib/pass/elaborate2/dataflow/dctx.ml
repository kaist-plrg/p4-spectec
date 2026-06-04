open Domain.Lib
open Lang
open Il2
open Runtime.Type
open Runtime.Static
open Envs
open Util.Source

(* Context for binding analysis.
   Built from the spec directly (no global Ctx.t available in elaborate2). *)

type t = {
  (* Free identifiers over the clause — used for fresh name generation *)
  frees : IdSet.t;
  (* Variables bound so far *)
  bounds : VEnv.t;
  (* Meta-variables (VarD entries) — needed for Fresh.var_from_exp *)
  menv : MEnv.t;
  (* Type definitions — needed for is_singleton_case in partialbind *)
  tdenv : TDEnv.t;
}

(* Build menv and tdenv by scanning the spec once *)

let build_spec_env (spec : spec) : MEnv.t * TDEnv.t =
  List.fold_left
    (fun (menv, tdenv) def ->
      match def.it with
      | VarD (id, typ, _) ->
          let menv = MEnv.add id typ menv in
          (menv, tdenv)
      | TypD (id, tparams, deftyp, _) ->
          let tdenv = TDEnv.add id (Typdef.Defined (tparams, deftyp)) tdenv in
          (menv, tdenv)
      | ExternTypD (id, _) ->
          let tdenv = TDEnv.add id Typdef.Extern tdenv in
          (menv, tdenv)
      | _ -> (menv, tdenv))
    (MEnv.empty, TDEnv.empty) spec

(* Init a fresh dctx for one clause *)

let init_clause (menv : MEnv.t) (tdenv : TDEnv.t) (clause : clause) : t =
  let frees = Free.free_clause clause in
  { frees; bounds = VEnv.empty; menv; tdenv }

(* Adders *)

let add_free (dctx : t) (id : Id.t) : t =
  let frees = IdSet.add id dctx.frees in
  { dctx with frees }

let add_bounds (dctx : t) (venv : VEnv.t) : t =
  let bounds = VEnv.union (fun _ typ _ -> Some typ) dctx.bounds venv in
  { dctx with bounds }

(* Finders *)

let find_typdef (dctx : t) (tid : TId.t) : Typdef.t =
  TDEnv.find tid dctx.tdenv
