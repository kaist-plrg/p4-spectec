(* Traverses SL AST and collects prose hints *)

open Util.Source
open Sl.Ast
open Xl
open Domain.Lib
open Runtime_static.Envs

let collect_hints (defid : Hintdb.def_id) (hdb : Hintdb.t) hints : Hintdb.t =
  let open El.Ast in
  List.fold_left
    (fun hdb { hintid; hintexp } ->
      match hintid.it with
      | "prose" | "prose_in" | "prose_out" | "prose_true" | "prose_false" ->
          Hintdb.add hintid.it defid hintexp hdb
      | _ -> hdb)
    hdb hints

let collect_typcases (tid : TId.t) (hdb : Hintdb.t) (typcases : typcase list) :
    Hintdb.t =
  List.fold_left
    (fun hdb (nottyp, hints) ->
      let mixop = fst nottyp.it in
      collect_hints (`Typ (tid, mixop)) hdb hints)
    hdb typcases

(* Collect hints into proseHintEnv *)

let collect_defs (hdb : Hintdb.t) (ienv : IEnv.t) (def : def) :
    Hintdb.t * IEnv.t =
  match def.it with
  | TypD (tid, _, deftyp, hints) -> (
      match deftyp.it with
      | VariantT typcases -> (collect_typcases tid hdb typcases, ienv)
      | _ -> (hdb, ienv))
  | RelD (rid, (mixop, inputs), _, _, hints) ->
      let ienv = IEnv.add rid inputs ienv in
      (collect_hints (`Rel rid) hdb hints, ienv)
  | DecD (fid, _, _, _, _, hints) -> (collect_hints (`Func fid) hdb hints, ienv)

let collect_spec (spec : spec) : Hintdb.t * IEnv.t =
  List.fold_left
    (fun (hdb, ienv) def -> collect_defs hdb ienv def)
    (Hintdb.empty, IEnv.empty) spec
