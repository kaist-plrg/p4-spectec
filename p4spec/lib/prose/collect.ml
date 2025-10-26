(* Traverses SL AST and collects prose hints *)

open Util.Source
open Sl.Ast
open Domain.Lib
open Runtime_static.Envs

let collect_def (defid : Hintdb.def_id) (hdb : Hintdb.t) hints : Hintdb.t =
  let open El.Ast in
  List.fold_left
    (fun hdb { hintid; hintexp } ->
      match hintid.it with
      | "prose_in" | "prose_out" | "prose_true" | "prose_false" ->
          Hintdb.add hintid.it defid hintexp hdb
      | _ -> hdb)
    hdb hints

(* Collect hints into proseHintEnv *)

let collect_defs (hdb : Hintdb.t) (ienv : IEnv.t) (def : def) :
    Hintdb.t * IEnv.t =
  match def.it with
  | TypD (tid, _, deftyp, hints) -> (collect_def (`Typ tid) hdb hints, ienv)
  | RelD (rid, (mixop, inputs), _, _, hints) ->
      let ienv = IEnv.add rid inputs ienv in
      (collect_def (`Rel rid) hdb hints, ienv)
  | DecD (fid, _, _, _, _, hints) -> (collect_def (`Func fid) hdb hints, ienv)

let collect_spec (spec : spec) : Hintdb.t * IEnv.t =
  List.fold_left
    (fun (hdb, ienv) def -> collect_defs hdb ienv def)
    (Hintdb.empty, IEnv.empty) spec
