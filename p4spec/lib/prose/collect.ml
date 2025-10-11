(* Traverses SL AST and collects prose hints *)

open Util.Source
open Sl.Ast
open Domain.Lib
open Runtime_static.Envs
module PEnv = Penv

let collect_rel_def (rid : RId.t) (penv : PEnv.t) hints : PEnv.t =
  let open El.Ast in
  List.fold_left
    (fun penv { hintid; hintexp } ->
      match hintid.it with
      | "prose_in" ->
          PEnv.
            { penv with prose_in = Hintenv.add_rel rid hintexp penv.prose_in }
      | "prose_out" ->
          PEnv.
            { penv with prose_out = Hintenv.add_rel rid hintexp penv.prose_out }
      | "prose_true" ->
          PEnv.
            {
              penv with
              prose_true = Hintenv.add_rel rid hintexp penv.prose_true;
            }
      | "prose_false" ->
          PEnv.
            {
              penv with
              prose_false = Hintenv.add_rel rid hintexp penv.prose_false;
            }
      | _ -> penv)
    penv hints

let collect_dec_def (fid : FId.t) (penv : PEnv.t) hints : PEnv.t =
  let open El.Ast in
  List.fold_left
    (fun penv { hintid; hintexp } ->
      match hintid.it with
      | "prose_in" ->
          PEnv.
            { penv with prose_in = Hintenv.add_func fid hintexp penv.prose_in }
      | "prose_out" ->
          PEnv.
            {
              penv with
              prose_out = Hintenv.add_func fid hintexp penv.prose_out;
            }
      | "prose_true" ->
          PEnv.
            {
              penv with
              prose_true = Hintenv.add_func fid hintexp penv.prose_true;
            }
      | "prose_false" ->
          PEnv.
            {
              penv with
              prose_false = Hintenv.add_func fid hintexp penv.prose_false;
            }
      | _ -> penv)
    penv hints

(* Collect hints into proseHintEnv *)

let collect_def (penv : PEnv.t) (ienv : IEnv.t) (def : def) : PEnv.t * IEnv.t =
  match def.it with
  | TypD _ -> (penv, ienv)
  | RelD (rid, (mixop, inputs), _, _, hints) ->
      let ienv = IEnv.add rid inputs ienv in
      (collect_rel_def rid penv hints, ienv)
  | DecD (fid, _, _, _, _, hints) -> (collect_dec_def fid penv hints, ienv)

let collect_spec (spec : spec) : PEnv.t * IEnv.t =
  List.fold_left
    (fun (penv, ienv) def -> collect_def penv ienv def)
    (PEnv.empty, IEnv.empty) spec
