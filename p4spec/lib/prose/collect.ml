(** * Traverses SL AST and collects prose hints *)

open Util.Source
open Sl.Ast
open Domain.Lib
open Ctx
open Runtime_static.Envs

let collect_rel_def (rid : RId.t) (penv : penv) hints : penv =
  let open El.Ast in
  List.fold_left
    (fun penv { hintid; hintexp } ->
      match hintid.it with
      | "prose" -> { prose = Hintenv.add_rel rid hintexp penv.prose }
      | _ -> penv)
    penv hints

let collect_dec_def (fid : FId.t) (penv : penv) hints : penv =
  let open El.Ast in
  List.fold_left
    (fun penv { hintid; hintexp } ->
      match hintid.it with
      | "prose" -> { prose = Hintenv.add_func fid hintexp penv.prose }
      | _ -> penv)
    penv hints

(* Collect hints into proseHintEnv *)

let collect_def (penv : penv) (ienv : IEnv.t) (def : def) : penv * IEnv.t =
  match def.it with
  | TypD _ -> (penv, ienv)
  | RelD (rid, (mixop, inputs), _, _, hints) ->
      let ienv = IEnv.add rid inputs ienv in
      (collect_rel_def rid penv hints, ienv)
  | DecD (fid, _, _, _, hints) -> (collect_dec_def fid penv hints, ienv)

let collect_spec (spec : spec) : penv * IEnv.t =
  List.fold_left
    (fun (penv, ienv) def -> collect_def penv ienv def)
    (empty_penv, IEnv.empty) spec
