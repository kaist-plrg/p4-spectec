(**
 * Traverses SL AST and collects prose hints
 *)

open Util.Source
open Sl.Ast
open Domain.Lib

type penv = {
  prose: Hintenv.t;
}
let empty_penv = {
  prose = Hintenv.empty;
}

let collect_rel_def (rid : RId.t) (penv : penv) hints : penv =
  let open El.Ast in
  List.fold_left (fun penv {hintid; hintexp} ->
    match hintid.it with
    | "prose" -> { prose = Hintenv.update_rel rid hintexp penv.prose }
    | _ -> penv
  ) penv hints

let collect_dec_def (fid : FId.t) (penv : penv) hints : penv =
  let open El.Ast in
  List.fold_left (fun penv {hintid; hintexp} ->
    match hintid.it with
    | "prose" -> { prose = Hintenv.update_rel fid hintexp penv.prose }
    | _ -> penv
  ) penv hints

(* Collect hints into proseHintEnv *)

let collect_def (penv : penv) (def : def) : penv =
  match def.it with
  | TypD _ -> penv
  | RelD (rid, _, _, _, hints) ->
      collect_rel_def rid penv hints
  | DecD (fid, _, _, _, hints) ->
      collect_dec_def fid penv hints

let collect_spec (spec : spec) : penv = List.fold_left collect_def empty_penv spec
