open Domain.Lib
open Lang
open Ol.Ast
open Util.Source

(* Helper for renaming identifiers in expressions and instructions. *)

module Rename = MakeIdEnv (Id)

type t = Rename.t

let empty : t = Rename.empty
let dom (rename : t) : IdSet.t = Rename.dom rename

let singleton (id : Id.t) (id_renamed : Id.t) : t =
  Rename.singleton id id_renamed

let add (id : Id.t) (id_renamed : Id.t) (rename : t) : t =
  Rename.add id id_renamed rename

let filter (p : Id.t -> 'a -> bool) (rename : t) : t = Rename.filter p rename

(* Renaming *)

let rename_iterexp (rename : t) (iterexp : iterexp) : iterexp =
  let iter, vars = iterexp in
  let vars =
    List.map
      (fun (id, typ, iters) ->
        match Rename.find_opt id rename with
        | Some id_renamed -> (id_renamed, typ, iters)
        | None -> (id, typ, iters))
      vars
  in
  (iter, vars)

let rename_iterexps (rename : t) (iterexps : iterexp list) : iterexp list =
  List.map (rename_iterexp rename) iterexps

let rec rename_exp (rename : t) (exp : exp) : exp =
  let at, note = (exp.at, exp.note) in
  match exp.it with
  | BoolE _ | NumE _ | TextE _ -> exp
  | VarE id when Rename.mem id rename ->
      let id_renamed = Rename.find id rename in
      Il.VarE id_renamed $$ (at, note)
  | VarE _ -> exp
  | UnE (unop, optyp, exp) ->
      let exp = rename_exp rename exp in
      Il.UnE (unop, optyp, exp) $$ (at, note)
  | BinE (binop, optyp, exp_l, exp_r) ->
      let exp_l = rename_exp rename exp_l in
      let exp_r = rename_exp rename exp_r in
      Il.BinE (binop, optyp, exp_l, exp_r) $$ (at, note)
  | CmpE (cmpop, optyp, exp_l, exp_r) ->
      let exp_l = rename_exp rename exp_l in
      let exp_r = rename_exp rename exp_r in
      Il.CmpE (cmpop, optyp, exp_l, exp_r) $$ (at, note)
  | UpCastE (typ, exp) ->
      let exp = rename_exp rename exp in
      Il.UpCastE (typ, exp) $$ (at, note)
  | DownCastE (typ, exp) ->
      let exp = rename_exp rename exp in
      Il.DownCastE (typ, exp) $$ (at, note)
  | SubE (exp, typ) ->
      let exp = rename_exp rename exp in
      Il.SubE (exp, typ) $$ (at, note)
  | MatchE (exp, pattern) ->
      let exp = rename_exp rename exp in
      Il.MatchE (exp, pattern) $$ (at, note)
  | TupleE exps ->
      let exps = rename_exps rename exps in
      Il.TupleE exps $$ (at, note)
  | CaseE (mixop, exps) ->
      let exps = rename_exps rename exps in
      Il.CaseE (mixop, exps) $$ (at, note)
  | StrE expfields ->
      let atoms, exps = List.split expfields in
      let exps = List.map (rename_exp rename) exps in
      let expfields = List.combine atoms exps in
      Il.StrE expfields $$ (at, note)
  | OptE exp_opt ->
      let exp_opt = Option.map (rename_exp rename) exp_opt in
      Il.OptE exp_opt $$ (at, note)
  | ListE exps ->
      let exps = List.map (rename_exp rename) exps in
      Il.ListE exps $$ (at, note)
  | ConsE (exp_h, exp_t) ->
      let exp_h = rename_exp rename exp_h in
      let exp_t = rename_exp rename exp_t in
      Il.ConsE (exp_h, exp_t) $$ (at, note)
  | CatE (exp_l, exp_r) ->
      let exp_l = rename_exp rename exp_l in
      let exp_r = rename_exp rename exp_r in
      Il.CatE (exp_l, exp_r) $$ (at, note)
  | MemE (exp_e, exp_s) ->
      let exp_e = rename_exp rename exp_e in
      let exp_s = rename_exp rename exp_s in
      Il.MemE (exp_e, exp_s) $$ (at, note)
  | LenE exp ->
      let exp = rename_exp rename exp in
      Il.LenE exp $$ (at, note)
  | DotE (exp, atom) ->
      let exp = rename_exp rename exp in
      Il.DotE (exp, atom) $$ (at, note)
  | IdxE (exp_b, exp_i) ->
      let exp_b = rename_exp rename exp_b in
      let exp_i = rename_exp rename exp_i in
      Il.IdxE (exp_b, exp_i) $$ (at, note)
  | SliceE (exp_b, exp_i, exp_n) ->
      let exp_b = rename_exp rename exp_b in
      let exp_i = rename_exp rename exp_i in
      let exp_n = rename_exp rename exp_n in
      Il.SliceE (exp_b, exp_i, exp_n) $$ (at, note)
  | UpdE (exp_b, path, exp_f) ->
      let exp_b = rename_exp rename exp_b in
      let path = rename_path rename path in
      let exp_f = rename_exp rename exp_f in
      Il.UpdE (exp_b, path, exp_f) $$ (at, note)
  | CallE (id, targs, args) ->
      let args = rename_args rename args in
      Il.CallE (id, targs, args) $$ (at, note)
  | IterE (exp, iterexp) ->
      let exp = rename_exp rename exp in
      let iterexp = rename_iterexp rename iterexp in
      Il.IterE (exp, iterexp) $$ (at, note)

and rename_exps (rename : t) (exps : exp list) : exp list =
  List.map (rename_exp rename) exps

and rename_path (rename : t) (path : path) : path =
  let at, note = (path.at, path.note) in
  match path.it with
  | RootP -> path
  | IdxP (path, exp) ->
      let path = rename_path rename path in
      let exp = rename_exp rename exp in
      Il.IdxP (path, exp) $$ (at, note)
  | SliceP (path, exp_i, exp_n) ->
      let path = rename_path rename path in
      let exp_i = rename_exp rename exp_i in
      let exp_n = rename_exp rename exp_n in
      Il.SliceP (path, exp_i, exp_n) $$ (at, note)
  | DotP (path, atom) ->
      let path = rename_path rename path in
      Il.DotP (path, atom) $$ (at, note)

and rename_arg (rename : t) (arg : arg) : arg =
  let at = arg.at in
  match arg.it with
  | ExpA exp ->
      let exp = rename_exp rename exp in
      Il.ExpA exp $ at
  | DefA _ -> arg

and rename_args (rename : t) (args : arg list) : arg list =
  List.map (rename_arg rename) args

and rename_case (rename : t) (case : case) : case =
  let guard, instrs = case in
  let guard = rename_guard rename guard in
  let instrs = rename_instrs rename instrs in
  (guard, instrs)

and rename_cases (rename : t) (cases : case list) : case list =
  List.map (rename_case rename) cases

and rename_guard (rename : t) (guard : guard) : guard =
  match guard with
  | BoolG _ -> guard
  | CmpG (cmpop, optyp, exp) ->
      let exp = rename_exp rename exp in
      CmpG (cmpop, optyp, exp)
  | SubG _ | MatchG _ -> guard
  | MemG exp ->
      let exp = rename_exp rename exp in
      MemG exp

and rename_instr (rename : t) (instr : instr) : instr =
  let at = instr.at in
  match instr.it with
  | IfI (exp_cond, iterexps, instrs_then) ->
      let exp_cond = rename_exp rename exp_cond in
      let iterexps = rename_iterexps rename iterexps in
      let instrs_then = rename_instrs rename instrs_then in
      IfI (exp_cond, iterexps, instrs_then) $ at
  | HoldI (id, (mixop, exps), iterexps, instrs_hold, instrs_nothold) ->
      let exps = rename_exps rename exps in
      let iterexps = rename_iterexps rename iterexps in
      let instrs_hold = rename_instrs rename instrs_hold in
      let instrs_nothold = rename_instrs rename instrs_nothold in
      HoldI (id, (mixop, exps), iterexps, instrs_hold, instrs_nothold) $ at
  | CaseI (exp, cases, total) ->
      let exp = rename_exp rename exp in
      let cases = rename_cases rename cases in
      CaseI (exp, cases, total) $ at
  | OtherwiseI instr ->
      let instr = rename_instr rename instr in
      OtherwiseI instr $ at
  | GroupI (id_group, rel_signature, exps_group, instrs_group) ->
      let instrs_group = rename_instrs rename instrs_group in
      let exps_group = rename_exps rename exps_group in
      GroupI (id_group, rel_signature, exps_group, instrs_group) $ at
  | LetI (exp_l, exp_r, iterexps) ->
      let exp_l = rename_exp rename exp_l in
      let exp_r = rename_exp rename exp_r in
      let iterexps = rename_iterexps rename iterexps in
      LetI (exp_l, exp_r, iterexps) $ at
  | RuleI (id_rel, (mixop, exps), iterexps) ->
      let exps = rename_exps rename exps in
      let iterexps = rename_iterexps rename iterexps in
      RuleI (id_rel, (mixop, exps), iterexps) $ at
  | ResultI (rel_signature, exps) ->
      let exps = rename_exps rename exps in
      ResultI (rel_signature, exps) $ at
  | ReturnI exp ->
      let exp = rename_exp rename exp in
      ReturnI exp $ at
  | DebugI exp ->
      let exp = rename_exp rename exp in
      DebugI exp $ at

and rename_instrs (rename : t) (instrs : instr list) : instr list =
  List.map (rename_instr rename) instrs
