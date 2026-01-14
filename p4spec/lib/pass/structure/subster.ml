open Domain.Lib
open Lang
open Ol.Ast
open Util.Source

(* Helper for substituting identifiers in expressions and instructions. *)

module Exp = struct
  type t = exp

  let to_string = Ol.Print.string_of_exp
end

module Subst = MakeIdEnv (Exp)

type t = Subst.t

let empty : t = Subst.empty
let dom (rename : t) : IdSet.t = Subst.dom rename
let singleton (id : Id.t) (exp : Exp.t) : t = Subst.singleton id exp
let add (id : Id.t) (exp : Exp.t) (rename : t) : t = Subst.add id exp rename

(* Substitution *)

let subst_iterexp (subster : t) (iterexp : iterexp) : iterexp =
  let iter, vars = iterexp in
  let vars = List.filter (fun (id, _, _) -> not (Subst.mem id subster)) vars in
  (iter, vars)

let subst_iterexps (subster : t) (iterexps : iterexp list) : iterexp list =
  List.map (subst_iterexp subster) iterexps

let rec subst_exp (subster : t) (exp : exp) : exp =
  let at, note = (exp.at, exp.note) in
  match exp.it with
  | BoolE _ | NumE _ | TextE _ -> exp
  | VarE id when Subst.mem id subster -> Subst.find id subster
  | VarE _ -> exp
  | UnE (unop, optyp, exp) ->
      let exp = subst_exp subster exp in
      Il.UnE (unop, optyp, exp) $$ (at, note)
  | BinE (binop, optyp, exp_l, exp_r) ->
      let exp_l = subst_exp subster exp_l in
      let exp_r = subst_exp subster exp_r in
      Il.BinE (binop, optyp, exp_l, exp_r) $$ (at, note)
  | CmpE (cmpop, optyp, exp_l, exp_r) ->
      let exp_l = subst_exp subster exp_l in
      let exp_r = subst_exp subster exp_r in
      Il.CmpE (cmpop, optyp, exp_l, exp_r) $$ (at, note)
  | UpCastE (typ, exp) ->
      let exp = subst_exp subster exp in
      Il.UpCastE (typ, exp) $$ (at, note)
  | DownCastE (typ, exp) ->
      let exp = subst_exp subster exp in
      Il.DownCastE (typ, exp) $$ (at, note)
  | SubE (exp, typ) ->
      let exp = subst_exp subster exp in
      Il.SubE (exp, typ) $$ (at, note)
  | MatchE (exp, pattern) ->
      let exp = subst_exp subster exp in
      Il.MatchE (exp, pattern) $$ (at, note)
  | TupleE exps ->
      let exps = subst_exps subster exps in
      Il.TupleE exps $$ (at, note)
  | CaseE (mixop, exps) ->
      let exps = subst_exps subster exps in
      Il.CaseE (mixop, exps) $$ (at, note)
  | StrE expfields ->
      let atoms, exps = List.split expfields in
      let exps = subst_exps subster exps in
      let expfields = List.combine atoms exps in
      Il.StrE expfields $$ (at, note)
  | OptE exp_opt ->
      let exp_opt = Option.map (subst_exp subster) exp_opt in
      Il.OptE exp_opt $$ (at, note)
  | ListE exps ->
      let exps = subst_exps subster exps in
      Il.ListE exps $$ (at, note)
  | ConsE (exp_h, exp_t) ->
      let exp_h = subst_exp subster exp_h in
      let exp_t = subst_exp subster exp_t in
      Il.ConsE (exp_h, exp_t) $$ (at, note)
  | CatE (exp_l, exp_r) ->
      let exp_l = subst_exp subster exp_l in
      let exp_r = subst_exp subster exp_r in
      Il.CatE (exp_l, exp_r) $$ (at, note)
  | MemE (exp_e, exp_s) ->
      let exp_e = subst_exp subster exp_e in
      let exp_s = subst_exp subster exp_s in
      Il.MemE (exp_e, exp_s) $$ (at, note)
  | LenE exp ->
      let exp = subst_exp subster exp in
      Il.LenE exp $$ (at, note)
  | DotE (exp, atom) ->
      let exp = subst_exp subster exp in
      Il.DotE (exp, atom) $$ (at, note)
  | IdxE (exp_b, exp_i) ->
      let exp_b = subst_exp subster exp_b in
      let exp_i = subst_exp subster exp_i in
      Il.IdxE (exp_b, exp_i) $$ (at, note)
  | SliceE (exp_b, exp_l, exp_h) ->
      let exp_b = subst_exp subster exp_b in
      let exp_l = subst_exp subster exp_l in
      let exp_h = subst_exp subster exp_h in
      Il.SliceE (exp_b, exp_l, exp_h) $$ (at, note)
  | UpdE (exp_b, path, exp_f) ->
      let exp_b = subst_exp subster exp_b in
      let path = subst_path subster path in
      let exp_f = subst_exp subster exp_f in
      Il.UpdE (exp_b, path, exp_f) $$ (at, note)
  | CallE (id, targs, args) ->
      let args = subst_args subster args in
      Il.CallE (id, targs, args) $$ (at, note)
  | IterE (exp, iterexp) ->
      let exp = subst_exp subster exp in
      let iterexp = subst_iterexp subster iterexp in
      Il.IterE (exp, iterexp) $$ (at, note)

and subst_exps (subster : t) (exps : exp list) : exp list =
  List.map (subst_exp subster) exps

and subst_path (subster : t) (path : path) : path =
  let at, note = (path.at, path.note) in
  match path.it with
  | RootP -> path
  | IdxP (path, exp) ->
      let path = subst_path subster path in
      let exp = subst_exp subster exp in
      Il.IdxP (path, exp) $$ (at, note)
  | SliceP (path, exp_i, exp_n) ->
      let path = subst_path subster path in
      let exp_i = subst_exp subster exp_i in
      let exp_n = subst_exp subster exp_n in
      Il.SliceP (path, exp_i, exp_n) $$ (at, note)
  | DotP (path, atom) ->
      let path = subst_path subster path in
      Il.DotP (path, atom) $$ (at, note)

and subst_arg (subster : t) (arg : arg) : arg =
  let at = arg.at in
  match arg.it with
  | ExpA exp ->
      let exp = subst_exp subster exp in
      Il.ExpA exp $ at
  | DefA _ -> arg

and subst_args (subster : t) (args : arg list) : arg list =
  List.map (subst_arg subster) args

and subst_case (subster : t) (case : case) : case =
  let guard, instrs = case in
  let guard = subst_guard subster guard in
  let instrs = subst_instrs subster instrs in
  (guard, instrs)

and subst_cases (subster : t) (cases : case list) : case list =
  List.map (subst_case subster) cases

and subst_guard (subster : t) (guard : guard) : guard =
  match guard with
  | BoolG _ -> guard
  | CmpG (cmpop, optyp, exp) ->
      let exp = subst_exp subster exp in
      CmpG (cmpop, optyp, exp)
  | SubG _ | MatchG _ -> guard
  | MemG exp ->
      let exp = subst_exp subster exp in
      MemG exp

and subst_instr (susbter : t) (instr : instr) : instr =
  let at = instr.at in
  match instr.it with
  | IfI (exp_cond, iterexps, instrs_then) ->
      let exp_cond = subst_exp susbter exp_cond in
      let iterexps = subst_iterexps susbter iterexps in
      let instrs_then = subst_instrs susbter instrs_then in
      IfI (exp_cond, iterexps, instrs_then) $ at
  | HoldI (id, (mixop, exps), iterexps, instrs_hold, instrs_nothold) ->
      let exps = subst_exps susbter exps in
      let iterexps = subst_iterexps susbter iterexps in
      let instrs_hold = subst_instrs susbter instrs_hold in
      let instrs_nothold = subst_instrs susbter instrs_nothold in
      HoldI (id, (mixop, exps), iterexps, instrs_hold, instrs_nothold) $ at
  | CaseI (exp, cases, total) ->
      let exp = subst_exp susbter exp in
      let cases = subst_cases susbter cases in
      CaseI (exp, cases, total) $ at
  | OtherwiseI instr ->
      let instr = subst_instr susbter instr in
      OtherwiseI instr $ at
  | GroupI (id_group, rel_signature, exps_group, instrs_group) ->
      let exps_group = subst_exps susbter exps_group in
      let instrs_group = subst_instrs susbter instrs_group in
      GroupI (id_group, rel_signature, exps_group, instrs_group) $ at
  | LetI (exp_l, exp_r, iterexps) ->
      let exp_l = subst_exp susbter exp_l in
      let exp_r = subst_exp susbter exp_r in
      let iterexps = subst_iterexps susbter iterexps in
      LetI (exp_l, exp_r, iterexps) $ at
  | RuleI (id_rel, (mixop, exps), iterexps) ->
      let exps = subst_exps susbter exps in
      let iterexps = subst_iterexps susbter iterexps in
      RuleI (id_rel, (mixop, exps), iterexps) $ at
  | ResultI (rel_signature, exps) ->
      let exps = subst_exps susbter exps in
      ResultI (rel_signature, exps) $ at
  | ReturnI exp ->
      let exp = subst_exp susbter exp in
      ReturnI exp $ at
  | DebugI exp ->
      let exp = subst_exp susbter exp in
      DebugI exp $ at

and subst_instrs (subster : t) (instrs : instr list) : instr list =
  List.map (subst_instr subster) instrs
