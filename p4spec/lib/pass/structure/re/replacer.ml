open Domain.Lib
open Lang
open Ol.Ast
open Util.Source

(* Helper for replacing identifiers in expressions and instructions. *)

module Exp = struct
  type t = exp

  let to_string = Ol.Print.string_of_exp
end

module Replace = MakeIdEnv (Exp)

type t = Replace.t

let empty : t = Replace.empty
let dom (replacer : t) : IdSet.t = Replace.dom replacer
let singleton (id : Id.t) (exp : Exp.t) : t = Replace.singleton id exp

let add (id : Id.t) (exp : Exp.t) (replacer : t) : t =
  Replace.add id exp replacer

let filter (p : Id.t -> 'a -> bool) (replacer : t) : t =
  Replace.filter p replacer

(* Replacement *)

(* Expressions *)

let rec replace_exp (replacer : t) (exp : exp) : exp =
  let at, note = (exp.at, exp.note) in
  match exp.it with
  | BoolE _ | NumE _ | TextE _ -> exp
  | VarE id when Replace.mem id replacer -> Replace.find id replacer
  | VarE _ -> exp
  | UnE (unop, optyp, exp) ->
      let exp = replace_exp replacer exp in
      Il.UnE (unop, optyp, exp) $$ (at, note)
  | BinE (binop, optyp, exp_l, exp_r) ->
      let exp_l = replace_exp replacer exp_l in
      let exp_r = replace_exp replacer exp_r in
      Il.BinE (binop, optyp, exp_l, exp_r) $$ (at, note)
  | CmpE (cmpop, optyp, exp_l, exp_r) ->
      let exp_l = replace_exp replacer exp_l in
      let exp_r = replace_exp replacer exp_r in
      Il.CmpE (cmpop, optyp, exp_l, exp_r) $$ (at, note)
  | UpCastE (typ, exp) ->
      let exp = replace_exp replacer exp in
      Il.UpCastE (typ, exp) $$ (at, note)
  | DownCastE (typ, exp) ->
      let exp = replace_exp replacer exp in
      Il.DownCastE (typ, exp) $$ (at, note)
  | SubE (exp, typ) ->
      let exp = replace_exp replacer exp in
      Il.SubE (exp, typ) $$ (at, note)
  | MatchE (exp, pattern) ->
      let exp = replace_exp replacer exp in
      Il.MatchE (exp, pattern) $$ (at, note)
  | TupleE exps ->
      let exps = replace_exps replacer exps in
      Il.TupleE exps $$ (at, note)
  | CaseE (mixop, exps) ->
      let exps = replace_exps replacer exps in
      Il.CaseE (mixop, exps) $$ (at, note)
  | StrE expfields ->
      let atoms, exps = List.split expfields in
      let exps = replace_exps replacer exps in
      let expfields = List.combine atoms exps in
      Il.StrE expfields $$ (at, note)
  | OptE exp_opt ->
      let exp_opt = Option.map (replace_exp replacer) exp_opt in
      Il.OptE exp_opt $$ (at, note)
  | ListE exps ->
      let exps = replace_exps replacer exps in
      Il.ListE exps $$ (at, note)
  | ConsE (exp_h, exp_t) ->
      let exp_h = replace_exp replacer exp_h in
      let exp_t = replace_exp replacer exp_t in
      Il.ConsE (exp_h, exp_t) $$ (at, note)
  | CatE (exp_l, exp_r) ->
      let exp_l = replace_exp replacer exp_l in
      let exp_r = replace_exp replacer exp_r in
      Il.CatE (exp_l, exp_r) $$ (at, note)
  | MemE (exp_e, exp_s) ->
      let exp_e = replace_exp replacer exp_e in
      let exp_s = replace_exp replacer exp_s in
      Il.MemE (exp_e, exp_s) $$ (at, note)
  | LenE exp ->
      let exp = replace_exp replacer exp in
      Il.LenE exp $$ (at, note)
  | DotE (exp, atom) ->
      let exp = replace_exp replacer exp in
      Il.DotE (exp, atom) $$ (at, note)
  | IdxE (exp_b, exp_i) ->
      let exp_b = replace_exp replacer exp_b in
      let exp_i = replace_exp replacer exp_i in
      Il.IdxE (exp_b, exp_i) $$ (at, note)
  | SliceE (exp_b, exp_l, exp_h) ->
      let exp_b = replace_exp replacer exp_b in
      let exp_l = replace_exp replacer exp_l in
      let exp_h = replace_exp replacer exp_h in
      Il.SliceE (exp_b, exp_l, exp_h) $$ (at, note)
  | UpdE (exp_b, path, exp_f) ->
      let exp_b = replace_exp replacer exp_b in
      let path = replace_path replacer path in
      let exp_f = replace_exp replacer exp_f in
      Il.UpdE (exp_b, path, exp_f) $$ (at, note)
  | CallE (id, targs, args) ->
      let args = replace_args replacer args in
      Il.CallE (id, targs, args) $$ (at, note)
  | IterE (exp, iterexp) ->
      let exp = replace_exp replacer exp in
      let iterexp = replace_iterexp replacer iterexp in
      Il.IterE (exp, iterexp) $$ (at, note)

and replace_exps (replacer : t) (exps : exp list) : exp list =
  List.map (replace_exp replacer) exps

and replace_iterexp (replacer : t) (iterexp : iterexp) : iterexp =
  let iter, vars = iterexp in
  let vars =
    List.filter (fun (id, _, _) -> not (Replace.mem id replacer)) vars
  in
  (iter, vars)

and replace_iterexps (replacer : t) (iterexps : iterexp list) : iterexp list =
  List.map (replace_iterexp replacer) iterexps

(* Paths *)

and replace_path (replacer : t) (path : path) : path =
  let at, note = (path.at, path.note) in
  match path.it with
  | RootP -> path
  | IdxP (path, exp) ->
      let path = replace_path replacer path in
      let exp = replace_exp replacer exp in
      Il.IdxP (path, exp) $$ (at, note)
  | SliceP (path, exp_i, exp_n) ->
      let path = replace_path replacer path in
      let exp_i = replace_exp replacer exp_i in
      let exp_n = replace_exp replacer exp_n in
      Il.SliceP (path, exp_i, exp_n) $$ (at, note)
  | DotP (path, atom) ->
      let path = replace_path replacer path in
      Il.DotP (path, atom) $$ (at, note)

(* Arguments *)

and replace_arg (replacer : t) (arg : arg) : arg =
  let at = arg.at in
  match arg.it with
  | ExpA exp ->
      let exp = replace_exp replacer exp in
      Il.ExpA exp $ at
  | DefA _ -> arg

and replace_args (replacer : t) (args : arg list) : arg list =
  List.map (replace_arg replacer) args

(* Cases *)

and replace_case (replacer : t) (case : case) : case =
  let guard, block = case in
  let guard = replace_guard replacer guard in
  let block = replace_block replacer block in
  (guard, block)

and replace_cases (replacer : t) (cases : case list) : case list =
  List.map (replace_case replacer) cases

and replace_guard (replacer : t) (guard : guard) : guard =
  match guard with
  | BoolG _ -> guard
  | CmpG (cmpop, optyp, exp) ->
      let exp = replace_exp replacer exp in
      CmpG (cmpop, optyp, exp)
  | SubG _ | MatchG _ -> guard
  | MemG exp ->
      let exp = replace_exp replacer exp in
      MemG exp

(* Instructions *)

and replace_instr (replacer : t) (instr : instr) : instr =
  let at = instr.at in
  match instr.it with
  | IfI (exp_cond, iterexps, block_then) ->
      let exp_cond = replace_exp replacer exp_cond in
      let iterexps = replace_iterexps replacer iterexps in
      let block_then = replace_block replacer block_then in
      IfI (exp_cond, iterexps, block_then) $ at
  | HoldI (id, (mixop, exps), iterexps, block_hold, block_nothold) ->
      let exps = replace_exps replacer exps in
      let iterexps = replace_iterexps replacer iterexps in
      let block_hold = replace_block replacer block_hold in
      let block_nothold = replace_block replacer block_nothold in
      HoldI (id, (mixop, exps), iterexps, block_hold, block_nothold) $ at
  | CaseI (exp, cases, total) ->
      let exp = replace_exp replacer exp in
      let cases = replace_cases replacer cases in
      CaseI (exp, cases, total) $ at
  | GroupI (id_group, rel_signature, exps_group, block) ->
      let exps_group = replace_exps replacer exps_group in
      let block = replace_block replacer block in
      GroupI (id_group, rel_signature, exps_group, block) $ at
  | LetI (exp_l, exp_r, iterinstrs, block) ->
      let frees_l = Ol.Free.free_exp exp_l in
      let replacer = filter (fun id _ -> not (IdSet.mem id frees_l)) replacer in
      let exp_r = replace_exp replacer exp_r in
      let iterinstrs = replace_iterinstrs_bound replacer iterinstrs in
      let block = replace_block replacer block in
      LetI (exp_l, exp_r, iterinstrs, block) $ at
  | RuleI (id_rel, (mixop, exps), inputs, iterinstrs, block) ->
      let exps_input, exps_output = Hints.Input.split inputs exps in
      let exps_input = replace_exps replacer exps_input in
      let frees_output = Ol.Free.free_exps exps_output in
      let replacer =
        filter (fun id _ -> not (IdSet.mem id frees_output)) replacer
      in
      let exps = Hints.Input.combine inputs exps_input exps_output in
      let iterinstrs = replace_iterinstrs_bound replacer iterinstrs in
      let block = replace_block replacer block in
      RuleI (id_rel, (mixop, exps), inputs, iterinstrs, block) $ at
  | ResultI (rel_signature, exps) ->
      let exps = replace_exps replacer exps in
      ResultI (rel_signature, exps) $ at
  | ReturnI exp ->
      let exp = replace_exp replacer exp in
      ReturnI exp $ at
  | DebugI exp ->
      let exp = replace_exp replacer exp in
      DebugI exp $ at

and replace_instrs (replacer : t) (instrs : instr list) : instr list =
  List.map (replace_instr replacer) instrs

and replace_block (replacer : t) (block : block) : block =
  replace_instrs replacer block

and replace_iterinstr_bound (replacer : t) (iterinstr : iterinstr) : iterinstr =
  let iter, vars_bind, vars_bound = iterinstr in
  let vars_bound =
    List.filter (fun (id, _, _) -> not (Replace.mem id replacer)) vars_bound
  in
  (iter, vars_bind, vars_bound)

and replace_iterinstrs_bound (replacer : t) (iterinstrs : iterinstr list) :
    iterinstr list =
  List.map (replace_iterinstr_bound replacer) iterinstrs
