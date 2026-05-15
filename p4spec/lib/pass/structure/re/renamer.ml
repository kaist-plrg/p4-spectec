open Domain.Lib
open Lang
open Ol.Ast
open Util.Source

(* Helper for renaming identifiers in expressions and instructions. *)

module Rename = MakeIdEnv (Id)

type t = Rename.t

let empty : t = Rename.empty
let dom (renamer : t) : IdSet.t = Rename.dom renamer
let values (renamer : t) : Id.t list = Rename.values renamer

let singleton (id : Id.t) (id_renamed : Id.t) : t =
  Rename.singleton id id_renamed

let add (id : Id.t) (id_renamed : Id.t) (renamer : t) : t =
  Rename.add id id_renamed renamer

let of_list (pairs : (Id.t * Id.t) list) : t = Rename.of_list pairs
let filter (p : Id.t -> 'a -> bool) (renamer : t) : t = Rename.filter p renamer

(* Renaming *)

(* Expressions *)

let rec rename_exp (renamer : t) (exp : exp) : exp =
  let at, note = (exp.at, exp.note) in
  match exp.it with
  | BoolE _ | NumE _ | TextE _ -> exp
  | VarE id when Rename.mem id renamer ->
      let id_renamed = Rename.find id renamer in
      Il.VarE id_renamed $$ (at, note)
  | VarE _ -> exp
  | UnE (unop, optyp, exp) ->
      let exp = rename_exp renamer exp in
      Il.UnE (unop, optyp, exp) $$ (at, note)
  | BinE (binop, optyp, exp_l, exp_r) ->
      let exp_l = rename_exp renamer exp_l in
      let exp_r = rename_exp renamer exp_r in
      Il.BinE (binop, optyp, exp_l, exp_r) $$ (at, note)
  | CmpE (cmpop, optyp, exp_l, exp_r) ->
      let exp_l = rename_exp renamer exp_l in
      let exp_r = rename_exp renamer exp_r in
      Il.CmpE (cmpop, optyp, exp_l, exp_r) $$ (at, note)
  | UpCastE (typ, exp) ->
      let exp = rename_exp renamer exp in
      Il.UpCastE (typ, exp) $$ (at, note)
  | DownCastE (typ, exp) ->
      let exp = rename_exp renamer exp in
      Il.DownCastE (typ, exp) $$ (at, note)
  | SubE (exp, typ) ->
      let exp = rename_exp renamer exp in
      Il.SubE (exp, typ) $$ (at, note)
  | MatchE (exp, pattern) ->
      let exp = rename_exp renamer exp in
      Il.MatchE (exp, pattern) $$ (at, note)
  | TupleE exps ->
      let exps = rename_exps renamer exps in
      Il.TupleE exps $$ (at, note)
  | CaseE (mixop, exps) ->
      let exps = rename_exps renamer exps in
      Il.CaseE (mixop, exps) $$ (at, note)
  | StrE expfields ->
      let atoms, exps = List.split expfields in
      let exps = rename_exps renamer exps in
      let expfields = List.combine atoms exps in
      Il.StrE expfields $$ (at, note)
  | OptE exp_opt ->
      let exp_opt = Option.map (rename_exp renamer) exp_opt in
      Il.OptE exp_opt $$ (at, note)
  | ListE exps ->
      let exps = rename_exps renamer exps in
      Il.ListE exps $$ (at, note)
  | ConsE (exp_h, exp_t) ->
      let exp_h = rename_exp renamer exp_h in
      let exp_t = rename_exp renamer exp_t in
      Il.ConsE (exp_h, exp_t) $$ (at, note)
  | CatE (exp_l, exp_r) ->
      let exp_l = rename_exp renamer exp_l in
      let exp_r = rename_exp renamer exp_r in
      Il.CatE (exp_l, exp_r) $$ (at, note)
  | MemE (exp_e, exp_s) ->
      let exp_e = rename_exp renamer exp_e in
      let exp_s = rename_exp renamer exp_s in
      Il.MemE (exp_e, exp_s) $$ (at, note)
  | LenE exp ->
      let exp = rename_exp renamer exp in
      Il.LenE exp $$ (at, note)
  | DotE (exp, atom) ->
      let exp = rename_exp renamer exp in
      Il.DotE (exp, atom) $$ (at, note)
  | IdxE (exp_b, exp_i) ->
      let exp_b = rename_exp renamer exp_b in
      let exp_i = rename_exp renamer exp_i in
      Il.IdxE (exp_b, exp_i) $$ (at, note)
  | SliceE (exp_b, exp_i, exp_n) ->
      let exp_b = rename_exp renamer exp_b in
      let exp_i = rename_exp renamer exp_i in
      let exp_n = rename_exp renamer exp_n in
      Il.SliceE (exp_b, exp_i, exp_n) $$ (at, note)
  | UpdE (exp_b, path, exp_f) ->
      let exp_b = rename_exp renamer exp_b in
      let path = rename_path renamer path in
      let exp_f = rename_exp renamer exp_f in
      Il.UpdE (exp_b, path, exp_f) $$ (at, note)
  | CallE (id, targs, args) ->
      let args = rename_args renamer args in
      Il.CallE (id, targs, args) $$ (at, note)
  | IterE (exp, iterexp) ->
      let exp = rename_exp renamer exp in
      let iterexp = rename_iterexp renamer iterexp in
      Il.IterE (exp, iterexp) $$ (at, note)

and rename_exps (renamer : t) (exps : exp list) : exp list =
  List.map (rename_exp renamer) exps

and rename_iterexp (renamer : t) (iterexp : iterexp) : iterexp =
  let iter, vars = iterexp in
  let vars =
    List.map
      (fun (id, typ, iters) ->
        match Rename.find_opt id renamer with
        | Some id_renamed -> (id_renamed, typ, iters)
        | None -> (id, typ, iters))
      vars
  in
  (iter, vars)

and rename_iterexps (renamer : t) (iterexps : iterexp list) : iterexp list =
  List.map (rename_iterexp renamer) iterexps

(* Paths *)

and rename_path (renamer : t) (path : path) : path =
  let at, note = (path.at, path.note) in
  match path.it with
  | RootP -> path
  | IdxP (path, exp) ->
      let path = rename_path renamer path in
      let exp = rename_exp renamer exp in
      Il.IdxP (path, exp) $$ (at, note)
  | SliceP (path, exp_i, exp_n) ->
      let path = rename_path renamer path in
      let exp_i = rename_exp renamer exp_i in
      let exp_n = rename_exp renamer exp_n in
      Il.SliceP (path, exp_i, exp_n) $$ (at, note)
  | DotP (path, atom) ->
      let path = rename_path renamer path in
      Il.DotP (path, atom) $$ (at, note)

(* Arguments *)

and rename_arg (renamer : t) (arg : arg) : arg =
  let at = arg.at in
  match arg.it with
  | ExpA exp ->
      let exp = rename_exp renamer exp in
      Il.ExpA exp $ at
  | DefA _ -> arg

and rename_args (renamer : t) (args : arg list) : arg list =
  List.map (rename_arg renamer) args

(* Cases *)

and rename_case (renamer : t) (case : case) : case =
  let guard, block = case in
  let guard = rename_guard renamer guard in
  let block = rename_block renamer block in
  (guard, block)

and rename_cases (renamer : t) (cases : case list) : case list =
  List.map (rename_case renamer) cases

and rename_guard (renamer : t) (guard : guard) : guard =
  match guard with
  | BoolG _ -> guard
  | CmpG (cmpop, optyp, exp) ->
      let exp = rename_exp renamer exp in
      CmpG (cmpop, optyp, exp)
  | SubG _ | MatchG _ -> guard
  | MemG exp ->
      let exp = rename_exp renamer exp in
      MemG exp

(* Instructions *)

and rename_instr (renamer : t) (instr : instr) : instr =
  let at = instr.at in
  match instr.it with
  | IfI (exp_cond, iterexps, block_then) ->
      let exp_cond = rename_exp renamer exp_cond in
      let iterexps = rename_iterexps renamer iterexps in
      let block_then = rename_block renamer block_then in
      IfI (exp_cond, iterexps, block_then) $ at
  | HoldI (id, (mixop, exps), iterexps, block_hold, block_nothold) ->
      let exps = rename_exps renamer exps in
      let iterexps = rename_iterexps renamer iterexps in
      let block_hold = rename_block renamer block_hold in
      let block_nothold = rename_block renamer block_nothold in
      HoldI (id, (mixop, exps), iterexps, block_hold, block_nothold) $ at
  | CaseI (exp, cases, total) ->
      let exp = rename_exp renamer exp in
      let cases = rename_cases renamer cases in
      CaseI (exp, cases, total) $ at
  | GroupI (id_group, rel_signature, exps_group, block) ->
      let exps_group = rename_exps renamer exps_group in
      let block = rename_block renamer block in
      GroupI (id_group, rel_signature, exps_group, block) $ at
  | LetI (exp_l, exp_r, iterinstrs, block) ->
      let exp_r = rename_exp renamer exp_r in
      let frees_l = Ol.Free.free_exp exp_l in
      let renamer = filter (fun id _ -> not (IdSet.mem id frees_l)) renamer in
      let iterinstrs = rename_iterinstrs_bound renamer iterinstrs in
      let block = rename_block renamer block in
      LetI (exp_l, exp_r, iterinstrs, block) $ at
  | RuleI (id_rel, (mixop, exps), inputs, iterinstrs, block) ->
      let exps_input, exps_output = Hints.Input.split inputs exps in
      let exps_input = rename_exps renamer exps_input in
      let frees_output = Ol.Free.free_exps exps_output in
      let renamer =
        filter (fun id _ -> not (IdSet.mem id frees_output)) renamer
      in
      let exps = Hints.Input.combine inputs exps_input exps_output in
      let iterinstrs = rename_iterinstrs_bound renamer iterinstrs in
      let block = rename_block renamer block in
      RuleI (id_rel, (mixop, exps), inputs, iterinstrs, block) $ at
  | ResultI (rel_signature, exps) ->
      let exps = rename_exps renamer exps in
      ResultI (rel_signature, exps) $ at
  | ReturnI exp ->
      let exp = rename_exp renamer exp in
      ReturnI exp $ at
  | DebugI exp ->
      let exp = rename_exp renamer exp in
      DebugI exp $ at

and rename_instrs (renamer : t) (instrs : instr list) : instr list =
  List.map (rename_instr renamer) instrs

and rename_block (renamer : t) (block : block) : block =
  rename_instrs renamer block

and rename_iterinstr_bound (renamer : t) (iterinstr : iterinstr) : iterinstr =
  let iter, vars_bound, vars_bind = iterinstr in
  let vars_bound =
    List.map
      (fun (id, typ, iters) ->
        match Rename.find_opt id renamer with
        | Some id_renamed -> (id_renamed, typ, iters)
        | None -> (id, typ, iters))
      vars_bound
  in
  (iter, vars_bound, vars_bind)

and rename_iterinstrs_bound (renamer : t) (iterinstrs : iterinstr list) :
    iterinstr list =
  List.map (rename_iterinstr_bound renamer) iterinstrs

and rename_iterinstr_bind (renamer : t) (iterinstr : iterinstr) : iterinstr =
  let iter, vars_bound, vars_bind = iterinstr in
  let vars_bind =
    List.map
      (fun (id, typ, iters) ->
        match Rename.find_opt id renamer with
        | Some id_renamed -> (id_renamed, typ, iters)
        | None -> (id, typ, iters))
      vars_bind
  in
  (iter, vars_bound, vars_bind)

and rename_iterinstrs_bind (renamer : t) (iterinstrs : iterinstr list) :
    iterinstr list =
  List.map (rename_iterinstr_bind renamer) iterinstrs
