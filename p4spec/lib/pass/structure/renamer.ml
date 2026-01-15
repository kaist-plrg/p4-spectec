open Domain.Lib
open Lang
open Ol.Ast
open Runtime.Dynamic_Sl
open Envs
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

and rename_arg (renamer : t) (arg : arg) : arg =
  let at = arg.at in
  match arg.it with
  | ExpA exp ->
      let exp = rename_exp renamer exp in
      Il.ExpA exp $ at
  | DefA _ -> arg

and rename_args (renamer : t) (args : arg list) : arg list =
  List.map (rename_arg renamer) args

and rename_case (ihenv : IHEnv.t) (renamer : t) (case : case) : case =
  let guard, instrs = case in
  let guard = rename_guard renamer guard in
  let instrs = rename_instrs ihenv renamer instrs in
  (guard, instrs)

and rename_cases (ihenv : IHEnv.t) (renamer : t) (cases : case list) : case list
    =
  List.map (rename_case ihenv renamer) cases

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

and rename_instr (ihenv : IHEnv.t) (renamer : t) (instr : instr) : t * instr =
  let at = instr.at in
  match instr.it with
  | IfI (exp_cond, iterexps, instrs_then) ->
      let exp_cond = rename_exp renamer exp_cond in
      let iterexps = rename_iterexps renamer iterexps in
      let instrs_then = rename_instrs ihenv renamer instrs_then in
      let instr = IfI (exp_cond, iterexps, instrs_then) $ at in
      (renamer, instr)
  | HoldI (id, (mixop, exps), iterexps, instrs_hold, instrs_nothold) ->
      let exps = rename_exps renamer exps in
      let iterexps = rename_iterexps renamer iterexps in
      let instrs_hold = rename_instrs ihenv renamer instrs_hold in
      let instrs_nothold = rename_instrs ihenv renamer instrs_nothold in
      let instr =
        HoldI (id, (mixop, exps), iterexps, instrs_hold, instrs_nothold) $ at
      in
      (renamer, instr)
  | CaseI (exp, cases, total) ->
      let exp = rename_exp renamer exp in
      let cases = rename_cases ihenv renamer cases in
      let instr = CaseI (exp, cases, total) $ at in
      (renamer, instr)
  | OtherwiseI instr ->
      let _, instr = rename_instr ihenv renamer instr in
      let instr = OtherwiseI instr $ at in
      (renamer, instr)
  | GroupI (id_group, rel_signature, exps_group, instrs_group) ->
      let exps_group = rename_exps renamer exps_group in
      let instrs_group = rename_instrs ihenv renamer instrs_group in
      let instr =
        GroupI (id_group, rel_signature, exps_group, instrs_group) $ at
      in
      (renamer, instr)
  | LetI (exp_l, exp_r, iterinstrs) ->
      let exp_r = rename_exp renamer exp_r in
      let frees_l = Ol.Free.free_exp exp_l in
      let renamer = filter (fun id _ -> not (IdSet.mem id frees_l)) renamer in
      let iterinstrs = rename_iterinstrs_bound renamer iterinstrs in
      let instr = LetI (exp_l, exp_r, iterinstrs) $ at in
      (renamer, instr)
  | RuleI (id_rel, (mixop, exps), iterinstrs) ->
      let exps_input_indexed, exps_output_indexed =
        let inputs = IHEnv.find id_rel ihenv in
        Hints.Input.split inputs exps
      in
      let exps_input_indexed =
        let idxs_input, exps_input = List.split exps_input_indexed in
        let exps_input = rename_exps renamer exps_input in
        List.combine idxs_input exps_input
      in
      let frees_output =
        let exps_output = List.map snd exps_output_indexed in
        Ol.Free.free_exps exps_output
      in
      let renamer =
        filter (fun id _ -> not (IdSet.mem id frees_output)) renamer
      in
      let exps = Hints.Input.combine exps_input_indexed exps_output_indexed in
      let iterinstrs = rename_iterinstrs_bound renamer iterinstrs in
      let instr = RuleI (id_rel, (mixop, exps), iterinstrs) $ at in
      (renamer, instr)
  | ResultI (rel_signature, exps) ->
      let exps = rename_exps renamer exps in
      let instr = ResultI (rel_signature, exps) $ at in
      (renamer, instr)
  | ReturnI exp ->
      let exp = rename_exp renamer exp in
      let instr = ReturnI exp $ at in
      (renamer, instr)
  | DebugI exp ->
      let exp = rename_exp renamer exp in
      let instr = DebugI exp $ at in
      (renamer, instr)

and rename_instrs (ihenv : IHEnv.t) (renamer : t) (instrs : instr list) :
    instr list =
  List.fold_left
    (fun (renamer, instrs) instr ->
      let renamer, instr = rename_instr ihenv renamer instr in
      (renamer, instrs @ [ instr ]))
    (renamer, []) instrs
  |> snd

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
