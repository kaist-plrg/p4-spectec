module Vars = Free.Vars
module VarSet = Free.VarSet
open Domain.Lib
open Lang
open Sl
open Runtime.Prose.Envs
open Transform
open Util.Source

(** Replaces first CallE (pre-order) in exp,

    Returns:

    1) the new instruction to be prepended

    2) the rewritten expression *)

type call_e_count = Yes | No | SkipOne

(* Skips the first CallE *)

let count_call_e (seen_calls : call_e_count) e =
  match e.it with
  | Il.CallE _ -> ( match seen_calls with No -> SkipOne | _ -> Yes)
  | Il.IterE _ -> seen_calls
  | _ -> Yes

(* Transformer takes a CallE and rewrites it to exp_new, while initializing accumulated data *)

let rewriter_call_e ids_used (call_e_count : call_e_count) (exp : exp) :
    (exp * iter_state) option =
  match call_e_count with
  | Yes -> (
      match exp.it with
      | CallE (_, _, []) -> None
      | CallE (_funcprose, _targs, args) ->
          let exp_new, var_new, ids_used =
            Transform.fresh_exp_from_typ ids_used (exp.note $ exp.at)
          in
          let iter_state : iter_state =
            {
              vars_inner = Vars.free_args args;
              vars_outer = VarSet.empty;
              var_new;
              iterexps = [];
              exp_orig = exp;
              exp_new;
              ids_used;
            }
          in
          Some (exp_new, iter_state)
      | _ -> None)
  | _ -> None

let transformer_call_e ids_used =
  Transform.transform_first_with_iters (rewriter_call_e ids_used) count_call_e

(* replacement for List.drop, added in OCaml 5.3 *)
let drop (n : int) (l : 'a list) : 'a list =
  let rec drop' (i : int) (l : 'a list) : 'a list =
    match l with _x :: l when i < n -> drop' (i + 1) l | rest -> rest
  in
  if n < 0 then invalid_arg "List.drop";
  drop' 0 l

let replace_call_exp ~(call_e_count : call_e_count) (ids_used : IdSet.t)
    (exp : exp) : (instr * exp * IdSet.t) option =
  (* Builds the new assignment instruction with the returned state *)
  match transformer_call_e ids_used call_e_count exp with
  | Some (exp_new_full, iter_state) ->
      let { var_new; iterexps; exp_orig; exp_new; ids_used; _ } = iter_state in
      let id, typ, iters = var_new in
      (* drops the original iterators in exp_new *)
      let iters_enclosing =
        drop (List.length iters - List.length iterexps) iters
      in
      let iter_combined = List.combine iters_enclosing iterexps in
      let iterexps_instr, _ =
        List.fold_left
          (fun (iterexps_instr, var_new) (iter_new, iterexp) ->
            let iter_in, itervars_in = iterexp in
            assert (iter_in = iter_new);
            (* itervars_out: each iteration layer of var_new *)
            (* itervars_in: each layer of iterexp *)
            let iterexp_instr = (iter_in, var_new :: itervars_in) in
            (* update iterator state of var_new *)
            let var_new =
              let id, typ, iters = var_new in
              (id, typ, iters @ [ iter_new ])
            in
            (iterexps_instr @ [ iterexp_instr ], var_new))
          ([], (id, typ, []))
          iter_combined
      in
      let instr_let =
        LetI (exp_new, exp_orig, iterexps_instr) $$ (no_region, { iid = -1 })
      in
      Some (instr_let, exp_new_full, ids_used)
  | None -> None

let rec replace_call_exps ~(call_e_count : call_e_count) (ids_used : IdSet.t)
    (exps : exp list) : (instr list * exp list * IdSet.t) option =
  match exps with
  | [] -> None
  | exp_h :: exps_t -> (
      match replace_call_exp ~call_e_count ids_used exp_h with
      | Some (instr_h_new, exp_h, ids) -> (
          match replace_call_exps ~call_e_count ids exps_t with
          | Some (instrs_t_new, exps_t, ids) ->
              Some (instr_h_new :: instrs_t_new, exp_h :: exps_t, ids)
          | None -> Some ([ instr_h_new ], exp_h :: exps_t, ids))
      | None -> (
          match replace_call_exps ~call_e_count ids_used exps_t with
          | Some (instrs_t_new, exps_t, ids) ->
              Some (instrs_t_new, exp_h :: exps_t, ids)
          | None -> None))

let expand_nested_calls (ihenv, ids_used) instrs =
  match instrs with
  | { it = LetI (exp_l, exp_r, iterexps); at; note } :: instrs_rest ->
      let* instr_new, exp_r, ids =
        replace_call_exp ~call_e_count:No ids_used exp_r
      in
      let instr_let = LetI (exp_l, exp_r, iterexps) $$ (at, note) in
      Some ((ihenv, ids), [ instr_new; instr_let ], instrs_rest)
  | { it = HoldI (id, notexp, iterexps, holdcase); at; note } :: instrs_rest ->
      let mixop, exps = notexp in
      let* instrs_new, exps, ids =
        replace_call_exps ~call_e_count:SkipOne ids_used exps
      in
      let notexp = (mixop, exps) in
      let instr_rule = HoldI (id, notexp, iterexps, holdcase) $$ (at, note) in
      Some ((ihenv, ids), instrs_new @ [ instr_rule ], instrs_rest)
  | { it = RuleI (id, notexp, iterexps); at; note } :: instrs_rest ->
      let mixop, exps = notexp in
      let inputs = IHEnv.find_opt id ihenv |> Option.value ~default:[] in
      let exps_input_indexed, exps_output_indexed =
        Hints.Input.split inputs exps
      in
      let idxs_input, exps_input = List.split exps_input_indexed in
      let* instrs_new, exps_input, ids =
        replace_call_exps ~call_e_count:SkipOne ids_used exps_input
      in
      let exps_input_indexed = List.combine idxs_input exps_input in
      let exps = Hints.Input.combine exps_input_indexed exps_output_indexed in
      let notexp = (mixop, exps) in
      let instr_rule = RuleI (id, notexp, iterexps) $$ (at, note) in
      Some ((ihenv, ids), instrs_new @ [ instr_rule ], instrs_rest)
  | _ -> None

type 'ctx expansion =
  'ctx -> instr list -> ('ctx * instr list * instr list) option

let rec expand' (ctx : 'ctx) (expansion : 'ctx expansion) (instrs : instr list)
    : 'ctx * instr list =
  match instrs with
  | [] -> (ctx, [])
  | instr_h :: instrs_t -> (
      match expansion ctx instrs with
      | Some (ctx_upd, expanded_instrs, instrs_rest) ->
          expand' ctx_upd expansion (expanded_instrs @ instrs_rest)
      | None ->
          let ctx, instrs_t_expanded = expand' ctx expansion instrs_t in
          (ctx, instr_h :: instrs_t_expanded))

let expand (ctx : 'ctx) (expansion : 'ctx expansion) (instrs : instr list) :
    instr list =
  expand' ctx expansion instrs |> snd
