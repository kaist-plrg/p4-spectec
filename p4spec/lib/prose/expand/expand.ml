open Util.Source
open Sl.Ast
open Transform
open Domain.Lib

(** Replaces first CallE (pre-order) in exp,

    Returns:

    1) the new instruction to be prepended

    2) the rewritten expression *)

type call_e_count = Yes | No | SkipOne

(* Skips the first CallE *)

let count_call_e (seen_calls : call_e_count) e =
  match e.it with
  | Il.Ast.CallE _ -> ( match seen_calls with No -> SkipOne | _ -> Yes)
  | Il.Ast.IterE _ -> seen_calls
  | _ -> Yes

(* Transformer takes a CallE and rewrites it to exp_new, while initializing accumulated data *)

let rewriter_call_e ids_used (call_e_count : call_e_count) (exp : exp) :
    (exp * iter_state) option =
  match call_e_count with
  | Yes -> (
      match exp.it with
      | CallE (_, _, []) -> None
      | CallE (_funcprose, _targs, args) ->
          let exp_new, var_new =
            Transform.fresh_exp_from_typ ids_used (exp.note $ exp.at)
          in
          let iter_state : iter_state =
            {
              vars_inner = Free.Vars.free_args args;
              vars_outer = Free.VarSet.empty;
              var_new;
              iterexps = [];
              exp_orig = exp;
              exp_new;
            }
          in
          Some (exp_new, iter_state)
      | _ -> None)
  | _ -> None

let transformer_call_e ids_used =
  Transform.transform_first_with_iters (rewriter_call_e ids_used) count_call_e

let replace_call_exp (ids_used : IdSet.t) exp : (instr * exp) option =
  (* Builds the new assignment instruction with the returned state *)
  match transformer_call_e ids_used No exp with
  | Some (exp_new_full, iter_state) ->
      let { var_new; iterexps; exp_orig; exp_new; _ } = iter_state in
      let id, typ, iters = var_new in
      (* drops the original iterators in exp_new *)
      let iters_enclosing =
        List.drop (List.length iters - List.length iterexps) iters
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
      let instr_let = LetI (exp_new, exp_orig, iterexps_instr) $ no_region in
      Some (instr_let, exp_new_full)
  | None -> None

let expand_nested_calls ids_used instrs =
  match instrs with
  | { it = LetI (exp_l, exp_r, iterexps); at; _ } :: instrs_rest ->
      let* instr_new, exp_r' = replace_call_exp ids_used exp_r in
      Some ([ instr_new; LetI (exp_l, exp_r', iterexps) $ at ], instrs_rest)
  | _ -> None

type 'ctx expansion = 'ctx -> instr list -> (instr list * instr list) option

let rec expand_with_context (ctx : 'ctx) (expansion : 'ctx expansion)
    (instrs : instr list) : instr list =
  match instrs with
  | [] -> []
  | instr_h :: instrs_t -> (
      match expansion ctx instrs with
      | Some (expanded_instrs, instrs_rest) ->
          expanded_instrs @ expand_with_context ctx expansion instrs_rest
      | None -> instr_h :: expand_with_context ctx expansion instrs_t)
