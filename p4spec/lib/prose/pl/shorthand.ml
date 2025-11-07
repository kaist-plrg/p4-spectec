open Ast
open Util.Source
open Domain.Lib
open Transform
module F = Format

type shorthand = instr list -> (instr list * instr list) option

(* Shorthands: applied in order *)

(* Check & Let -> CheckLet *)
let force_let instrs =
  match instrs with
  | { it = CheckI (ExpCond { it = MatchE (exp, _); _ }); _ }
    :: { it = LetI (exp_l, exp_r); _ }
    :: instrs_rest
    when Eq.eq_exp exp exp_r ->
      Some ([ CheckLetI (exp_l, exp) $ exp_r.at ], instrs_rest)
  | { it = CheckI (ExpCond { it = SubE (exp, typ); _ }); _ }
    :: { it = LetI (exp_l, { it = DownCastE (typ_r, exp_r); _ }); _ }
    :: instrs_rest
    when Eq.eq_exp exp exp_r && Eq.eq_typ typ typ_r ->
      Some ([ CheckLetI (exp_l, exp) $ exp_r.at ], instrs_rest)
  | _ -> None

let option_get instrs =
  match instrs with
  | { it = LetI (exp_opt, exp_call); at; _ }
    :: { it = CheckLetI ({ it = OptE (Some exp_l); _ }, exp_r); _ }
    :: instrs_rest
    when Eq.eq_exp exp_opt exp_r ->
      Some ([ OptionGetI (exp_l, exp_call) $ at ], instrs_rest)
  | _ -> None

(** Replaces first CallE (pre-order) in exp,

    returns:

    1) the new instruction to be prepended

    2) the rewritten expression *)

let replace_call_exp (ids_used : IdSet.t) exp : (instr * exp) option =
  let transformer exp =
    match exp.it with
    | CallE (_funcprose, _targs, args) ->
        let exp_new, id_added =
          Transform.fresh_exp_from_typ ids_used (exp.note $ exp.at)
        in
        let var = (id_added, exp.note $ exp.at, []) in
        let iter_state : iter_state =
          {
            vars_inner = Free.Vars.free_args args;
            vars_outer = Free.VarSet.empty;
            var_new = var;
            iterexps = [];
            exp_orig = exp;
          }
        in
        Some (exp_new, iter_state)
    | _ -> None
  in
  (* rewrite CallE to VarE, and collect enclosing iterexps *)
  match Transform.transform_first_with_iters transformer exp with
  | Some (exp, iter_state) ->
      let { var_new; iterexps; exp_orig; _ } = iter_state in
      let id, typ, iters = var_new in
      let exp_var = VarE id $$ (typ.at, typ.it) in
      let instr_let = LetI (exp_var, exp_orig) $ no_region in
      let iter_combined = List.combine iters iterexps in
      let instr_iterated, _ =
        List.fold_left
          (fun (instr, var_new) (iter_new, iterexp) ->
            (* itervars_out: each iteration layer of var_new *)
            let itervars_out = [ var_new ] in
            let var_new =
              let id, typ, iters = var_new in
              (id, typ, iters @ [ iter_new ])
            in
            (* itervars_in: each layer of iterexp *)
            let iter_in, itervars_in = iterexp in
            assert (iter_in = iter_new);
            let instr =
              ForEachI (itervars_out, instr, itervars_in) $ no_region
            in
            (instr, var_new))
          (instr_let, (id, typ, []))
          iter_combined
      in
      Some (instr_iterated, exp)
  | None -> None

let contains_call_exp exp =
  let cond e = match e.it with CallE _ -> true | _ -> false in
  Transform.search_exp cond exp

(* let expand_nested_calls instrs = *)
(*   match instrs with *)
(*   | { it = LetI (exp_l, exp_r); at; _ } :: instrs_rest when contains_call_exp exp_r *)
(*     -> *)

let rec apply_shorthand (shorthand : shorthand) (instrs : instr list) :
    instr list =
  match instrs with
  | [] -> []
  | instr_h :: instrs_t -> (
      match shorthand instrs with
      | Some (shortened_instrs, instrs_rest) ->
          shortened_instrs @ apply_shorthand shorthand instrs_rest
      | None -> instr_h :: apply_shorthand shorthand instrs_t)

let apply_all_shorthands (instrs : instr list) : instr list = instrs
