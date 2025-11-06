open Ast
open Util.Source
open Domain.Lib
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

let replace_call_exp (ids_used : IdSet.t) exp =
  let vars_extracted = ref Free.VarSet.empty in
  let var_new = ref ("" $ no_region, Il.Ast.FuncT $ no_region, []) in
  let transformer e (acc : iterexp list) =
    match e.it with
    | CallE (_funcprose, _targs, args) ->
        vars_extracted := Free.Vars.free_args args;
        let exp_new, id_added =
          Transform.fresh_exp_from_typ ids_used (e.note $ e.at)
        in
        let var = (id_added, e.note $ e.at, []) in
        var_new := var;
        Some (exp_new, acc)
    | _ -> None
  in
  (* No top-down information flow *)
  let accumulator acc _e = acc in
  (* rewrite CallE to VarE, and collect enclosing iterexps *)
  match
    Transform.transform_first_with_acc transformer accumulator Fold.identity []
      exp
  with
  | Some (exp, iterexps) ->
      (* compute dimension of var_new *)
      let _iters = iterexps |> List.rev |> List.map fst in
      (* construct leading instruction enclosed in necessary iterexps *)
      Some exp
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
