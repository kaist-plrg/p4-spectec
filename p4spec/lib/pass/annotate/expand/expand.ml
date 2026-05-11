module Free = Prose.Expand.Free
module Transform = Prose.Expand.Transform
module Vars = Free.Vars
module VarSet = Free.VarSet
open Domain.Lib
open Lang
open Sl
open Transform
open Util.Source
module Mixfix = Domain.Mixfix

(* Per-instruction CallE extraction state *)

type call_e_count = Yes | No | SkipOne

let count_call_e (seen_calls : call_e_count) e =
  match e.it with
  | Il.CallE _ -> ( match seen_calls with No -> SkipOne | _ -> Yes)
  | Il.IterE _ -> seen_calls
  | _ -> Yes

let rewriter_call_e ids_used (call_e_count : call_e_count) (exp : exp) :
    (exp * iter_state) option =
  match call_e_count with
  | Yes -> (
      match exp.it with
      | CallE (_, _, []) -> None
      | CallE (_, _, args) ->
          let id_new, typ_new, iters_new =
            Il.Fresh.var_from_exp TIdMap.empty ids_used exp
          in
          let ids_used = IdSet.add id_new ids_used in
          let var_new = (id_new, typ_new, iters_new) in
          let exp_new = Il.Var.as_exp ~dim:true var_new in
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
    (exp : exp) : ((instr -> instr) * exp * IdSet.t) option =
  match transformer_call_e ids_used call_e_count exp with
  | Some (exp_new_full, iter_state) ->
      let { var_new; iterexps; exp_orig; exp_new; ids_used; _ } = iter_state in
      let id, typ, iters = var_new in
      let iters_enclosing =
        drop (List.length iters - List.length iterexps) iters
      in
      let iter_combined = List.combine iters_enclosing iterexps in
      let iterinstrs, _ =
        List.fold_left
          (fun (iterinstrs, var_bind) (iter_enclosing, iterexp) ->
            let _, vars_bound = iterexp in
            let iterinstr = (iter_enclosing, vars_bound, [ var_bind ]) in
            let var_bind =
              let id, typ, iters = var_bind in
              (id, typ, iters @ [ iter_enclosing ])
            in
            (iterinstrs @ [ iterinstr ], var_bind))
          ([], (id, typ, []))
          iter_combined
      in
      let wrap_in_let body =
        LetI (exp_new, exp_orig, iterinstrs, [ body ])
        $$ (no_region, { iid = -1 })
      in
      Some (wrap_in_let, exp_new_full, ids_used)
  | None -> None

let rec replace_call_exps_first ~(call_e_count : call_e_count)
    (ids_used : IdSet.t) (exps : exp list) :
    ((instr -> instr) * exp list * IdSet.t) option =
  match exps with
  | [] -> None
  | exp_h :: exps_t -> (
      match replace_call_exp ~call_e_count ids_used exp_h with
      | Some (wrap_in_let, exp_h', ids') ->
          Some (wrap_in_let, exp_h' :: exps_t, ids')
      | None ->
          replace_call_exps_first ~call_e_count ids_used exps_t
          |> Option.map (fun (wrap_in_let, exps_t', ids') ->
                 (wrap_in_let, exp_h :: exps_t', ids')))

let expand_nested_calls (ids_used : IdSet.t) (instr : instr) :
    ((instr -> instr) * instr * IdSet.t) option =
  let { it; at; note } = instr in
  let mk it' = it' $$ (at, note) in
  match it with
  | LetI (exp_l, exp_r, iterexps, body) ->
      let* wrap_in_let, exp_r', ids' =
        replace_call_exp ~call_e_count:No ids_used exp_r
      in
      Some (wrap_in_let, mk (LetI (exp_l, exp_r', iterexps, body)), ids')
  | RuleI (id, notexp, inputs, iterexps, body) ->
      let mixop, exps = Mixfix.split notexp in
      let exps_input, exps_output = Hints.Input.split inputs exps in
      let* wrap_in_let, exps_input', ids' =
        replace_call_exps_first ~call_e_count:SkipOne ids_used exps_input
      in
      let exps' = Hints.Input.combine inputs exps_input' exps_output in
      Some
        ( wrap_in_let,
          mk (RuleI (id, Mixfix.fill mixop exps', inputs, iterexps, body)),
          ids' )
  | HoldI (id, notexp, iterexps, holdcase) ->
      let mixop, exps = Mixfix.split notexp in
      let* wrap_in_let, exps', ids' =
        replace_call_exps_first ~call_e_count:SkipOne ids_used exps
      in
      Some
        ( wrap_in_let,
          mk (HoldI (id, Mixfix.fill mixop exps', iterexps, holdcase)),
          ids' )
  | ResultI (rel_signature, exps) ->
      let* wrap_in_let, exps', ids' =
        replace_call_exps_first ~call_e_count:No ids_used exps
      in
      Some (wrap_in_let, mk (ResultI (rel_signature, exps')), ids')
  | ReturnI exp ->
      let* wrap_in_let, exp', ids' =
        replace_call_exp ~call_e_count:No ids_used exp
      in
      Some (wrap_in_let, mk (ReturnI exp'), ids')
  | _ -> None

(* Tree walk *)

let rec expand_block (ids_used : IdSet.t) (block : block) : IdSet.t * block =
  List.fold_left_map expand_instr ids_used block

and expand_instr (ids_used : IdSet.t) (instr : instr) : IdSet.t * instr =
  let { it; at; note } = instr in
  let ids_used, instr = expand_sub_blocks ids_used at note it in
  let rec loop ids_used instr =
    match expand_nested_calls ids_used instr with
    | Some (wrap_in_let, instr, ids_used) ->
        let ids_used, instr = loop ids_used instr in
        loop ids_used (wrap_in_let instr)
    | None -> (ids_used, instr)
  in
  loop ids_used instr

and expand_sub_blocks (ids_used : IdSet.t) (at : Util.Source.region)
    (note : inote) (it : instr') : IdSet.t * instr =
  match it with
  | LetI (exp_l, exp_r, iters, body) ->
      let ids', body' = expand_block ids_used body in
      (ids', LetI (exp_l, exp_r, iters, body') $$ (at, note))
  | RuleI (id, notexp, inputs, iters, body) ->
      let ids', body' = expand_block ids_used body in
      (ids', RuleI (id, notexp, inputs, iters, body') $$ (at, note))
  | IfI (cond, iters, body, dangle) ->
      let ids', body' = expand_block ids_used body in
      (ids', IfI (cond, iters, body', dangle) $$ (at, note))
  | HoldI (id, notexp, iters, holdcase) ->
      let ids', holdcase' = expand_holdcase ids_used holdcase in
      (ids', HoldI (id, notexp, iters, holdcase') $$ (at, note))
  | CaseI (exp, cases, dangle) ->
      let ids', cases' =
        List.fold_left_map
          (fun ids (g, b) ->
            let ids', b' = expand_block ids b in
            (ids', (g, b')))
          ids_used cases
      in
      (ids', CaseI (exp, cases', dangle) $$ (at, note))
  | GroupI (id, sg, exps, body) ->
      let ids', body' = expand_block ids_used body in
      (ids', GroupI (id, sg, exps, body') $$ (at, note))
  | DebugI (exp, instr_inner) ->
      let ids', instr_inner' = expand_instr ids_used instr_inner in
      (ids', DebugI (exp, instr_inner') $$ (at, note))
  | (ResultI _ | ReturnI _) as it -> (ids_used, it $$ (at, note))

and expand_holdcase (ids_used : IdSet.t) (holdcase : holdcase) :
    IdSet.t * holdcase =
  match holdcase with
  | BothH (b1, b2) ->
      let ids, b1' = expand_block ids_used b1 in
      let ids, b2' = expand_block ids b2 in
      (ids, BothH (b1', b2'))
  | HoldH (b, dangle) ->
      let ids, b' = expand_block ids_used b in
      (ids, HoldH (b', dangle))
  | NotHoldH (b, dangle) ->
      let ids, b' = expand_block ids_used b in
      (ids, NotHoldH (b', dangle))

(* Spec-level entry points *)

let expand_block_top (ids_used : IdSet.t) (block : block) : block =
  expand_block ids_used block |> snd

let expand_def (def : def) : def =
  let { it; at; _ } = def in
  let it' =
    match it with
    | RelD (id, sig_, exps, body, elseblock_opt, hints) ->
        let frees =
          IdSet.union (Sl.Free.free_exps exps) (Sl.Free.free_block body)
        in
        let frees =
          match elseblock_opt with
          | Some eb -> IdSet.union frees (Sl.Free.free_block eb)
          | None -> frees
        in
        let body' = expand_block_top frees body in
        let elseblock_opt' =
          Option.map (expand_block_top frees) elseblock_opt
        in
        RelD (id, sig_, exps, body', elseblock_opt', hints)
    | TableDecD (id, params, typ, tablerows, hints) ->
        let tablerows' =
          List.map
            (fun (exps_in, exp_out, body) ->
              let frees =
                IdSet.union
                  (Sl.Free.free_exps exps_in)
                  (IdSet.union (Sl.Free.free_exp exp_out)
                     (Sl.Free.free_block body))
              in
              (exps_in, exp_out, expand_block_top frees body))
            tablerows
        in
        TableDecD (id, params, typ, tablerows', hints)
    | FuncDecD (id, tparams, params, typ, body, elseblock_opt, hints) ->
        let frees =
          IdSet.union (Sl.Free.free_params params) (Sl.Free.free_block body)
        in
        let frees =
          match elseblock_opt with
          | Some eb -> IdSet.union frees (Sl.Free.free_block eb)
          | None -> frees
        in
        let body' = expand_block_top frees body in
        let elseblock_opt' =
          Option.map (expand_block_top frees) elseblock_opt
        in
        FuncDecD (id, tparams, params, typ, body', elseblock_opt', hints)
    | _ -> it
  in
  it' $ at

let expand_spec (spec : spec) : spec = List.map expand_def spec
