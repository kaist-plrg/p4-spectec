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

let rec remove_last l =
  match l with [] -> [] | [ _ ] -> [] | hd :: tl -> hd :: remove_last tl

let recompute_iterators (ids_extracted : IdSet.t) var_new exp =
  let rec reiterate var_new e =
    let exp' =
      match e.it with
      | IterE (exp_inner, (iter, vars)) ->
          let ids_occuring = Free.free_exp exp_inner in
          let vars, var_new_inner =
            if List.exists (fun (id, _, _) -> IdSet.mem id ids_extracted) vars
            then
              let id, typ, iters = var_new in
              (vars @ [ var_new ], (id, typ, remove_last iters))
            else (vars, var_new)
          in
          let vars =
            List.filter (fun (id, _, _) -> IdSet.mem id ids_occuring) vars
          in
          IterE (reiterate var_new_inner exp_inner, (iter, vars))
      | BoolE _ | NumE _ | TextE _ | VarE _ -> e.it
      | UnE (unop, optyp, exp) -> UnE (unop, optyp, reiterate var_new exp)
      | BinE (binop, optyp, exp_l, exp_r) ->
          BinE (binop, optyp, reiterate var_new exp_l, reiterate var_new exp_r)
      | CmpE (cmpop, optyp, exp_l, exp_r) ->
          CmpE (cmpop, optyp, reiterate var_new exp_l, reiterate var_new exp_r)
      | UpCastE (typ, exp) -> UpCastE (typ, reiterate var_new exp)
      | DownCastE (typ, exp) -> DownCastE (typ, reiterate var_new exp)
      | SubE (exp, typ) -> SubE (reiterate var_new exp, typ)
      | MatchE (exp, pattern) -> MatchE (reiterate var_new exp, pattern)
      | TupleE exps -> TupleE (List.map (reiterate var_new) exps)
      | CaseE (id, mixop, exps, hint) ->
          CaseE (id, mixop, List.map (reiterate var_new) exps, hint)
      | StrE fields ->
          StrE
            (List.map (fun (atom, exp) -> (atom, reiterate var_new exp)) fields)
      | OptE (Some exp) -> OptE (Some (reiterate var_new exp))
      | OptE None -> e.it
      | ListE exps -> ListE (List.map (reiterate var_new) exps)
      | ConsE (exp_h, exp_t) ->
          ConsE (reiterate var_new exp_h, reiterate var_new exp_t)
      | CatE (exp_l, exp_r) ->
          CatE (reiterate var_new exp_l, reiterate var_new exp_r)
      | MemE (exp_l, exp_r) ->
          MemE (reiterate var_new exp_l, reiterate var_new exp_r)
      | LenE exp -> LenE (reiterate var_new exp)
      | DotE (exp, atom) -> DotE (reiterate var_new exp, atom)
      | IdxE (exp_b, exp_i) ->
          IdxE (reiterate var_new exp_b, reiterate var_new exp_i)
      | SliceE (exp_b, exp_l, exp_h) ->
          SliceE
            ( reiterate var_new exp_b,
              reiterate var_new exp_l,
              reiterate var_new exp_h )
      | UpdE (exp_b, path, exp_f) ->
          UpdE (reiterate var_new exp_b, path, reiterate var_new exp_f)
      | CallE (funcprose, targs, args) ->
          CallE
            ( funcprose,
              targs,
              List.map
                (fun arg ->
                  match arg.it with
                  | ExpA e -> ExpA (reiterate var_new e) $ arg.at
                  | DefA _ -> arg)
                args )
    in
    exp' $$ (e.at, e.note)
  in
  reiterate var_new exp

let replace_call_exp exp =
  let ids_extracted = ref IdSet.empty in
  let typ_extracted = ref Il.Ast.FuncT in
  let transformer e (acc : iterexp list) =
    match e.it with
    | CallE (_funcprose, _targs, args) ->
        ids_extracted := Free.free_args args;
        typ_extracted := e.note;
        Some (e, acc)
    | _ -> None
  in
  let folder acc e =
    match e.it with IterE (_, iterexp) -> iterexp :: acc | _ -> acc
  in
  (* rewrite CallE to VarE, and collect enclosing iterexps *)
  match Transform.transform_first_with_acc transformer folder [] exp with
  | Some (exp, iterexps) ->
      (* compute dimension of var_new *)
      (* construct leading instruction enclosed in necessary iterexps *)
      let iters = iterexps |> List.rev |> List.map fst in
      let var_new = ("" $ no_region, !typ_extracted $ no_region, iters) in
      (* recompute iterexps in original expression *)
      recompute_iterators !ids_extracted var_new exp |> Option.some
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
