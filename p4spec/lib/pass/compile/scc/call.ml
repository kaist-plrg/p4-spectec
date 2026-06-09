open Lang
open Sl

(* ── Reference collection from SL/IL AST ── *)

(* Collect all function/relation names referenced from an expression.
   CallE callee ids and DefA ids (higher-order function arguments) are included.
   VarE ids are local variables, NOT function calls, so they are excluded. *)
let rec refs_exp (exp : exp) : string list =
  match exp.it with
  | CallE (id, _, args) -> id.it :: List.concat_map refs_arg args
  | BoolE _ | NumE _ | TextE _ | VarE _ -> []
  | UnE (_, _, e)
  | LenE e
  | DotE (e, _)
  | UpCastE (_, e)
  | DownCastE (_, e)
  | SubE (e, _)
  | MatchE (e, _) ->
      refs_exp e
  | BinE (_, _, e1, e2)
  | CmpE (_, _, e1, e2)
  | ConsE (e1, e2)
  | CatE (e1, e2)
  | MemE (e1, e2)
  | IdxE (e1, e2) ->
      refs_exp e1 @ refs_exp e2
  | SliceE (e1, e2, e3) -> refs_exp e1 @ refs_exp e2 @ refs_exp e3
  | UpdE (e1, path, e2) -> refs_exp e1 @ refs_path path @ refs_exp e2
  | TupleE es | ListE es -> List.concat_map refs_exp es
  | CaseE notexp -> List.concat_map refs_exp (Domain.Mixfix.args notexp)
  | StrE fields -> List.concat_map (fun (_, e) -> refs_exp e) fields
  | OptE None -> []
  | OptE (Some e) -> refs_exp e
  | IterE (e, _) -> refs_exp e

and refs_arg (arg : arg) : string list =
  match arg.it with ExpA e -> refs_exp e | DefA id -> [ id.it ]

and refs_path (path : path) : string list =
  match path.it with
  | RootP -> []
  | IdxP (p, e) -> refs_path p @ refs_exp e
  | SliceP (p, e1, e2) -> refs_path p @ refs_exp e1 @ refs_exp e2
  | DotP (p, _) -> refs_path p

(* Collect all names referenced from a block.
   Includes function refs from expressions (via CallE/DefA)
   and relation refs from RuleI / HoldI instructions. *)
let rec refs_block (block : block) : string list =
  List.concat_map refs_instr block

and refs_instr (instr : instr) : string list =
  match instr.it with
  | IfI (cond, _, block, _) -> refs_exp cond @ refs_block block
  | HoldI (id, notexp, _, holdcase) ->
      (id.it :: refs_notexp notexp) @ refs_holdcase holdcase
  | CaseI (exp, cases, _) -> refs_exp exp @ List.concat_map refs_case cases
  | GroupI (_, _, exps, block) ->
      List.concat_map refs_exp exps @ refs_block block
  | LetI (exp_l, exp_r, _, block) ->
      refs_exp exp_l @ refs_exp exp_r @ refs_block block
  | RuleI (id, notexp, _, _, block) ->
      (id.it :: refs_notexp notexp) @ refs_block block
  | ResultI (_, exps) -> List.concat_map refs_exp exps
  | ReturnI exp -> refs_exp exp
  | DebugI exp -> refs_exp exp

and refs_notexp (notexp : notexp) : string list =
  List.concat_map refs_exp (Domain.Mixfix.args notexp)

and refs_holdcase (holdcase : holdcase) : string list =
  match holdcase with
  | BothH (b1, b2) -> refs_block b1 @ refs_block b2
  | HoldH (b, _) | NotHoldH (b, _) -> refs_block b

and refs_case ((g, block) : case) : string list =
  refs_guard g @ refs_block block

and refs_guard (guard : guard) : string list =
  match guard with
  | BoolG _ | SubG _ | MatchG _ -> []
  | CmpG (_, _, exp) | MemG exp -> refs_exp exp

(* Count ExpP params in a Sl.param list (used for arity matching) *)
let count_expp (params : param list) : int =
  List.length
    (List.filter
       (fun (p : param) -> match p.it with ExpP (_, _) -> true | _ -> false)
       params)

(* Collect refs from param binding expressions (ExpP's exp field) *)
let refs_params (params : param list) : string list =
  List.concat_map
    (fun (p : param) ->
      match p.it with ExpP (_, e) -> refs_exp e | DefP _ -> [])
    params

(* Returns (all_refs, defp_arities) for a def.
   all_refs: all function/relation names referenced in the body.
   defp_arities: for each DefP param, the count of its own ExpP sub-params
                 (= the value-argument arity of the higher-order function). *)
let collect_refs_def (def : def) : string list * int list =
  let defp_arities params =
    List.filter_map
      (fun (p : param) ->
        match p.it with DefP (_, _, dp, _) -> Some (count_expp dp) | _ -> None)
      params
  in
  match def.it with
  | FuncDecD (_, _, params, _, block, elseblock_opt, _) ->
      let all_blocks = block @ Option.value ~default:[] elseblock_opt in
      (refs_params params @ refs_block all_blocks, defp_arities params)
  | TableDecD (_, params, _, tablerows, _) ->
      let blocks = List.concat_map (fun (_, _, b) -> b) tablerows in
      (refs_params params @ refs_block blocks, defp_arities params)
  | RelD (_, _, exps, block, elseblock_opt, _) ->
      let all_blocks = block @ Option.value ~default:[] elseblock_opt in
      let exp_refs = List.concat_map refs_exp exps in
      (exp_refs @ refs_block all_blocks, [])
  | ExternDecD _ | BuiltinDecD _ | ExternRelD _ -> ([], [])
  | ExternTypD _ | TypD _ | VarD _ -> ([], [])

(* ── Node classification ── *)

(* True for defs that compile to OCaml let-bindings (func or rel, concrete) *)
let is_entity (def : def) : bool =
  match def.it with
  | FuncDecD (_, tparams, _, _, _, _, _) -> tparams = []
  | ExternDecD (_, tparams, _, _, _) -> tparams = []
  | BuiltinDecD (_, tparams, _, _, _) -> tparams = []
  | TableDecD _ | RelD _ | ExternRelD _ -> true
  | _ -> false

let def_id (def : def) : string =
  match def.it with
  | FuncDecD (id, _, _, _, _, _, _) -> id.it
  | ExternDecD (id, _, _, _, _) -> id.it
  | BuiltinDecD (id, _, _, _, _) -> id.it
  | TableDecD (id, _, _, _, _) -> id.it
  | RelD (id, _, _, _, _, _) -> id.it
  | ExternRelD (id, _, _, _) -> id.it
  | _ -> assert false

(* Returns Some arity (# ExpP params) for function defs, None for rels.
   Used to build the DefP conservative-edge targets: only functions, not rels. *)
let func_expp_arity (def : def) : int option =
  match def.it with
  | FuncDecD (_, _, params, _, _, _, _) -> Some (count_expp params)
  | ExternDecD (_, _, params, _, _) -> Some (count_expp params)
  | BuiltinDecD (_, _, params, _, _) -> Some (count_expp params)
  | TableDecD (_, params, _, _, _) -> Some (count_expp params)
  | RelD _ | ExternRelD _ -> None
  | _ -> None

(* ── Entry point ── *)

(* Given a monomorphized SL spec, compute SCCs on the function/relation call
   graph and return binding groups in topological order (dependencies first).
   Each group becomes one Ml.LetRec in codegen. *)
let compute (spec : spec) : def list list =
  let defs = List.filter is_entity spec in
  let n = List.length defs in
  if n = 0 then []
  else
    let defs_arr = Array.of_list defs in
    (* name -> node index *)
    let name_idx : (string, int) Hashtbl.t = Hashtbl.create (n * 2) in
    Array.iteri (fun i def -> Hashtbl.replace name_idx (def_id def) i) defs_arr;
    (* node index -> ExpP arity (functions only; rels absent from this table) *)
    let func_arity : (int, int) Hashtbl.t = Hashtbl.create n in
    Array.iteri
      (fun i def ->
        match func_expp_arity def with
        | Some a -> Hashtbl.replace func_arity i a
        | None -> ())
      defs_arr;
    (* build adjacency list *)
    let adj = Array.make n [] in
    Array.iteri
      (fun i def ->
        let all_refs, defp_arities = collect_refs_def def in
        let edges : (int, unit) Hashtbl.t = Hashtbl.create 8 in
        (* direct call/use edges *)
        List.iter
          (fun name ->
            match Hashtbl.find_opt name_idx name with
            | Some j when j <> i -> Hashtbl.replace edges j ()
            | _ -> ())
          all_refs;
        (* DefP conservative edges: to every function def with matching ExpP arity.
           Rels are excluded because DefP can only stand for a function, not a relation. *)
        List.iter
          (fun arity ->
            Hashtbl.iter
              (fun j j_arity ->
                if j <> i && j_arity = arity then Hashtbl.replace edges j ())
              func_arity)
          defp_arities;
        adj.(i) <- Hashtbl.fold (fun j () acc -> j :: acc) edges [])
      defs_arr;
    let sccs = Tarjan.tarjan n adj in
    List.map (fun scc -> List.map (fun i -> defs_arr.(i)) scc) sccs
