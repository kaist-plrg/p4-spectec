open Domain
open Lib
open Lang
module Walk = Il.Walk.Collect
module Typ = Runtime.Type
module Typdef = Typ.Typdef
open Util.Source

(* Collect: discovers every type needing marshal_T/unmarshal_T

   [collect_types] does a BFS from every function/relation signature's param
   and return types — e.g. [def $f(x : bool) : nat] enqueues [bool]/[nat],
   then follows struct/variant fields transitively. [collect_targs] seeds a
   few more: a ground type instantiating a *generic* callee, e.g.
   [$rev<int>(..)] inside some other function's body. *)

let rec enqueue_ground (enqueue : Sl.typ -> unit) (tparams : string list)
    (typ : Sl.typ) : unit =
  if not (Type.is_generic tparams typ) then enqueue typ
  else
    match typ.it with
    | Il.VarT (_, targs) -> List.iter (enqueue_ground enqueue tparams) targs
    | Il.TupleT typs -> List.iter (enqueue_ground enqueue tparams) typs
    | Il.IterT (t, _) -> enqueue_ground enqueue tparams t
    | _ -> ()

(* A collector that, at every call to a generic function, enqueues the
   ground parts of its type arguments. *)

let make_collector (ctx : Ctx.t) (enqueue : Sl.typ -> unit)
    (tparams : string list) : unit Walk.collector =
  let collect_exp c exp =
    (match exp.it with
    | Il.CallE (id, targs, _)
      when match Ctx.find_func_tparams ctx id.it with
           | Some (_ :: _) -> true
           | Some [] | None -> false ->
        List.iter (enqueue_ground enqueue tparams) targs
    | _ -> ());
    Walk.default_collect_exp c exp
  in
  { (Walk.make_base ~default:() ~compose:(fun () () -> ())) with collect_exp }

let visit_exps c exps = List.iter (fun exp -> Walk.collect_exp c exp) exps

let rec visit_block c block = List.iter (visit_instr c) block

and visit_instr c instr =
  match instr.it with
  | Sl.IfI (exp, _, block, _) ->
      visit_exps c [ exp ];
      visit_block c block
  | Sl.HoldI (_, notexp, _, holdcase) ->
      visit_exps c (Mixfix.args notexp);
      visit_holdcase c holdcase
  | Sl.CaseI (exp, cases, _) ->
      visit_exps c [ exp ];
      List.iter (fun (_, block) -> visit_block c block) cases
  | Sl.GroupI (_, _, exps, block) ->
      visit_exps c exps;
      visit_block c block
  | Sl.LetI (lhs, rhs, _, block) ->
      visit_exps c [ lhs; rhs ];
      visit_block c block
  | Sl.RuleI (_, notexp, _, _, block) ->
      visit_exps c (Mixfix.args notexp);
      visit_block c block
  | Sl.ResultI (_, exps) -> visit_exps c exps
  | Sl.ReturnI exp -> visit_exps c [ exp ]
  | Sl.DebugI exp -> visit_exps c [ exp ]

and visit_holdcase c = function
  | Sl.BothH (block_hold, block_nhold) ->
      visit_block c block_hold;
      visit_block c block_nhold
  | Sl.HoldH (block, _) | Sl.NotHoldH (block, _) -> visit_block c block

(* Seed the BFS with ground types instantiating a generic callee's type parameters *)

let collect_targs (ctx : Ctx.t) ~(enqueue : Sl.typ -> unit) (spec : Sl.spec) :
    unit =
  List.iter
    (fun def ->
      match def.it with
      | Sl.FuncDecD (_, tparams, _, _, block, elseblock, _) ->
          let c =
            make_collector ctx enqueue
              (List.map (fun (tp : Il.tparam) -> tp.it) tparams)
          in
          visit_block c block;
          Option.iter (visit_block c) elseblock
      | Sl.RelD (_, _, exps, block, elseblock, _) ->
          let c = make_collector ctx enqueue [] in
          visit_exps c exps;
          visit_block c block;
          Option.iter (visit_block c) elseblock
      | Sl.TableDecD (_, _, _, tablerows, _) ->
          let c = make_collector ctx enqueue [] in
          List.iter (fun (_, _, block_row) -> visit_block c block_row) tablerows
      | _ -> ())
    spec

(* Enqueue every [ExpP] param's type *)

let enqueue_params (enqueue : Sl.typ -> unit) (params : Sl.param list) : unit =
  List.iter
    (fun param ->
      match param.it with Sl.ExpP (typ, _) -> enqueue typ | Sl.DefP _ -> ())
    params

(* Seed the BFS from every function/relation signature's param and return types *)

let seed_from_signatures (enqueue : Sl.typ -> unit) (spec : Sl.spec) : unit =
  List.iter
    (fun def ->
      match def.it with
      | Sl.FuncDecD (_, [], params, typ_ret, _, _, _) ->
          enqueue_params enqueue params;
          enqueue typ_ret
      | Sl.BuiltinDecD (_, [], params, typ_ret, _) ->
          enqueue_params enqueue params;
          enqueue typ_ret
      | Sl.ExternDecD (_, [], params, typ_ret, _) ->
          enqueue_params enqueue params;
          enqueue typ_ret
      | Sl.TableDecD (_, params, typ_ret, _, _) ->
          enqueue_params enqueue params;
          enqueue typ_ret
      | Sl.ExternRelD (_, (nottyp, inputs), _, _)
      | Sl.RelD (_, (nottyp, inputs), _, _, _, _) ->
          let typs_rel = Mixfix.args nottyp.it in
          let typs_input, typs_output = Hints.Input.split inputs typs_rel in
          List.iter enqueue typs_input;
          List.iter enqueue typs_output
      | _ -> ())
    spec

(* Enqueue [typ]'s immediate structural children *)

let expand_typ (ctx : Ctx.t) (enqueue : Sl.typ -> unit) (typ : Sl.typ) : unit =
  match typ.it with
  | Il.BoolT | Il.NumT _ | Il.TextT | Il.FuncT _ -> ()
  | Il.TupleT typs -> List.iter enqueue typs
  | Il.IterT (t, _) -> enqueue t
  | Il.VarT (id, targs) -> (
      match Ctx.find_typdef_opt ctx id with
      | None | Some Typdef.Extern | Some Typdef.Param | Some (Typdef.Defining _)
        ->
          ()
      | Some (Typdef.Defined (tparams, deftyp))
        when List.length tparams = List.length targs -> (
          let theta = TIdMap.of_lists tparams targs in
          match deftyp.it with
          | Il.PlainT typ ->
              let typ = Typ.Subst.subst_typ theta typ in
              enqueue typ
          | Il.StructT typfields ->
              List.iter
                (fun (_, typ) ->
                  let typ = Typ.Subst.subst_typ theta typ in
                  enqueue typ)
                typfields
          | Il.VariantT typcases ->
              List.iter
                (fun (nottyp, _, _) ->
                  let typs = Mixfix.args nottyp.it in
                  let typs = Typ.Subst.subst_typs theta typs in
                  List.iter enqueue typs)
                typcases)
      | _ -> ())

(* BFS over all types reachable from function/relation signatures. *)

let collect_types (ctx : Ctx.t) (spec : Sl.spec) : Sl.typ list =
  let seen : (string, unit) Hashtbl.t = Hashtbl.create 32 in
  let queue : Sl.typ Queue.t = Queue.create () in
  let enqueue typ =
    let name = Naming.name typ in
    if not (Hashtbl.mem seen name) then (
      Hashtbl.replace seen name ();
      Queue.push typ queue)
  in
  seed_from_signatures enqueue spec;
  collect_targs ctx ~enqueue spec;
  let result = ref [] in
  while not (Queue.is_empty queue) do
    let typ = Queue.pop queue in
    result := typ :: !result;
    expand_typ ctx enqueue typ
  done;
  List.rev !result
