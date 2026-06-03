open Lang
open Util.Source
module Mixfix = Domain.Mixfix
module StringSet = Collect.StringSet

(* ===== Dispatch table types ===== *)

type dispatch_kind = Builtin | Extern

type dispatch_info = {
  original_name : string;
  concrete_targs : Il.typ list;
  kind : dispatch_kind;
}

(* ===== Call-site rewriting ===== *)

(* Rewrite all CallE nodes in an expression tree:
   - CallE(id, targs, args) where id.it ∈ poly_funcs and targs ≠ []
     → CallE({id with it = mangled_name}, [], args)
   - All other nodes are left unchanged (types are not substituted here). *)

let rec rewrite_exp (poly_funcs : StringSet.t) (exp : Il.exp) : Il.exp =
  { exp with it = rewrite_exp' poly_funcs exp.it }

and rewrite_exp' (poly_funcs : StringSet.t) (exp' : Il.exp') : Il.exp' =
  let re = rewrite_exp poly_funcs in
  match exp' with
  | Il.BoolE _ | Il.NumE _ | Il.TextE _ | Il.VarE _ -> exp'
  | Il.UnE (op, ot, exp) -> Il.UnE (op, ot, re exp)
  | Il.BinE (op, ot, exp_l, exp_r) -> Il.BinE (op, ot, re exp_l, re exp_r)
  | Il.CmpE (op, ot, exp_l, exp_r) -> Il.CmpE (op, ot, re exp_l, re exp_r)
  | Il.UpCastE (typ, exp) -> Il.UpCastE (typ, re exp)
  | Il.DownCastE (typ, exp) -> Il.DownCastE (typ, re exp)
  | Il.SubE (exp, typ) -> Il.SubE (re exp, typ)
  | Il.MatchE (exp, pat) -> Il.MatchE (re exp, pat)
  | Il.TupleE exps -> Il.TupleE (List.map re exps)
  | Il.CaseE notexp -> Il.CaseE (Mixfix.map re notexp)
  | Il.StrE expfields ->
      Il.StrE (List.map (fun (atom, exp) -> (atom, re exp)) expfields)
  | Il.OptE (Some exp) -> Il.OptE (Some (re exp))
  | Il.OptE None -> Il.OptE None
  | Il.ListE exps -> Il.ListE (List.map re exps)
  | Il.ConsE (exp_h, exp_t) -> Il.ConsE (re exp_h, re exp_t)
  | Il.CatE (exp_l, exp_r) -> Il.CatE (re exp_l, re exp_r)
  | Il.MemE (exp_e, exp_s) -> Il.MemE (re exp_e, re exp_s)
  | Il.LenE exp -> Il.LenE (re exp)
  | Il.DotE (exp, atom) -> Il.DotE (re exp, atom)
  | Il.IdxE (exp_l, exp_r) -> Il.IdxE (re exp_l, re exp_r)
  | Il.SliceE (exp_l, exp_m, exp_r) -> Il.SliceE (re exp_l, re exp_m, re exp_r)
  | Il.UpdE (exp_b, path, exp_u) ->
      Il.UpdE (re exp_b, rewrite_path poly_funcs path, re exp_u)
  | Il.CallE (id, targs, args)
    when targs <> [] && StringSet.mem id.it poly_funcs ->
      let mangled = Name.mangle id.it targs in
      Il.CallE
        ({ id with it = mangled }, [], List.map (rewrite_arg poly_funcs) args)
  | Il.CallE (id, targs, args) ->
      Il.CallE (id, targs, List.map (rewrite_arg poly_funcs) args)
  | Il.IterE (exp, iterexp) -> Il.IterE (re exp, iterexp)

and rewrite_path (poly_funcs : StringSet.t) (path : Il.path) : Il.path =
  { path with it = rewrite_path' poly_funcs path.it }

and rewrite_path' (poly_funcs : StringSet.t) (path' : Il.path') : Il.path' =
  let re = rewrite_exp poly_funcs in
  match path' with
  | Il.RootP -> Il.RootP
  | Il.IdxP (path, exp) -> Il.IdxP (rewrite_path poly_funcs path, re exp)
  | Il.SliceP (path, exp_l, exp_r) ->
      Il.SliceP (rewrite_path poly_funcs path, re exp_l, re exp_r)
  | Il.DotP (path, atom) -> Il.DotP (rewrite_path poly_funcs path, atom)

and rewrite_arg (poly_funcs : StringSet.t) (arg : Il.arg) : Il.arg =
  match arg.it with
  | Il.ExpA exp -> Il.ExpA (rewrite_exp poly_funcs exp) $ arg.at
  | Il.DefA _ -> arg

let rewrite_notexp (poly_funcs : StringSet.t) (notexp : Sl.notexp) : Sl.notexp =
  Mixfix.map (rewrite_exp poly_funcs) notexp

let rewrite_guard (poly_funcs : StringSet.t) (guard : Sl.guard) : Sl.guard =
  match guard with
  | Sl.BoolG _ | Sl.MatchG _ -> guard
  | Sl.CmpG (op, ot, exp) -> Sl.CmpG (op, ot, rewrite_exp poly_funcs exp)
  | Sl.SubG _ -> guard
  | Sl.MemG exp -> Sl.MemG (rewrite_exp poly_funcs exp)

let rec rewrite_instr (poly_funcs : StringSet.t) (instr : Sl.instr) : Sl.instr =
  { instr with it = rewrite_instr' poly_funcs instr.it }

and rewrite_instr' (poly_funcs : StringSet.t) (instr' : Sl.instr') : Sl.instr' =
  let re = rewrite_exp poly_funcs in
  let rb = rewrite_block poly_funcs in
  match instr' with
  | Sl.IfI (exp, iterexps, block, dangle) ->
      Sl.IfI (re exp, iterexps, rb block, dangle)
  | Sl.HoldI (id, notexp, iterexps, holdcase) ->
      Sl.HoldI
        ( id,
          rewrite_notexp poly_funcs notexp,
          iterexps,
          rewrite_holdcase poly_funcs holdcase )
  | Sl.CaseI (exp, cases, dangle) ->
      Sl.CaseI
        ( re exp,
          List.map
            (fun (guard, block) -> (rewrite_guard poly_funcs guard, rb block))
            cases,
          dangle )
  | Sl.GroupI (id, rel_sig, exps, block) ->
      Sl.GroupI (id, rel_sig, List.map re exps, rb block)
  | Sl.LetI (lhs_exp, rhs_exp, iterinstrs, block) ->
      Sl.LetI (re lhs_exp, re rhs_exp, iterinstrs, rb block)
  | Sl.RuleI (id, notexp, hints, iterinstrs, block) ->
      Sl.RuleI
        (id, rewrite_notexp poly_funcs notexp, hints, iterinstrs, rb block)
  | Sl.ResultI (rel_sig, exps) -> Sl.ResultI (rel_sig, List.map re exps)
  | Sl.ReturnI exp -> Sl.ReturnI (re exp)
  | Sl.DebugI exp -> Sl.DebugI (re exp)

and rewrite_block (poly_funcs : StringSet.t) (block : Sl.block) : Sl.block =
  List.map (rewrite_instr poly_funcs) block

and rewrite_holdcase (poly_funcs : StringSet.t) (holdcase : Sl.holdcase) :
    Sl.holdcase =
  let rb = rewrite_block poly_funcs in
  match holdcase with
  | Sl.BothH (block_hold, block_nhold) ->
      Sl.BothH (rb block_hold, rb block_nhold)
  | Sl.HoldH (block, dangle) -> Sl.HoldH (rb block, dangle)
  | Sl.NotHoldH (block, dangle) -> Sl.NotHoldH (rb block, dangle)

let rewrite_def (poly_funcs : StringSet.t) (def : Sl.def) : Sl.def =
  let rb = rewrite_block poly_funcs in
  match def.it with
  | Sl.FuncDecD (id, tparams, params, typ_ret, block, elseblock, hints) ->
      {
        def with
        it =
          Sl.FuncDecD
            ( id,
              tparams,
              params,
              typ_ret,
              rb block,
              Option.map rb elseblock,
              hints );
      }
  | Sl.RelD (id, rel_sig, exps, block, elseblock, hints) ->
      {
        def with
        it =
          Sl.RelD
            ( id,
              rel_sig,
              List.map (rewrite_exp poly_funcs) exps,
              rb block,
              Option.map rb elseblock,
              hints );
      }
  | _ -> def

let rewrite_spec (poly_funcs : StringSet.t) (spec : Sl.spec) : Sl.spec =
  List.map (rewrite_def poly_funcs) spec

(* ===== Worklist driver ===== *)

let monomorphize (spec : Sl.spec) : Sl.spec * (string, dispatch_info) Hashtbl.t
    =
  (* Step 1: collect all poly FuncDecD / BuiltinDecD / ExternDecD ids *)
  let poly_funcs =
    List.fold_left
      (fun acc def ->
        match def.it with
        | Sl.FuncDecD (id, tparams, _, _, _, _, _) when tparams <> [] ->
            StringSet.add id.it acc
        | Sl.BuiltinDecD (id, tparams, _, _, _) when tparams <> [] ->
            StringSet.add id.it acc
        | Sl.ExternDecD (id, tparams, _, _, _) when tparams <> [] ->
            StringSet.add id.it acc
        | _ -> acc)
      StringSet.empty spec
  in
  let dispatch_table : (string, dispatch_info) Hashtbl.t = Hashtbl.create 16 in
  (* Fast path: no polymorphic functions *)
  if StringSet.is_empty poly_funcs then (spec, dispatch_table)
  else
    (* Step 2: build function tables for each declaration kind *)
    let func_table : (string, Sl.definedfunc) Hashtbl.t = Hashtbl.create 64 in
    let builtin_table : (string, Sl.builtinfunc) Hashtbl.t =
      Hashtbl.create 16
    in
    let extern_table : (string, Sl.externfunc) Hashtbl.t = Hashtbl.create 16 in
    List.iter
      (fun def ->
        match def.it with
        | Sl.FuncDecD (id, tparams, params, typ_ret, block, elseblock, hints)
          when StringSet.mem id.it poly_funcs ->
            Hashtbl.replace func_table id.it
              (id, tparams, params, typ_ret, block, elseblock, hints)
        | Sl.BuiltinDecD (id, tparams, params, typ_ret, hints)
          when StringSet.mem id.it poly_funcs ->
            Hashtbl.replace builtin_table id.it
              (id, tparams, params, typ_ret, hints)
        | Sl.ExternDecD (id, tparams, params, typ_ret, hints)
          when StringSet.mem id.it poly_funcs ->
            Hashtbl.replace extern_table id.it
              (id, tparams, params, typ_ret, hints)
        | _ -> ())
      spec;
    (* Step 3: seed worklist from spec-wide call sites *)
    let initial_sites = Collect.collect_call_sites ~poly_funcs spec in
    (* Use mangled name as done-set key — avoids source-location false-inequalities
       in targs (same type structure, different .at regions → same key). *)
    let worklist : (string * Il.typ list) Queue.t = Queue.create () in
    List.iter (fun site -> Queue.push site worklist) initial_sites;
    let done_set : (string, unit) Hashtbl.t = Hashtbl.create 64 in
    let new_defs : Sl.def list ref = ref [] in
    (* Step 4: process worklist *)
    while not (Queue.is_empty worklist) do
      let func_id, targs = Queue.pop worklist in
      let mangled_key = Name.mangle func_id targs in
      if not (Hashtbl.mem done_set mangled_key) then (
        Hashtbl.replace done_set mangled_key ();
        let new_def_opt =
          match Hashtbl.find_opt func_table func_id with
          | Some ((id, tparams, _, _, _, _, _) as dfunc) ->
              let theta = Specialize.build_theta tparams targs in
              let new_name = Name.mangle func_id targs in
              let new_def = Specialize.specialize dfunc theta ~new_name in
              ignore id;
              Some new_def
          | None -> (
              match Hashtbl.find_opt builtin_table func_id with
              | Some ((id, tparams, _, _, _) as bfunc) ->
                  let theta = Specialize.build_theta tparams targs in
                  let new_name = Name.mangle func_id targs in
                  Hashtbl.replace dispatch_table new_name
                    {
                      original_name = func_id;
                      concrete_targs = targs;
                      kind = Builtin;
                    };
                  let new_def =
                    Specialize.specialize_builtin bfunc theta ~new_name
                  in
                  ignore id;
                  Some new_def
              | None -> (
                  match Hashtbl.find_opt extern_table func_id with
                  | Some ((id, tparams, _, _, _) as efunc) ->
                      let theta = Specialize.build_theta tparams targs in
                      let new_name = Name.mangle func_id targs in
                      Hashtbl.replace dispatch_table new_name
                        {
                          original_name = func_id;
                          concrete_targs = targs;
                          kind = Extern;
                        };
                      let new_def =
                        Specialize.specialize_extern efunc theta ~new_name
                      in
                      ignore id;
                      Some new_def
                  | None -> None))
        in
        match new_def_opt with
        | None -> ()
        | Some new_def ->
            new_defs := new_def :: !new_defs;
            (* scan new def body for more poly calls (only FuncDecDs have bodies) *)
            let new_spec_fragment = [ new_def ] in
            let more_sites =
              Collect.collect_call_sites ~poly_funcs new_spec_fragment
            in
            List.iter
              (fun (site_id, site_targs) ->
                let site_key = Name.mangle site_id site_targs in
                if not (Hashtbl.mem done_set site_key) then
                  Queue.push (site_id, site_targs) worklist)
              more_sites)
    done;
    (* Step 5: rewrite all call sites in the full spec (including new defs) *)
    let combined_spec = spec @ List.rev !new_defs in
    let rewritten = rewrite_spec poly_funcs combined_spec in
    (* Step 6: remove original poly FuncDecDs/BuiltinDecDs/ExternDecDs *)
    let filtered =
      List.filter
        (fun def ->
          match def.it with
          | Sl.FuncDecD (id, tparams, _, _, _, _, _) ->
              not (tparams <> [] && StringSet.mem id.it poly_funcs)
          | Sl.BuiltinDecD (id, tparams, _, _, _) ->
              not (tparams <> [] && StringSet.mem id.it poly_funcs)
          | Sl.ExternDecD (id, tparams, _, _, _) ->
              not (tparams <> [] && StringSet.mem id.it poly_funcs)
          | _ -> true)
        rewritten
    in
    (filtered, dispatch_table)
