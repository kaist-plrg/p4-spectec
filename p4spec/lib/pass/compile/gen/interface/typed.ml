open Lang
module Typdef = Runtime.Type.Typdef
module Expand = Runtime.Type.Expand
module Mixop = Domain.Mixop
open Util.Source

(* Typed dispatch: [marshal_typed]/[unmarshal_typed] keyed on [Typ.t].
   A match on the head constructor of [typ.it] dispatches to the per-type
   [marshal_<name>] / [unmarshal_<name>] generated in the same Dispatch module.
   Structural arms handle bare [IterT]/[TupleT] heads inline. *)

(* [marshal_typed]/[unmarshal_typed]: Typ.t-dispatched persist bridge.
   Named-type arms delegate to the generated per-type functions; structural
   arms handle [IterT]/[TupleT] recursively without a named helper. *)
let compile_marshal_dispatch (typs : Sl.typ list) : Ml.funcdef list =
  let keys =
    List.filter_map
      (fun (t : Sl.typ) ->
        match t.it with
        | Il.VarT (id, _) -> Some (id.it, Naming.name t)
        | _ -> None)
      typs
    |> List.sort_uniq compare
  in
  let scrut = Ml.FieldE (Ml.VarE "typ", "it") in
  let var_pat key =
    Ml.LitP (Printf.sprintf "Il.VarT ({ it = %S; _ }, _)" key)
  in
  let marshal_arms_named =
    List.map
      (fun (key, iname) ->
        ( var_pat key,
          Ml.AppE
            ( Ml.LitE ("marshal_" ^ iname),
              [ Ml.AppE (Ml.LitE "Obj.obj", [ Ml.VarE "x" ]) ] ) ))
      keys
  in
  let unmarshal_arms_named =
    List.map
      (fun (key, iname) ->
        ( var_pat key,
          Ml.AppE
            ( Ml.LitE "Obj.repr",
              [ Ml.AppE (Ml.VarE ("unmarshal_" ^ iname), [ Ml.VarE "v" ]) ] )
        ))
      keys
  in
  (* Structural arms: IterT/TupleT — recurse via marshal_typed/unmarshal_typed *)
  let marshal_arm_list =
    ( Ml.LitP "Il.IterT (elem__, Il.List)",
      Ml.LitE
        "(let xs : Obj.t list = Obj.obj x in\n\
        \  Value.Make.list\n\
        \    (Typ.Make.iter elem__ Il.List)\n\
        \    (List.map (marshal_typed elem__) xs))" )
  in
  let marshal_arm_opt =
    ( Ml.LitP "Il.IterT (elem__, Il.Opt)",
      Ml.LitE
        "(let xo : Obj.t option = Obj.obj x in\n\
        \  Value.Make.opt\n\
        \    (Typ.Make.iter elem__ Il.Opt)\n\
        \    (Option.map (marshal_typed elem__) xo))" )
  in
  let marshal_arm_tuple =
    ( Ml.LitP "Il.TupleT elems__",
      Ml.LitE
        "(let xs : Obj.t list = Obj.obj x in\n\
        \  Value.Make.tuple\n\
        \    (Typ.Make.tuple elems__)\n\
        \    (List.map2 marshal_typed elems__ xs))" )
  in
  let unmarshal_arm_list =
    ( Ml.LitP "Il.IterT (elem__, Il.List)",
      Ml.LitE
        "(let vs = Value.Get.list v in\n\
        \  Obj.repr (List.map (unmarshal_typed elem__) vs))" )
  in
  let unmarshal_arm_opt =
    ( Ml.LitP "Il.IterT (elem__, Il.Opt)",
      Ml.LitE
        "(let vo = Value.Get.opt v in\n\
        \  Obj.repr (Option.map (unmarshal_typed elem__) vo))" )
  in
  let unmarshal_arm_tuple =
    ( Ml.LitP "Il.TupleT elems__",
      Ml.LitE
        "(let vs = Value.Get.tuple v in\n\
        \  Obj.repr (List.map2 unmarshal_typed elems__ vs))" )
  in
  let wild name =
    ( Ml.WildP,
      Ml.AppE
        ( Ml.LitE "failwith",
          [
            Ml.BinopE
              ( "^",
                Ml.StrE (name ^ ": unknown type "),
                Ml.AppE (Ml.LitE "Typ.to_string", [ Ml.VarE "typ" ]) );
          ] ) )
  in
  let marshal_arms =
    marshal_arms_named
    @ [ marshal_arm_list; marshal_arm_opt; marshal_arm_tuple; wild "marshal_typed" ]
  in
  let unmarshal_arms =
    unmarshal_arms_named
    @ [
        unmarshal_arm_list;
        unmarshal_arm_opt;
        unmarshal_arm_tuple;
        wild "unmarshal_typed";
      ]
  in
  [
    ( "marshal_typed",
      [],
      [ ("typ", Some (Ml.NameT "Typ.t")); ("x", Some (Ml.NameT "Obj.t")) ],
      Some (Ml.NameT "Value.t"),
      Ml.MatchE (scrut, marshal_arms) );
    ( "unmarshal_typed",
      [],
      [ ("typ", Some (Ml.NameT "Typ.t")); ("v", Some (Ml.NameT "Value.t")) ],
      Some (Ml.NameT "Obj.t"),
      Ml.MatchE (scrut, unmarshal_arms) );
  ]

(* Typed mixop bridges: [case_of_typed]/[make_case_typed] convert between a
   value's native (typed [Obj.t]) representation and its [Mixfix.t] shape.
   The caller threads the value's spec type as a structured [Il.typ]; each arm
   keys on the type NAME [id.it], read once from the [VarT] head.

   Parametric variants ([set]/[pair]/[res]) and their aliases ([map]/[valres]/
   [valsres]) fold into the normal path: the alias resolves to its underlying
   variant, and every type parameter erases to [Obj.t] (all instantiations
   share one arm). Every inner match ends in a wildcard so a wrong runtime ctor
   fails fast — without it a single-ctor match skips the tag check and would
   segfault on the projection. *)

(* Replace every type variable in [typ_ml] with [Obj.t]: each parametric slot
   erases at the boundary since [V_native]'s [t] is [Obj.t]. *)
let rec erase_tvars (typ_ml : Ml.typ) : Ml.typ =
  match typ_ml with
  | Ml.VarT _ -> Ml.NameT "Obj.t"
  | Ml.AppT (id, typs) -> Ml.AppT (id, List.map erase_tvars typs)
  | Ml.TupleT typs -> Ml.TupleT (List.map erase_tvars typs)
  | Ml.FuncT (typ_l, typ_r) -> Ml.FuncT (erase_tvars typ_l, erase_tvars typ_r)
  | Ml.OpenRowT rows ->
      Ml.OpenRowT (List.map (fun (c, typs) -> (c, List.map erase_tvars typs)) rows)
  | (Ml.UnitT | Ml.BoolT | Ml.StringT | Ml.BigintT | Ml.NameT _) as t -> t

(* The scrutinee type of [id]'s arm: the named type applied to one [Obj.t] per
   type parameter ([set] -> [Obj.t set], [map] -> [(Obj.t, Obj.t) map]). *)
let scrut_typ (id : Sl.id) (tparams : Sl.tparam list) : Ml.typ =
  let id_ml = Names.var_of_id id in
  match tparams with
  | [] -> Ml.NameT id_ml
  | _ -> Ml.AppT (id_ml, List.map (fun _ -> Ml.NameT "Obj.t") tparams)

(* Resolve [id] to the underlying variant its [case_of_typed] arm dispatches on:
   its own tparams (for the scrutinee), and the variant's ctors + tparams (for
   payload shapes). Direct variants stay put; aliases ([map]/[valres]) expand to
   the underlying variant. Non-variant types yield [None] and get no arm. *)
let resolve_variant (ctx : Ctx.t) (id : Sl.id) :
    (Sl.tparam list * Sl.id * Sl.tparam list) option =
  match Ctx.find_typdef ctx id with
  | Typdef.Defined (tparams, { it = Il.VariantT _; _ }) ->
      Some (tparams, id, tparams)
  | Typdef.Defined (tparams, { it = Il.PlainT _; _ }) -> (
      (* Feed the tparams back as targs so [expand_typ]'s arity check passes;
         they substitute to themselves and erase to [Obj.t] downstream. *)
      let targs = List.map (fun tp -> Il.VarT (tp, []) $ tp.at) tparams in
      let typ_expanded =
        Expand.expand_typ (Ctx.find_typdef_opt ctx) (Il.VarT (id, targs) $ id.at)
      in
      match typ_expanded.it with
      | Il.VarT (id_under, _) -> (
          match Ctx.find_typdef ctx id_under with
          | Typdef.Defined (under_tparams, { it = Il.VariantT _; _ }) ->
              Some (tparams, id_under, under_tparams)
          | _ -> None)
      | _ -> None)
  | _ -> None

(* Named closure types whose (expanded) head is a variant, direct or aliased. *)
let variant_ids (ctx : Ctx.t) (typs : Sl.typ list) :
    (Sl.id * Sl.tparam list * Sl.id * Sl.tparam list) list =
  typs
  |> List.filter_map (fun (typ : Sl.typ) ->
         match typ.it with
         (* Applied forms ([set<..>]) appear for parametric variants; key on
            the head name and dedup, since every instantiation shares one arm. *)
         | Il.VarT (id, _) -> (
             match resolve_variant ctx id with
             | Some (tparams, id_under, under_tparams) ->
                 Some (id, tparams, id_under, under_tparams)
             | None -> None)
         | _ -> None)
  |> List.sort_uniq (fun (a, _, _, _) (b, _, _, _) -> compare a.it b.it)

(* [Obj.obj (List.nth args i) : <payload i>] with tparams erased to [Obj.t]. *)
let arg_of_nth (under_tparams : Sl.tparam list) (i : int) (typ : Sl.typ) :
    Ml.expr =
  let tparams = List.map it under_tparams in
  let typ_ml = erase_tvars (Type.compile_typ ~tparams typ) in
  Ml.AnnotE
    ( Ml.AppE
        ( Ml.LitE "Obj.obj",
          [
            Ml.AppE
              (Ml.LitE "List.nth", [ Ml.VarE "args"; Ml.LitE (string_of_int i) ]);
          ] ),
      typ_ml )

(* [match typ.it with Il.VarT (id, _) -> id.it | _ -> ""] *)
let typename_of_expr : Ml.expr =
  Ml.MatchE
    ( Ml.FieldE (Ml.VarE "typ", "it"),
      [
        ( Ml.VariantP (`Mono ("Il.VarT", [ Ml.VarP "id"; Ml.WildP ])),
          Ml.FieldE (Ml.VarE "id", "it") );
        (Ml.WildP, Ml.StrE "");
      ] )

let compile_make_case (ctx : Ctx.t)
    (variants : (Sl.id * Sl.tparam list * Sl.id * Sl.tparam list) list) :
    Ml.funcdef =
  let outer_arms =
    List.map
      (fun (id, _tparams, id_under, under_tparams) ->
        let ctors = Ctx.find_ctors_full ctx id_under in
        let inner_arms =
          List.map
            (fun (mixop, ctor_ml, payload_typs) ->
              let canon = Mixop.string_of_mixop mixop in
              let arg_exprs =
                List.mapi (arg_of_nth under_tparams) payload_typs
              in
              ( Ml.LitP (Printf.sprintf "%S" canon),
                Ml.AppE
                  (Ml.LitE "Obj.repr", [ Ml.VariantE (ctor_ml, arg_exprs) ]) ))
            ctors
        in
        let inner_wild =
          ( Ml.WildP,
            Ml.AppE
              ( Ml.LitE "failwith",
                [
                  Ml.BinopE
                    ( "^",
                      Ml.StrE ("make_case_typed: bad mixop for " ^ id.it ^ ": "),
                      Ml.VarE "mixop" );
                ] ) )
        in
        ( Ml.LitP (Printf.sprintf "%S" id.it),
          Ml.MatchE (Ml.VarE "mixop", inner_arms @ [ inner_wild ]) ))
      variants
  in
  let outer_wild =
    ( Ml.WildP,
      Ml.AppE
        ( Ml.LitE "failwith",
          [
            Ml.BinopE
              ("^", Ml.StrE "make_case_typed: unknown typ ", Ml.VarE "typ");
          ] ) )
  in
  ( "make_case_typed",
    [],
    [
      ("mixop", Some (Ml.NameT "Il.mixop"));
      ("args", Some (Ml.AppT ("list", [ Ml.NameT "Obj.t" ])));
      ("typ", Some (Ml.NameT "Il.typ"));
    ],
    Some (Ml.NameT "Obj.t"),
    Ml.LetE
      ( Ml.VarP "typ",
        typename_of_expr,
        Ml.LetE
          ( Ml.VarP "mixop",
            Ml.AppE (Ml.LitE "Mixop.string_of_mixop", [ Ml.VarE "mixop" ]),
            Ml.MatchE (Ml.VarE "typ", outer_arms @ [ outer_wild ]) ) ) )

let compile_case_of (ctx : Ctx.t) (pool : Constpool.t)
    (variants : (Sl.id * Sl.tparam list * Sl.id * Sl.tparam list) list) :
    Constpool.t * Ml.funcdef =
  let pool, outer_arms =
    List.fold_left_map
      (fun pool (id, tparams, id_under, _under_tparams) ->
        let ctors = Ctx.find_ctors_full ctx id_under in
        let pool, inner_arms =
          List.fold_left_map
            (fun pool (mixop, ctor_ml, payload_typs) ->
              let pvars =
                List.mapi (fun i _ -> "p" ^ string_of_int i) payload_typs
              in
              let pat =
                Ml.VariantP
                  (`Poly (ctor_ml, List.map (fun v -> Ml.VarP v) pvars))
              in
              let pool, mo_ref = Constpool.intern_mixop pool mixop in
              let repr_args =
                List.map
                  (fun v -> Ml.AppE (Ml.LitE "Obj.repr", [ Ml.VarE v ]))
                  pvars
              in
              ( pool,
                ( pat,
                  Ml.AppE
                    (Ml.LitE "Mixfix.fill", [ Ml.VarE mo_ref; Ml.ListE repr_args ])
                ) ))
            pool ctors
        in
        (* Force the ctor-tag check even on single-ctor types. *)
        let inner_wild =
          ( Ml.WildP,
            Ml.AppE
              ( Ml.LitE "failwith",
                [
                  Ml.StrE
                    ("case_of_typed: unexpected ctor for " ^ id.it);
                ] ) )
        in
        let scrut =
          Ml.AnnotE
            ( Ml.AppE (Ml.LitE "Obj.obj", [ Ml.VarE "x" ]),
              scrut_typ id tparams )
        in
        ( pool,
          ( Ml.LitP (Printf.sprintf "%S" id.it),
            Ml.MatchE (scrut, inner_arms @ [ inner_wild ]) ) ))
      pool variants
  in
  let outer_wild =
    ( Ml.WildP,
      Ml.AppE
        ( Ml.LitE "failwith",
          [
            Ml.BinopE ("^", Ml.StrE "case_of_typed: unknown typ ", Ml.VarE "typ");
          ] ) )
  in
  ( pool,
    ( "case_of_typed",
      [],
      [ ("x", Some (Ml.NameT "Obj.t")); ("typ", Some (Ml.NameT "Il.typ")) ],
      Some (Ml.AppT ("Mixfix.t", [ Ml.NameT "Obj.t" ])),
      Ml.LetE
        ( Ml.VarP "typ",
          typename_of_expr,
          Ml.MatchE (Ml.VarE "typ", outer_arms @ [ outer_wild ]) ) ) )

(* Both typed mixop bridges over the closure [typs]. *)
let compile_case_dispatch (ctx : Ctx.t) (pool : Constpool.t)
    (typs : Sl.typ list) : Constpool.t * Ml.funcdef list =
  let variants = variant_ids ctx typs in
  let funcdef_make_case = compile_make_case ctx variants in
  let pool, funcdef_case_of = compile_case_of ctx pool variants in
  (pool, [ funcdef_make_case; funcdef_case_of ])
