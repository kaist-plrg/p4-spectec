open Domain
open Lang
module Typdef = Runtime.Type.Typdef
open Util.Source

(* Typed mixop bridges: [make_case_typed]/[case_of_typed] convert between a
   value's native (typed [Obj.t]) representation and its [Domain.Mixfix.t]
   shape, threaded from the (hand-written, [SAFE]-generic) extern as a
   structured [Il.typ] — not a bare string (which was fragile: a stale
   typename only failed at runtime). The per-type arms still key on the
   type's name, so we extract [id.it] from the [VarT] head once at the top
   and let the existing string-keyed match stand:
     [match typ.it with Il.VarT (id, _) -> id.it | _ -> ""]. *)

let variant_ids (ctx : Ctx.t) (typs : Sl.typ list) : (Sl.id * Sl.typ) list =
  List.filter_map
    (fun typ ->
      match typ.it with
      | Il.VarT (id, []) -> (
          match Ctx.find_typdef ctx id with
          | Typdef.Defined ([], deftyp) -> (
              match deftyp.it with Il.VariantT _ -> Some (id, typ) | _ -> None)
          | _ -> None)
      | _ -> None)
    typs

(* [Obj.obj (List.nth args i)] *)
let obj_obj_nth (i : int) : Ml.expr =
  Ml.AppE
    ( Ml.LitE "Obj.obj",
      [
        Ml.AppE
          (Ml.LitE "List.nth", [ Ml.VarE "args"; Ml.LitE (string_of_int i) ]);
      ] )

let typename_of_expr : Ml.expr =
  Ml.MatchE
    ( Ml.FieldE (Ml.VarE "typ", "it"),
      [
        ( Ml.VariantP (`Mono ("Il.VarT", [ Ml.VarP "id"; Ml.WildP ])),
          Ml.FieldE (Ml.VarE "id", "it") );
        (Ml.WildP, Ml.StrE "");
      ] )

(* [set]/[pair]/[map]/[res] are parametric poly-variants ([`Set of 'k list],
   [`Pair of 'k * 'v], [map = pair set], [`OK_X of 'x | `FAIL]) absent from
   [variant_ids]: their decls carry tparams, so the normal arm-builder's
   per-payload [compile_typ] annotation would dangle a free type var. But
   the ctor is type-uniform — 'k/'v/'x erase to [Obj.t] — so ONE
   annotation-free arm per head covers every instantiation. The real ctor +
   mixop come from [Ctx] (keys then match the spec, hence [V_native]'s
   threaded mixop); [map]'s value is a [`Set] of pairs, so it reuses the
   "set" ctor. [res] is included for the same reason as the other three:
   [valres]/[valsres] (= [res<val>]/[res<val*>]) are concrete aliases of the
   parametric [res<X>], and nothing in the marshal closure unfolds through a
   parametric alias target, so they never got a [case_of_typed]/
   [make_case_typed] arm despite being exactly as type-uniform as
   [set]/[pair]/[map]. *)
let parametric_heads = [ "set"; "pair"; "map"; "res" ]

let parametric_ctors (ctx : Ctx.t) (head : string) =
  let src = if head = "map" then "set" else head in
  Ctx.find_ctors_full ctx (src $ no_region)

(* Not every spec declares every parametric head — P4's own "spec" tree has
   no [res]/[valres] concept (that's spec-meta's own extern-call-result
   wrapper). [make_case_typed]'s per-head arm is still safe with zero ctors
   (it falls through to its own [wild]/[failwith]), but [case_of_typed]'s
   per-head arm has no such fallback: a literal-string match on a head
   whose [inner_arms] is empty compiles to a zero-case [match], a syntax
   error, not a runtime one. Drop heads with no ctors in this spec before
   emitting either — they correctly fall through to [case_of_typed]'s own
   outer ["unknown typ"] instead. *)
let parametric_heads_present (ctx : Ctx.t) : string list =
  List.filter (fun head -> parametric_ctors ctx head <> []) parametric_heads

(* set/map -> [Obj.t set], pair -> [(Obj.t, Obj.t) pair], res -> [Obj.t res]. *)
let parametric_scrut_typ (head : string) : Ml.typ =
  match head with
  | "pair" -> Ml.AppT ("pair", [ Ml.NameT "Obj.t"; Ml.NameT "Obj.t" ])
  | "res" -> Ml.AppT ("res", [ Ml.NameT "Obj.t" ])
  | _ -> Ml.AppT ("set", [ Ml.NameT "Obj.t" ])

let make_parametric_arms (ctx : Ctx.t) : Ml.arm list =
  List.map
    (fun head ->
      let inner_arms =
        List.map
          (fun (mixop, ctor_ml, payload_typs) ->
            let canon = Mixop.string_of_mixop mixop in
            let args = List.mapi (fun i _ -> obj_obj_nth i) payload_typs in
            ( Ml.LitP (Printf.sprintf "%S" canon),
              Ml.AppE (Ml.LitE "Obj.repr", [ Ml.VariantE (ctor_ml, args) ]) ))
          (parametric_ctors ctx head)
      in
      let wild =
        ( Ml.WildP,
          Ml.AppE
            ( Ml.LitE "failwith",
              [
                Ml.BinopE
                  ( "^",
                    Ml.StrE ("make_case_typed: bad mixop for " ^ head ^ ": "),
                    Ml.VarE "mixop" );
              ] ) )
      in
      ( Ml.LitP (Printf.sprintf "%S" head),
        Ml.MatchE (Ml.VarE "mixop", inner_arms @ [ wild ]) ))
    (parametric_heads_present ctx)

let case_parametric_arms (ctx : Ctx.t) (pool : Constpool.t) :
    Constpool.t * Ml.arm list =
  List.fold_left_map
    (fun pool head ->
      let pool, inner_arms =
        List.fold_left_map
          (fun pool (mixop, ctor_ml, payload_typs) ->
            let pvars =
              List.mapi (fun i _ -> "p" ^ string_of_int i) payload_typs
            in
            let pat =
              Ml.VariantP (`Poly (ctor_ml, List.map (fun v -> Ml.VarP v) pvars))
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
          pool (parametric_ctors ctx head)
      in
      let scrut =
        Ml.AnnotE
          ( Ml.AppE (Ml.LitE "Obj.obj", [ Ml.VarE "x" ]),
            parametric_scrut_typ head )
      in
      (pool, (Ml.LitP (Printf.sprintf "%S" head), Ml.MatchE (scrut, inner_arms))))
    pool
    (parametric_heads_present ctx)

let compile_make_case (ctx : Ctx.t) (variants : (Sl.id * Sl.typ) list) :
    Ml.funcdef =
  let outer_arms =
    List.map
      (fun (id, _typ) ->
        let ctors = Ctx.find_ctors_full ctx id in
        let inner_arms =
          List.map
            (fun (mixop, ctor_ml, payload_typs) ->
              let canon = Mixop.string_of_mixop mixop in
              let arg_exprs =
                List.mapi
                  (fun i pt ->
                    Ml.AnnotE (obj_obj_nth i, Type.compile_typ ~tparams:[] pt))
                  payload_typs
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
            Ml.MatchE
              (Ml.VarE "typ", outer_arms @ make_parametric_arms ctx @ [ outer_wild ])
          ) ) )

let compile_case_of (ctx : Ctx.t) (pool : Constpool.t)
    (variants : (Sl.id * Sl.typ) list) : Constpool.t * Ml.funcdef =
  let pool, outer_arms =
    List.fold_left_map
      (fun pool (id, typ) ->
        let ctors = Ctx.find_ctors_full ctx id in
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
                    ( Ml.LitE "Mixfix.fill",
                      [ Ml.VarE mo_ref; Ml.ListE repr_args ] ) ) ))
            pool ctors
        in
        let scrut =
          Ml.AnnotE
            ( Ml.AppE (Ml.LitE "Obj.obj", [ Ml.VarE "x" ]),
              Type.compile_typ ~tparams:[] typ )
        in
        (pool, (Ml.LitP (Printf.sprintf "%S" id.it), Ml.MatchE (scrut, inner_arms))))
      pool variants
  in
  let pool, arms_parametric = case_parametric_arms ctx pool in
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
          Ml.MatchE (Ml.VarE "typ", outer_arms @ arms_parametric @ [ outer_wild ])
        ) ) )

let compile (ctx : Ctx.t) (pool : Constpool.t) (typs : Sl.typ list) :
    Constpool.t * Ml.funcdef list =
  let variants = variant_ids ctx typs in
  let funcdef_make_case = compile_make_case ctx variants in
  let pool, funcdef_case_of = compile_case_of ctx pool variants in
  (pool, [ funcdef_make_case; funcdef_case_of ])

(* [marshal_typed]/[unmarshal_typed]: the per-type [V_native] persist bridge.
   Dispatched by matching the value's spec type [Typ.t] directly — call sites
   pass the type they already hold (backend-sim's [Typs.*], the builtins'
   element-type targ), no string convention. Total over the marshal closure
   [typs]: every named ([VarT]) closure type gets a marshal and unmarshal
   arm. No curated entry-point list, so a new persist/builtin marshal target
   needs no codegen change. *)
let compile_marshal_dispatch (typs : Sl.typ list) : Ml.funcdef list =
  let keys =
    List.filter_map
      (fun (t : Sl.typ) ->
        match t.it with Il.VarT (id, _) -> Some (id.it, Naming.name t) | _ -> None)
      typs
    |> List.sort_uniq compare
  in
  (* Match [typ.it] against the named-type constructor for each closure type. *)
  let scrut = Ml.FieldE (Ml.VarE "typ", "it") in
  let var_pat key =
    Ml.LitP (Printf.sprintf "Il.VarT ({ it = %S; _ }, _)" key)
  in
  let marshal_arms =
    List.map
      (fun (key, iname) ->
        ( var_pat key,
          Ml.AppE
            (Ml.LitE ("marshal_" ^ iname), [ Ml.AppE (Ml.LitE "Obj.obj", [ Ml.VarE "x" ]) ]) ))
      keys
  in
  let unmarshal_arms =
    List.map
      (fun (key, iname) ->
        ( var_pat key,
          Ml.AppE
            ( Ml.LitE "Obj.repr",
              [ Ml.AppE (Ml.VarE ("unmarshal_" ^ iname), [ Ml.VarE "v" ]) ] ) ))
      keys
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
  [
    ( "marshal_typed",
      [],
      [ ("typ", Some (Ml.NameT "Typ.t")); ("x", Some (Ml.NameT "Obj.t")) ],
      Some (Ml.NameT "Value.t"),
      Ml.MatchE (scrut, marshal_arms @ [ wild "marshal_typed" ]) );
    ( "unmarshal_typed",
      [],
      [ ("typ", Some (Ml.NameT "Typ.t")); ("v", Some (Ml.NameT "Value.t")) ],
      Some (Ml.NameT "Obj.t"),
      Ml.MatchE (scrut, unmarshal_arms @ [ wild "unmarshal_typed" ]) );
  ]
