open Lang
module Typ = Runtime.Type
module Typdef = Typ.Typdef
open Util.Source

(* Generates the note-wrapping companions of each type:
     [hash_<name>]  reads a wrapped node's stored slot (structural otherwise)
     [mk_<name>]    wraps a raw body, computing the incremental vhash once
     [eq_<name>]    cheap note checks short-circuit before the deep compare
   A ground instantiation gets a monomorphic family; a parametric definition
   gets a polymorphic one taking a [typ]/[hash] dictionary per type parameter
   for [mk] and an [eq] dictionary for [eq], so construction inside a generic
   function has a [mk] to call. Children route through [Converter.resolve],
   which plugs a mono function for a ground child and the caller's dictionary
   for a bare type parameter *)

(* e.note.<field> *)

let note_field (expr_ml : Ml.expr) (field_ml : Ml.field) : Ml.expr =
  Ml.FieldE (Ml.FieldE (expr_ml, "note"), field_ml)

(* seed*31 + h0 ..*31 + hn *)

let hash_combine (seed : int) (exprs_hash_ml : Ml.expr list) : Ml.expr =
  List.fold_left
    (fun expr_acc_ml expr_ml ->
      Ml.BinopE ("+", Ml.BinopE ("*", expr_acc_ml, Ml.LitE "31"), expr_ml))
    (Ml.LitE (string_of_int seed))
    exprs_hash_ml

let and_all (exprs_ml : Ml.expr list) : Ml.expr =
  match exprs_ml with
  | [] -> Ml.BoolE true
  | expr_ml :: exprs_ml ->
      List.fold_left
        (fun expr_acc_ml expr_ml -> Ml.BinopE ("&&", expr_acc_ml, expr_ml))
        expr_ml exprs_ml

(* Child hash/eq: a mono function for a ground child, a dictionary for a bare
   type parameter, an inline closure for a composite *)

let hash_child (ctx : Ctx.t) (tparams : string list) (typ : Sl.typ)
    (expr_ml : Ml.expr) : Ml.expr =
  Ml.AppE ((Converter.resolve ctx tparams typ).hash, [ expr_ml ])

let eq_child (ctx : Ctx.t) (tparams : string list) (typ : Sl.typ)
    (expr_l_ml : Ml.expr) (expr_r_ml : Ml.expr) : Ml.expr =
  Ml.AppE ((Converter.resolve ctx tparams typ).eq, [ expr_l_ml; expr_r_ml ])

(* Body folds over the raw [x] body (mk) and comparisons over [l]/[r] (eq) *)

let hash_struct_body (ctx : Ctx.t) (tparams : string list)
    (typfields : Sl.typfield list) : Ml.expr =
  hash_combine (List.length typfields)
    (List.map
       (fun (atom, typ) ->
         hash_child ctx tparams typ (Ml.FieldE (Ml.VarE "x", Names.field atom)))
       typfields)

(* Seed the case hash by a type-independent ctor id ([Hashtbl.hash] of the ctor
   name), never its positional index: an included/coerced value keeps its hash
   across variant subtyping, so the vhash eq short-circuit stays sound *)

let hash_variant_body (ctx : Ctx.t) (tparams : string list)
    (ctors : (Domain.Mixop.t * Ml.ctor * Sl.typ list) list) : Ml.expr =
  let arms_ml =
    List.map
      (fun (_, ctor_ml, payload_typs) ->
        let ids_ml =
          List.mapi (fun i _ -> "p_" ^ string_of_int i) payload_typs
        in
        let pat_ml =
          Ml.VariantP
            (`Poly (ctor_ml, List.map (fun id_ml -> Ml.VarP id_ml) ids_ml))
        in
        let exprs_hash_ml =
          List.map2
            (fun typ id_ml -> hash_child ctx tparams typ (Ml.VarE id_ml))
            payload_typs ids_ml
        in
        (pat_ml, hash_combine (Hashtbl.hash ctor_ml) exprs_hash_ml))
      ctors
  in
  Ml.MatchE (Ml.VarE "x", arms_ml)

let eq_struct_body (ctx : Ctx.t) (tparams : string list)
    (typfields : Sl.typfield list) : Ml.expr =
  and_all
    (List.map
       (fun (atom, typ) ->
         let field_ml = Names.field atom in
         eq_child ctx tparams typ
           (Ml.FieldE (Ml.VarE "l", field_ml))
           (Ml.FieldE (Ml.VarE "r", field_ml)))
       typfields)

let eq_variant_body (ctx : Ctx.t) (tparams : string list)
    (ctors : (Domain.Mixop.t * Ml.ctor * Sl.typ list) list) : Ml.expr =
  let arms_ml =
    List.map
      (fun (_, ctor_ml, payload_typs) ->
        let ids_l_ml =
          List.mapi (fun i _ -> "l_" ^ string_of_int i) payload_typs
        in
        let ids_r_ml =
          List.mapi (fun i _ -> "r_" ^ string_of_int i) payload_typs
        in
        let pat_ml =
          Ml.TupleP
            [
              Ml.VariantP
                (`Poly (ctor_ml, List.map (fun id_ml -> Ml.VarP id_ml) ids_l_ml));
              Ml.VariantP
                (`Poly (ctor_ml, List.map (fun id_ml -> Ml.VarP id_ml) ids_r_ml));
            ]
        in
        let expr_cmp_ml =
          and_all
            (List.map2
               (fun typ (id_l_ml, id_r_ml) ->
                 eq_child ctx tparams typ (Ml.VarE id_l_ml) (Ml.VarE id_r_ml))
               payload_typs
               (List.combine ids_l_ml ids_r_ml))
        in
        (pat_ml, expr_cmp_ml))
      ctors
  in
  Ml.MatchE
    ( Ml.TupleE [ Ml.VarE "l"; Ml.VarE "r" ],
      arms_ml @ [ (Ml.WildP, Ml.BoolE false) ] )

(* Physical, vid, then vhash checks short-circuit before the deep [.it] compare *)

let eq_short_circuit (expr_cmp_ml : Ml.expr) : Ml.expr =
  Ml.BinopE
    ( "||",
      Ml.BinopE ("==", Ml.VarE "l", Ml.VarE "r"),
      Ml.BinopE
        ( "||",
          Ml.BinopE
            ("=", note_field (Ml.VarE "l") "vid", note_field (Ml.VarE "r") "vid"),
          Ml.BinopE
            ( "&&",
              Ml.BinopE
                ( "=",
                  note_field (Ml.VarE "l") "vhash",
                  note_field (Ml.VarE "r") "vhash" ),
              Ml.LetE
                ( Ml.VarP "l",
                  Ml.FieldE (Ml.VarE "l", "it"),
                  Ml.LetE
                    (Ml.VarP "r", Ml.FieldE (Ml.VarE "r", "it"), expr_cmp_ml) )
            ) ) )

(* [('x, ..) name], or [name] with no args *)

let typ_app_ml (name : Ml.id) (typs_arg_ml : Ml.typ list) : Ml.typ =
  match typs_arg_ml with [] -> Ml.NameT name | _ -> Ml.AppT (name, typs_arg_ml)

(* [mk]'s [typ__x : Typ.t] / [hash__x : 'x -> int] dictionary parameters *)

let params_mk_dict (tparams_ml : Ml.tparam list) : Ml.param list =
  List.concat_map
    (fun tparam_ml ->
      [
        (Naming.name_typ tparam_ml, Some (Ml.NameT "Typ.t"));
        ( Converter.name_hash tparam_ml,
          Some (Ml.FuncT (Ml.VarT tparam_ml, Ml.NameT "int")) );
      ])
    tparams_ml

(* [eq]'s [eq__x : 'x -> 'x -> bool] dictionary parameters *)

let params_eq_dict (tparams_ml : Ml.tparam list) : Ml.param list =
  List.map
    (fun tparam_ml ->
      ( Converter.name_eq tparam_ml,
        Some (Ml.FuncT (Ml.VarT tparam_ml, Ml.FuncT (Ml.VarT tparam_ml, Ml.BoolT)))
      ))
    tparams_ml

type wrapped_body =
  | BodyStruct of Sl.typfield list
  | BodyVariant of (Domain.Mixop.t * Ml.ctor * Sl.typ list) list
  | BodyList of Sl.typ
  | BodyOpt of Sl.typ
  | BodyTuple of Sl.typ list

(* mk fold / eq compare over a raw tuple body *)

let hash_tuple_body (ctx : Ctx.t) (tparams : string list) (typs : Sl.typ list) :
    Ml.expr =
  let ids_ml = List.mapi (fun i _ -> "x" ^ string_of_int i) typs in
  Ml.LetE
    ( Ml.TupleP (List.map (fun id_ml -> Ml.VarP id_ml) ids_ml),
      Ml.VarE "x",
      hash_combine (List.length typs)
        (List.map2
           (fun typ id_ml -> hash_child ctx tparams typ (Ml.VarE id_ml))
           typs ids_ml) )

let eq_tuple_body (ctx : Ctx.t) (tparams : string list) (typs : Sl.typ list) :
    Ml.expr =
  let ids_l_ml = List.mapi (fun i _ -> "l" ^ string_of_int i) typs in
  let ids_r_ml = List.mapi (fun i _ -> "r" ^ string_of_int i) typs in
  Ml.LetE
    ( Ml.TupleP (List.map (fun id_ml -> Ml.VarP id_ml) ids_l_ml),
      Ml.VarE "l",
      Ml.LetE
        ( Ml.TupleP (List.map (fun id_ml -> Ml.VarP id_ml) ids_r_ml),
          Ml.VarE "r",
          and_all
            (List.map2
               (fun typ (id_l_ml, id_r_ml) ->
                 eq_child ctx tparams typ (Ml.VarE id_l_ml) (Ml.VarE id_r_ml))
               typs
               (List.combine ids_l_ml ids_r_ml)) ) )

(* [hash_<name>]/[eq_<name>]/[mk_<name>] for a wrapped (struct/variant) type *)

let compile_wrapped_family (ctx : Ctx.t) ~(tparams : string list)
    ~(tparams_ml : Ml.tparam list) ~(name : Ml.id) ~(typ_wrapped_ml : Ml.typ)
    ~(typ_body_ml : Ml.typ) ~(typ_reify : Sl.typ) ~(body : wrapped_body) :
    Ml.funcdef list =
  let expr_hash_body_ml, expr_eq_body_ml =
    match body with
    | BodyStruct typfields ->
        (hash_struct_body ctx tparams typfields, eq_struct_body ctx tparams typfields)
    | BodyVariant ctors ->
        (hash_variant_body ctx tparams ctors, eq_variant_body ctx tparams ctors)
    | BodyList elem ->
        let conv = Converter.resolve ctx tparams elem in
        ( Ml.AppE (Ml.LitE "hash_list", [ conv.hash; Ml.VarE "x" ]),
          Ml.AppE (Ml.LitE "List.equal", [ conv.eq; Ml.VarE "l"; Ml.VarE "r" ]) )
    | BodyOpt elem ->
        let conv = Converter.resolve ctx tparams elem in
        ( Ml.AppE (Ml.LitE "hash_opt", [ conv.hash; Ml.VarE "x" ]),
          Ml.AppE
            (Ml.LitE "Option.equal", [ conv.eq; Ml.VarE "l"; Ml.VarE "r" ]) )
    | BodyTuple typs ->
        (hash_tuple_body ctx tparams typs, eq_tuple_body ctx tparams typs)
  in
  let funcdef_hash_ml =
    ( "hash_" ^ name,
      tparams_ml,
      [ ("x", Some typ_wrapped_ml) ],
      Some (Ml.NameT "int"),
      note_field (Ml.VarE "x") "vhash" )
  in
  let funcdef_eq_ml =
    ( "eq_" ^ name,
      tparams_ml,
      params_eq_dict tparams_ml
      @ [ ("l", Some typ_wrapped_ml); ("r", Some typ_wrapped_ml) ],
      Some Ml.BoolT,
      eq_short_circuit expr_eq_body_ml )
  in
  let funcdef_mk_ml =
    let expr_note_ml =
      Ml.RecordE
        [
          ("Il.vid", Ml.AppE (Ml.LitE "Value.fresh", []));
          ( "typ",
            Ml.LetE
              ( Ml.VarP "typ_",
                Dynamic_gen.make_typ_expr ~tparams typ_reify,
                Ml.FieldE (Ml.VarE "typ_", "it") ) );
          ("vhash", expr_hash_body_ml);
        ]
    in
    let expr_body_ml =
      Ml.BinopE
        ("$$", Ml.VarE "x", Ml.TupleE [ Ml.LitE "no_region"; expr_note_ml ])
    in
    ( "mk_" ^ name,
      tparams_ml,
      params_mk_dict tparams_ml @ [ ("x", Some typ_body_ml) ],
      Some typ_wrapped_ml,
      expr_body_ml )
  in
  [ funcdef_hash_ml; funcdef_eq_ml; funcdef_mk_ml ]

(* Structural (non-wrapped) types: only [hash_<name>]/[eq_<name>], no [mk] *)

let compile_hash_eq (name : Ml.id) (typ_ml : Ml.typ) (expr_hash_ml : Ml.expr)
    (expr_eq_ml : Ml.expr) : Ml.funcdef list =
  [
    ( "hash_" ^ name,
      [],
      [ ("x", Some typ_ml) ],
      Some (Ml.NameT "int"),
      expr_hash_ml );
    ( "eq_" ^ name,
      [],
      [ ("l", Some typ_ml); ("r", Some typ_ml) ],
      Some Ml.BoolT,
      expr_eq_ml );
  ]

let compile_prim (name : Ml.id) (typ_ml : Ml.typ) : Ml.funcdef list =
  compile_hash_eq name typ_ml
    (Ml.AppE (Ml.LitE "Hashtbl.hash", [ Ml.VarE "x" ]))
    (Ml.BinopE ("=", Ml.VarE "l", Ml.VarE "r"))

let compile_delegate (ctx : Ctx.t) (name : Ml.id) (typ_ml : Ml.typ)
    (typ_under : Sl.typ) : Ml.funcdef list =
  let conv = Converter.resolve ctx [] typ_under in
  compile_hash_eq name typ_ml
    (Ml.AppE (conv.hash, [ Ml.VarE "x" ]))
    (Ml.AppE (conv.eq, [ Ml.VarE "l"; Ml.VarE "r" ]))

let _compile_iter_unused (ctx : Ctx.t) (name : Ml.id) (typ_ml : Ml.typ)
    (typ_inner : Sl.typ) (iter : Sl.iter) : Ml.funcdef list =
  let conv = Converter.resolve ctx [] typ_inner in
  let id_hash_ml, id_eq_ml =
    match iter with
    | Il.List -> ("hash_list", "List.equal")
    | Il.Opt -> ("hash_opt", "Option.equal")
  in
  compile_hash_eq name typ_ml
    (Ml.AppE (Ml.LitE id_hash_ml, [ conv.hash; Ml.VarE "x" ]))
    (Ml.AppE (Ml.LitE id_eq_ml, [ conv.eq; Ml.VarE "l"; Ml.VarE "r" ]))

let compile_func (name : Ml.id) (typ_ml : Ml.typ) : Ml.funcdef list =
  compile_hash_eq name typ_ml (Ml.LitE "0")
    (Ml.BinopE ("==", Ml.VarE "l", Ml.VarE "r"))

(* Monomorphic family for a ground collected type *)

let compile (ctx : Ctx.t) (typ : Sl.typ) : Ml.funcdef list =
  let name = Naming.name typ in
  let typ_ml = Type.compile_typ ~tparams:[] typ in
  match typ.it with
  | Il.BoolT | Il.NumT _ | Il.TextT -> compile_prim name typ_ml
  | Il.TupleT typs ->
      let typ_body_ml = Ml.TupleT (List.map (Type.compile_typ ~tparams:[]) typs) in
      compile_wrapped_family ctx ~tparams:[] ~tparams_ml:[] ~name
        ~typ_wrapped_ml:typ_ml ~typ_body_ml ~typ_reify:typ ~body:(BodyTuple typs)
  | Il.IterT (typ_inner, Il.List) ->
      let typ_body_ml =
        Ml.AppT ("list", [ Type.compile_typ ~tparams:[] typ_inner ])
      in
      compile_wrapped_family ctx ~tparams:[] ~tparams_ml:[] ~name
        ~typ_wrapped_ml:typ_ml ~typ_body_ml ~typ_reify:typ
        ~body:(BodyList typ_inner)
  | Il.IterT (typ_inner, Il.Opt) ->
      let typ_body_ml =
        Ml.AppT ("option", [ Type.compile_typ ~tparams:[] typ_inner ])
      in
      compile_wrapped_family ctx ~tparams:[] ~tparams_ml:[] ~name
        ~typ_wrapped_ml:typ_ml ~typ_body_ml ~typ_reify:typ
        ~body:(BodyOpt typ_inner)
  | Il.FuncT _ -> compile_func name typ_ml
  | Il.VarT (id, targs) -> (
      match Ctx.find_typdef ctx id with
      | Typdef.Defined (tparams_def, deftyp) -> (
          let theta = Domain.Lib.TIdMap.of_lists tparams_def targs in
          let typ_body_ml =
            typ_app_ml (Names.body_of_id id)
              (List.map (Type.compile_typ ~tparams:[]) targs)
          in
          match deftyp.it with
          | Il.PlainT typ_alias ->
              compile_delegate ctx name typ_ml (Typ.Subst.subst_typ theta typ_alias)
          | Il.StructT typfields ->
              let typfields =
                List.map
                  (fun (atom, typ) -> (atom, Typ.Subst.subst_typ theta typ))
                  typfields
              in
              compile_wrapped_family ctx ~tparams:[] ~tparams_ml:[] ~name
                ~typ_wrapped_ml:typ_ml ~typ_body_ml ~typ_reify:typ
                ~body:(BodyStruct typfields)
          | Il.VariantT _ ->
              let ctors =
                Ctx.find_ctors_full ctx id
                |> List.map (fun (mixop, ctor_ml, typs) ->
                       (mixop, ctor_ml, Typ.Subst.subst_typs theta typs))
              in
              compile_wrapped_family ctx ~tparams:[] ~tparams_ml:[] ~name
                ~typ_wrapped_ml:typ_ml ~typ_body_ml ~typ_reify:typ
                ~body:(BodyVariant ctors))
      | Typdef.Extern -> compile_prim name typ_ml
      | Typdef.Param | Typdef.Defining _ -> [])

(* Polymorphic family for a parametric struct/variant definition *)

let compile_poly (ctx : Ctx.t) (id : Sl.id) (tparams_def : Sl.tparam list)
    (deftyp : Sl.deftyp) : Ml.funcdef list =
  let tparams = List.map it tparams_def in
  let tparams_ml = List.map Names.tvar tparams_def in
  let name = Names.var_of_id id in
  let typs_arg_ml = List.map (fun tparam_ml -> Ml.VarT tparam_ml) tparams_ml in
  let typ_wrapped_ml = typ_app_ml name typs_arg_ml in
  let typ_body_ml = typ_app_ml (Names.body_of_id id) typs_arg_ml in
  let typ_reify =
    Il.VarT (id, List.map (fun tparam -> Il.VarT (tparam, []) $ no_region) tparams_def)
    $ no_region
  in
  match deftyp.it with
  | Il.StructT typfields ->
      compile_wrapped_family ctx ~tparams ~tparams_ml ~name ~typ_wrapped_ml
        ~typ_body_ml ~typ_reify ~body:(BodyStruct typfields)
  | Il.VariantT _ ->
      let ctors = Ctx.find_ctors_full ctx id in
      compile_wrapped_family ctx ~tparams ~tparams_ml ~name ~typ_wrapped_ml
        ~typ_body_ml ~typ_reify ~body:(BodyVariant ctors)
  | Il.PlainT _ -> []

let compile_poly_all (ctx : Ctx.t) : Ml.funcdef list =
  Ctx.fold_typdefs
    (fun id typdef funcdefs_ml ->
      match typdef with
      | Typdef.Defined (tparams_def, deftyp) when tparams_def <> [] -> (
          match deftyp.it with
          | Il.StructT _ | Il.VariantT _ ->
              funcdefs_ml @ compile_poly ctx id tparams_def deftyp
          | _ -> funcdefs_ml)
      | _ -> funcdefs_ml)
    ctx []
