open Lang
module Typ = Runtime.Type
module Typdef = Typ.Typdef
open Util.Source

(* Generic-boundary marshal/unmarshal

   A polymorphic SpecTec function

     [def $rev<X>(xs : List<X>) : List<X>]

   compiles to an OCaml function taking an extra closure pair per type
   parameter — [marshal__x : 'x -> Value.t] / [unmarshal__x : Value.t -> 'x]
   — since its body still needs to cross the [Value.t] boundary (e.g. to call
   a builtin) for a type ([X]) it never knows concretely.

   Now say another generic function, [$g<X>], calls [$rev<X>], forwarding its
   own still-unknown [X]. Inside [$g]'s body there is no [marshal_X] to
   pass — that name only exists for a *ground* type, generated once [$g]
   itself gets called with a concrete [X]. What [$g] has instead is its own
   closure parameter [marshal__x], received the same way. [resolve] builds
   the converter [$g] needs to pass down, by walking the type's shape and
   plugging in whatever it has on hand at each layer:

     [(resolve ctx tparams (List<X>)).marshal]
     -->
     [fun x__ -> Value.Make.list (Typ.Make.var ("X" $ no_region) []) (List.map marshal__x x__)]

   (the "list" layer is handled the same way it would be for [List<int>];
   the "X" layer just plugs in the closure already in scope, [marshal__x]) *)

(* A boundary-call converter: [marshal]/[unmarshal] cross the [Value.t] edge,
   [hash]/[eq] give a value's structural hash/equality without leaving native
   form (a wrapped node's [hash] is an O(1) slot read) *)

type t = {
  marshal : Ml.expr;
  unmarshal : Ml.expr;
  hash : Ml.expr;
  eq : Ml.expr;
}

(* Naming *)

let name_marshal (tvar : string) = "marshal__" ^ tvar
let name_unmarshal (tvar : string) = "unmarshal__" ^ tvar
let name_hash (tvar : string) = "hash__" ^ tvar
let name_eq (tvar : string) = "eq__" ^ tvar

(* A wrapped node's hash is its stored slot; its structural eq is the poly
   [eq_<base>] applied to the element eqs *)

(* [.Il.vhash] is qualified because this closure's [x__] is unannotated, so the
   field cannot be disambiguated by type *)

let expr_slot_hash : Ml.expr =
  Ml.FunE
    ([ Ml.VarP "x__" ], Ml.FieldE (Ml.FieldE (Ml.VarE "x__", "note"), "Il.vhash"))

(* Wrap a raw composite body [expr_body_ml] into a note-carrying value, stamping
   a fresh vid, the reified typ, and the incremental vhash [expr_hash_ml] (which
   folds over [body__]) *)

let wrap_note (tparams : string list) (typ : Sl.typ) (expr_body_ml : Ml.expr)
    (expr_hash_ml : Ml.expr) : Ml.expr =
  Ml.LetE
    ( Ml.VarP "body__",
      expr_body_ml,
      Ml.BinopE
        ( "$$",
          Ml.VarE "body__",
          Ml.TupleE
            [
              Ml.LitE "no_region";
              Ml.RecordE
                [
                  ("Il.vid", Ml.AppE (Ml.LitE "Value.fresh", []));
                  ( "typ",
                    Ml.LetE
                      ( Ml.VarP "typ__w",
                        Dynamic_gen.make_typ_expr ~tparams typ,
                        Ml.FieldE (Ml.VarE "typ__w", "it") ) );
                  ("vhash", expr_hash_ml);
                ];
            ] ) )

(* == / vid / vhash short-circuit, then the structural comparison [expr_cmp_ml]
   over the two bodies [l__.it]/[r__.it] *)

let eq_composite_ladder (expr_cmp_ml : Ml.expr) : Ml.expr =
  let note_of id_ml field_ml =
    Ml.FieldE (Ml.FieldE (Ml.VarE id_ml, "note"), field_ml)
  in
  Ml.FunE
    ( [ Ml.VarP "l__"; Ml.VarP "r__" ],
      Ml.BinopE
        ( "||",
          Ml.BinopE ("==", Ml.VarE "l__", Ml.VarE "r__"),
          Ml.BinopE
            ( "||",
              Ml.BinopE ("=", note_of "l__" "Il.vid", note_of "r__" "Il.vid"),
              Ml.BinopE
                ( "&&",
                  Ml.BinopE
                    ("=", note_of "l__" "Il.vhash", note_of "r__" "Il.vhash"),
                  expr_cmp_ml ) ) ) )

(* Application *)

let apply_converter (tag : string) (expr_resolve_ml : Ml.expr)
    (expr_arg_ml : Ml.expr) : Ml.expr =
  let id_converter_ml = "converter__" ^ tag in
  let pat_converter_ml = Ml.VarP id_converter_ml in
  let expr_apply_ml = Ml.AppE (Ml.VarE id_converter_ml, [ expr_arg_ml ]) in
  Ml.LetE (pat_converter_ml, expr_resolve_ml, expr_apply_ml)

(* Struct/variant field and case construction *)

let compile_field_atom (s : string) : Ml.expr =
  Common.make_phrase (Dynamic_gen.make_atom_string (Domain.Atom.Atom s))

let compile_field_access (s : string) (expr_fields_ml : Ml.expr) : Ml.expr =
  let expr_pred_ml =
    Ml.LitE
      (Printf.sprintf "(fun ({ it; _ }, _) -> it = Atom.Atom \"%s\")"
         (String.escaped s))
  in
  Ml.AppE
    ( Ml.LitE "snd",
      [ Ml.AppE (Ml.LitE "List.find", [ expr_pred_ml; expr_fields_ml ]) ] )

let compile_case_value (expr_mixop_ml : Ml.expr) (expr_payload_ml : Ml.expr)
    (expr_typ_ml : Ml.expr) : Ml.expr =
  Ml.AppE
    ( Ml.LitE "Value.Make.case",
      [
        expr_typ_ml;
        Ml.AppE (Ml.LitE "Mixfix.fill", [ expr_mixop_ml; expr_payload_ml ]);
      ] )

(* Builds a [t] for [typ]; [visiting] guards typedef cycles *)

let rec resolve ?(visiting : string list = []) (ctx : Ctx.t)
    (tparams : string list) (typ : Sl.typ) : t =
  match typ.it with
  | Il.VarT (id, []) when List.mem id.it tparams -> resolve_var_typ_tparam id
  | Il.VarT (id, targs) when Type.is_generic tparams typ ->
      resolve_var_typ ~visiting ctx tparams typ id targs
  | Il.VarT (id, targs) ->
      resolve_var_typ_ground ~visiting ctx tparams typ id targs
  (* composites resolve to closures over the element converters even when
     ground, so a composite (e.g. [infer?]) never needs a mono function that
     the marshal-driven [Collect] may not have produced *)
  | Il.TupleT typs -> resolve_tuple_typ ~visiting ctx tparams typ typs
  | Il.IterT (t, Il.Opt) -> resolve_opt_typ ~visiting ctx tparams typ t
  | Il.IterT (t, Il.List) -> resolve_list_typ ~visiting ctx tparams typ t
  | _ -> resolve_ground typ

(* Ground type *)

and resolve_ground (typ : Sl.typ) : t =
  let name = Naming.name typ in
  {
    marshal = Ml.VarE ("marshal_" ^ name);
    unmarshal = Ml.VarE ("unmarshal_" ^ name);
    hash = Ml.VarE ("hash_" ^ name);
    eq = Ml.VarE ("eq_" ^ name);
  }

(* Bare type parameter

   - the converter is just the caller-supplied dictionary entry *)

and resolve_var_typ_tparam (id : Sl.id) : t =
  {
    marshal = Ml.VarE (name_marshal (Names.tvar id));
    unmarshal = Ml.VarE (name_unmarshal (Names.tvar id));
    hash = Ml.VarE (name_hash (Names.tvar id));
    eq = Ml.VarE (name_eq (Names.tvar id));
  }

(* Variable type *)

and resolve_var_typ ~(visiting : string list) (ctx : Ctx.t)
    (tparams : string list) (typ : Sl.typ) (id : Sl.id) (targs : Sl.targ list) :
    t =
  if List.mem id.it visiting then
    failwith
      (Printf.sprintf
         "resolve: %s: recursive generic typedef at a boundary call is not \
          supported"
         id.it)
  else resolve_typdef ~visiting:(id.it :: visiting) ctx tparams typ id targs

(* Ground variable type: mono marshal/unmarshal, but hash is the poly base's
   slot read and eq the poly base applied to the arg eqs, so a parametric
   instantiation never needs a mono hash/eq that may not have been collected *)

and resolve_var_typ_ground ~(visiting : string list) (ctx : Ctx.t)
    (tparams : string list) (typ : Sl.typ) (id : Sl.id) (targs : Sl.targ list) :
    t =
  match Ctx.find_typdef ctx id with
  | Typdef.Defined (tparams_def, deftyp) -> (
      let theta = Domain.Lib.TIdMap.of_lists tparams_def targs in
      match deftyp.it with
      | Il.PlainT typ_alias ->
          resolve ~visiting ctx tparams (Typ.Subst.subst_typ theta typ_alias)
      | Il.StructT _ | Il.VariantT _ ->
          let name = Naming.name typ in
          {
            marshal = Ml.VarE ("marshal_" ^ name);
            unmarshal = Ml.VarE ("unmarshal_" ^ name);
            hash = Ml.VarE ("hash_" ^ Names.var_of_id id);
            eq = resolve_wrapped_eq ~visiting ctx tparams typ;
          })
  | _ -> resolve_ground typ

(* Tuple type *)

and resolve_tuple_typ ~(visiting : string list) (ctx : Ctx.t)
    (tparams : string list) (typ : Sl.typ) (typs : Sl.typ list) : t =
  let convs = List.map (resolve ~visiting ctx tparams) typs in
  let n = List.length typs in
  let vars_m = List.mapi (fun i _ -> "x" ^ string_of_int i) typs in
  let marshal_calls_ml =
    List.map2
      (fun conv var -> apply_converter "resolved_" conv.marshal (Ml.VarE var))
      convs vars_m
  in
  let vars_u = List.init n (fun i -> "v" ^ string_of_int i) in
  let unmarshal_calls_ml =
    List.map2
      (fun conv var -> apply_converter "resolved_" conv.unmarshal (Ml.VarE var))
      convs vars_u
  in
  (* the body is the native tuple; marshal reads it from [x__.it] *)
  let expr_marshal_ml =
    let pat_vars_ml = Ml.TupleP (List.map (fun var -> Ml.VarP var) vars_m) in
    let expr_tuple_ml =
      Ml.AppE
        ( Ml.LitE "Value.Make.tuple",
          [ Dynamic_gen.make_typ_expr ~tparams typ; Ml.ListE marshal_calls_ml ] )
    in
    Ml.FunE
      ( [ Ml.VarP "x__" ],
        Ml.LetE (pat_vars_ml, Ml.FieldE (Ml.VarE "x__", "it"), expr_tuple_ml) )
  in
  (* the tuple hash folds [conv.hash] over the freshly-built body components *)
  let vars_h = List.mapi (fun i _ -> "h" ^ string_of_int i) typs in
  let expr_hash_body_ml =
    let exprs_hash_ml =
      List.map2 (fun conv var -> Ml.AppE (conv.hash, [ Ml.VarE var ])) convs vars_h
    in
    let expr_combine_ml =
      List.fold_left
        (fun acc_ml expr_ml ->
          Ml.BinopE ("+", Ml.BinopE ("*", acc_ml, Ml.LitE "31"), expr_ml))
        (Ml.LitE (string_of_int n))
        exprs_hash_ml
    in
    Ml.LetE
      ( Ml.TupleP (List.map (fun var -> Ml.VarP var) vars_h),
        Ml.VarE "body__",
        expr_combine_ml )
  in
  let expr_unmarshal_ml =
    let expr_get_ml = Ml.AppE (Ml.LitE "Value.Get.tuple", [ Ml.VarE "v__" ]) in
    let pat_vars_ml = Ml.ListP (List.map (fun var -> Ml.VarP var) vars_u) in
    let arm_ok_ml = (pat_vars_ml, Ml.TupleE unmarshal_calls_ml) in
    let arm_wild_ml = (Ml.WildP, Common.raise_unmatch "resolve: tuple") in
    let expr_body_ml = Ml.MatchE (expr_get_ml, [ arm_ok_ml; arm_wild_ml ]) in
    Ml.FunE
      ( [ Ml.VarP "v__" ],
        wrap_note tparams typ expr_body_ml expr_hash_body_ml )
  in
  let expr_eq_ml =
    let vars_l = List.mapi (fun i _ -> "l" ^ string_of_int i) typs in
    let vars_r = List.mapi (fun i _ -> "r" ^ string_of_int i) typs in
    let pat_l_ml = Ml.TupleP (List.map (fun var -> Ml.VarP var) vars_l) in
    let pat_r_ml = Ml.TupleP (List.map (fun var -> Ml.VarP var) vars_r) in
    let exprs_eq_ml =
      List.map2
        (fun conv (var_l, var_r) ->
          Ml.AppE (conv.eq, [ Ml.VarE var_l; Ml.VarE var_r ]))
        convs
        (List.combine vars_l vars_r)
    in
    let expr_and_ml =
      match exprs_eq_ml with
      | [] -> Ml.BoolE true
      | expr_ml :: exprs_ml ->
          List.fold_left
            (fun acc_ml expr_ml -> Ml.BinopE ("&&", acc_ml, expr_ml))
            expr_ml exprs_ml
    in
    eq_composite_ladder
      (Ml.LetE
         ( pat_l_ml,
           Ml.FieldE (Ml.VarE "l__", "it"),
           Ml.LetE (pat_r_ml, Ml.FieldE (Ml.VarE "r__", "it"), expr_and_ml) ))
  in
  {
    marshal = expr_marshal_ml;
    unmarshal = expr_unmarshal_ml;
    hash = expr_slot_hash;
    eq = expr_eq_ml;
  }

(* Option type *)

and resolve_opt_typ ~(visiting : string list) (ctx : Ctx.t)
    (tparams : string list) (typ : Sl.typ) (t : Sl.typ) : t =
  let conv = resolve ~visiting ctx tparams t in
  let expr_marshal_ml =
    Ml.FunE
      ( [ Ml.VarP "x__" ],
        Ml.AppE
          ( Ml.LitE "Value.Make.opt",
            [
              Dynamic_gen.make_typ_expr ~tparams typ;
              Ml.AppE
                ( Ml.LitE "Option.map",
                  [ conv.marshal; Ml.FieldE (Ml.VarE "x__", "it") ] );
            ] ) )
  in
  let expr_unmarshal_ml =
    Ml.FunE
      ( [ Ml.VarP "v__" ],
        wrap_note tparams typ
          (Ml.AppE
             ( Ml.LitE "Option.map",
               [
                 conv.unmarshal;
                 Ml.AppE (Ml.LitE "Value.Get.opt", [ Ml.VarE "v__" ]);
               ] ))
          (Ml.AppE (Ml.LitE "hash_opt", [ conv.hash; Ml.VarE "body__" ])) )
  in
  let expr_eq_ml =
    eq_composite_ladder
      (Ml.AppE
         ( Ml.LitE "Option.equal",
           [
             conv.eq;
             Ml.FieldE (Ml.VarE "l__", "it");
             Ml.FieldE (Ml.VarE "r__", "it");
           ] ))
  in
  {
    marshal = expr_marshal_ml;
    unmarshal = expr_unmarshal_ml;
    hash = expr_slot_hash;
    eq = expr_eq_ml;
  }

(* List type *)

and resolve_list_typ ~(visiting : string list) (ctx : Ctx.t)
    (tparams : string list) (typ : Sl.typ) (t : Sl.typ) : t =
  let conv = resolve ~visiting ctx tparams t in
  let expr_marshal_ml =
    Ml.FunE
      ( [ Ml.VarP "x__" ],
        Ml.AppE
          ( Ml.LitE "Value.Make.list",
            [
              Dynamic_gen.make_typ_expr ~tparams typ;
              Ml.AppE
                ( Ml.LitE "List.map",
                  [ conv.marshal; Ml.FieldE (Ml.VarE "x__", "it") ] );
            ] ) )
  in
  let expr_unmarshal_ml =
    Ml.FunE
      ( [ Ml.VarP "v__" ],
        wrap_note tparams typ
          (Ml.AppE
             ( Ml.LitE "List.map",
               [
                 conv.unmarshal;
                 Ml.AppE (Ml.LitE "Value.Get.list", [ Ml.VarE "v__" ]);
               ] ))
          (Ml.AppE (Ml.LitE "hash_list", [ conv.hash; Ml.VarE "body__" ])) )
  in
  let expr_eq_ml =
    eq_composite_ladder
      (Ml.AppE
         ( Ml.LitE "List.equal",
           [
             conv.eq;
             Ml.FieldE (Ml.VarE "l__", "it");
             Ml.FieldE (Ml.VarE "r__", "it");
           ] ))
  in
  {
    marshal = expr_marshal_ml;
    unmarshal = expr_unmarshal_ml;
    hash = expr_slot_hash;
    eq = expr_eq_ml;
  }

(* Error *)

and resolve_unsupported (typ : Sl.typ) : t =
  failwith
    (Printf.sprintf
       "resolve: %s: type parameter used inside an unsupported container at a \
        boundary call"
       (Sl.Print.string_of_typ typ))

(* Builds a [t] for a type definition *)

and resolve_typdef ~(visiting : string list) (ctx : Ctx.t)
    (tparams : string list) (typ : Sl.typ) (id : Sl.id) (targs : Sl.targ list) :
    t =
  match Ctx.find_typdef ctx id with
  | Typdef.Defined (tparams_def, deftyp) -> (
      let theta = Domain.Lib.TIdMap.of_lists tparams_def targs in
      match deftyp.it with
      | Il.PlainT typ_alias ->
          let typ_alias = Typ.Subst.subst_typ theta typ_alias in
          resolve ~visiting ctx tparams typ_alias
      | Il.StructT typfields ->
          let typfields =
            List.map
              (fun (atom, typ) ->
                let typ = Typ.Subst.subst_typ theta typ in
                (atom, typ))
              typfields
          in
          resolve_struct_typdef ~visiting ctx tparams typ typfields
      | Il.VariantT _ ->
          let ctors = Ctx.find_ctors_full ctx id in
          let ctors =
            List.map
              (fun (mixop, ctor_ml, typs) ->
                let typs = Typ.Subst.subst_typs theta typs in
                (mixop, ctor_ml, typs))
              ctors
          in
          resolve_variant_typdef ~visiting ctx tparams typ ctors)
  | _ ->
      failwith
        (Printf.sprintf "resolve: %s: not a plain/struct/variant typedef" id.it)

(* Wrap a raw body through the poly [mk_<base>] with a typ/hash dictionary per
   type argument, resolved in the ambient generic context *)

and mk_wrap ~(visiting : string list) (ctx : Ctx.t) (tparams : string list)
    (typ : Sl.typ) (expr_body_ml : Ml.expr) : Ml.expr =
  match typ.it with
  | Il.VarT (id, targs) ->
      let exprs_dict_ml =
        List.concat_map
          (fun targ ->
            [
              Dynamic_gen.make_typ_expr ~tparams targ;
              (resolve ~visiting ctx tparams targ).hash;
            ])
          targs
      in
      Ml.AppE (Ml.VarE ("mk_" ^ Names.var_of_id id), exprs_dict_ml @ [ expr_body_ml ])
  | _ -> expr_body_ml

(* A wrapped typedef's eq is the poly [eq_<base>] applied to its element eqs *)

and resolve_wrapped_eq ~(visiting : string list) (ctx : Ctx.t)
    (tparams : string list) (typ : Sl.typ) : Ml.expr =
  match typ.it with
  | Il.VarT (id, targs) -> (
      let exprs_eq_targ_ml =
        List.map (fun targ -> (resolve ~visiting ctx tparams targ).eq) targs
      in
      match exprs_eq_targ_ml with
      | [] -> Ml.VarE ("eq_" ^ Names.var_of_id id)
      | _ -> Ml.AppE (Ml.VarE ("eq_" ^ Names.var_of_id id), exprs_eq_targ_ml))
  | _ -> Ml.LitE "(=)"

and resolve_struct_typdef ~(visiting : string list) (ctx : Ctx.t)
    (tparams : string list) (typ : Sl.typ) (typfields : Sl.typfield list) : t =
  let field_convs =
    List.map
      (fun (atom, t) -> (atom, resolve ~visiting ctx tparams t))
      typfields
  in
  let x_ann = Ml.AnnotE (Ml.VarE "x__", Type.compile_typ ~tparams typ) in
  let expr_marshal_ml =
    let field_exprs_ml =
      List.map
        (fun (atom, conv) ->
          let atom_str = Names.Ctor.atom atom in
          let expr_atom_ml = compile_field_atom atom_str in
          let expr_field_ml =
            Ml.FieldE (Ml.FieldE (x_ann, "it"), Names.field atom)
          in
          let expr_conv_ml =
            apply_converter "resolved_" conv.marshal expr_field_ml
          in
          Ml.TupleE [ expr_atom_ml; expr_conv_ml ])
        field_convs
    in
    let expr_typ_ml = Dynamic_gen.make_typ_expr ~tparams typ in
    let expr_str_ml =
      Ml.AppE
        (Ml.LitE "Value.Make.str", [ expr_typ_ml; Ml.ListE field_exprs_ml ])
    in
    Ml.FunE ([ Ml.VarP "x__" ], expr_str_ml)
  in
  let expr_unmarshal_ml =
    let field_bindings_ml =
      List.map
        (fun (atom, conv) ->
          let atom_str = Names.Ctor.atom atom in
          let expr_get_field_ml =
            compile_field_access atom_str (Ml.VarE "fields__")
          in
          let expr_conv_ml =
            apply_converter "resolved_" conv.unmarshal expr_get_field_ml
          in
          (Names.field atom, expr_conv_ml))
        field_convs
    in
    let pat_fields_ml = Ml.VarP "fields__" in
    let expr_str_ml = Ml.AppE (Ml.LitE "Value.Get.str", [ Ml.VarE "v__" ]) in
    let expr_record_ml =
      mk_wrap ~visiting ctx tparams typ (Ml.RecordE field_bindings_ml)
    in
    let expr_let_ml = Ml.LetE (pat_fields_ml, expr_str_ml, expr_record_ml) in
    Ml.FunE ([ Ml.VarP "v__" ], expr_let_ml)
  in
  {
    marshal = expr_marshal_ml;
    unmarshal = expr_unmarshal_ml;
    hash = expr_slot_hash;
    eq = resolve_wrapped_eq ~visiting ctx tparams typ;
  }

and resolve_variant_typdef ~(visiting : string list) (ctx : Ctx.t)
    (tparams : string list) (typ : Sl.typ)
    (ctors : (Domain.Mixop.t * Ml.ctor * Sl.typ list) list) : t =
  let per_ctor =
    List.map
      (fun (mixop, ctor_ml, payload_typs) ->
        let convs = List.map (resolve ~visiting ctx tparams) payload_typs in
        (mixop, ctor_ml, convs))
      ctors
  in
  let name = Naming.name typ in
  let expr_marshal_ml =
    let arms_ml =
      List.map
        (fun (mixop, ctor_ml, convs) ->
          let pvars = List.mapi (fun i _ -> "p_" ^ string_of_int i) convs in
          let pat_ml =
            Ml.VariantP
              (`Poly (ctor_ml, List.map (fun var -> Ml.VarP var) pvars))
          in
          let marshal_calls_ml =
            List.map2
              (fun conv pvar ->
                apply_converter "resolved_" conv.marshal (Ml.VarE pvar))
              convs pvars
          in
          let expr_mixop_ml = Dynamic_gen.make_mixop_expr mixop in
          let expr_typ_ml = Dynamic_gen.make_typ_expr ~tparams typ in
          let expr_case_ml =
            compile_case_value expr_mixop_ml (Ml.ListE marshal_calls_ml)
              expr_typ_ml
          in
          (pat_ml, expr_case_ml))
        per_ctor
    in
    let expr_match_ml = Ml.MatchE (Ml.FieldE (Ml.VarE "x__", "it"), arms_ml) in
    Ml.FunE ([ Ml.VarP "x__" ], expr_match_ml)
  in
  let expr_unmarshal_ml =
    let arms_ctor_ml =
      List.map
        (fun (mixop, ctor_ml, convs) ->
          let pat_str, ids_arg_ml = Dynamic_gen.make_mixop_pat_string mixop in
          let exprs_payload_ml =
            List.map2
              (fun conv id_arg_ml ->
                apply_converter "resolved_" conv.unmarshal (Ml.VarE id_arg_ml))
              convs ids_arg_ml
          in
          (Ml.LitP pat_str, Ml.VariantE (ctor_ml, exprs_payload_ml)))
        per_ctor
    in
    let arm_unknown_ml =
      let expr_raise_ml =
        Common.raise_unmatch (Printf.sprintf "unmarshal_%s: unknown case" name)
      in
      (Ml.WildP, expr_raise_ml)
    in
    let expr_v_ann_ml = Ml.AnnotE (Ml.VarE "v__", Ml.NameT "Value.t") in
    let expr_it_ml = Ml.FieldE (expr_v_ann_ml, "it") in
    let pat_case_ml = Ml.VariantP (`Mono ("CaseV", [ Ml.VarP "vc_" ])) in
    let expr_match_ctor_ml =
      Ml.MatchE (Ml.VarE "vc_", arms_ctor_ml @ [ arm_unknown_ml ])
    in
    let arm_case_ml = (pat_case_ml, expr_match_ctor_ml) in
    let arm_wild_ml = (Ml.WildP, Common.raise_unmatch ("unmarshal_" ^ name)) in
    let expr_match_ml = Ml.MatchE (expr_it_ml, [ arm_case_ml; arm_wild_ml ]) in
    Ml.FunE ([ Ml.VarP "v__" ], mk_wrap ~visiting ctx tparams typ expr_match_ml)
  in
  {
    marshal = expr_marshal_ml;
    unmarshal = expr_unmarshal_ml;
    hash = expr_slot_hash;
    eq = resolve_wrapped_eq ~visiting ctx tparams typ;
  }

(* Table for finding converters *)

let compile_converter_table (typs : Sl.typ list) : Ml.toplevel =
  let entries_ml =
    List.map
      (fun typ ->
        let name = Naming.name typ in
        let expr_marshal_ml =
          Ml.AppE (Ml.LitE "Obj.repr", [ Ml.VarE ("marshal_" ^ name) ])
        in
        let expr_unmarshal_ml =
          Ml.AppE (Ml.LitE "Obj.repr", [ Ml.VarE ("unmarshal_" ^ name) ])
        in
        let expr_hash_ml =
          Ml.AppE (Ml.LitE "Obj.repr", [ Ml.VarE ("hash_" ^ name) ])
        in
        let expr_eq_ml =
          Ml.AppE (Ml.LitE "Obj.repr", [ Ml.VarE ("eq_" ^ name) ])
        in
        Ml.TupleE
          [
            Ml.StrE name;
            Ml.TupleE
              [ expr_marshal_ml; expr_unmarshal_ml; expr_hash_ml; expr_eq_ml ];
          ])
      typs
  in
  let expr_seq_ml = Ml.AppE (Ml.LitE "List.to_seq", [ Ml.ListE entries_ml ]) in
  let expr_table_ml = Ml.AppE (Ml.LitE "Hashtbl.of_seq", [ expr_seq_ml ]) in
  Ml.Let ("interface_registry_", expr_table_ml)
