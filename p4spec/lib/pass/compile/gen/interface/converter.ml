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

(* A boundary-call converter *)

type t = { marshal : Ml.expr; unmarshal : Ml.expr }

(* Naming *)

let name_marshal (tvar : string) = "marshal__" ^ tvar
let name_unmarshal (tvar : string) = "unmarshal__" ^ tvar

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
  | Il.TupleT typs when Type.is_generic tparams typ ->
      resolve_tuple_typ ~visiting ctx tparams typ typs
  | Il.IterT (t, Il.Opt) when Type.is_generic tparams t ->
      resolve_opt_typ ~visiting ctx tparams typ t
  | Il.IterT (t, Il.List) when Type.is_generic tparams t ->
      resolve_list_typ ~visiting ctx tparams typ t
  | _ when Type.is_generic tparams typ -> resolve_unsupported typ
  | _ -> resolve_ground typ

(* Ground type *)

and resolve_ground (typ : Sl.typ) : t =
  let name = Naming.name typ in
  let expr_marshal_ml = Ml.VarE ("marshal_" ^ name) in
  let expr_unmarshal_ml = Ml.VarE ("unmarshal_" ^ name) in
  { marshal = expr_marshal_ml; unmarshal = expr_unmarshal_ml }

(* Bare type parameter

   - the converter is just the caller-supplied dictionary entry *)

and resolve_var_typ_tparam (id : Sl.id) : t =
  let expr_marshal_ml = Ml.VarE (name_marshal (Names.tvar id)) in
  let expr_unmarshal_ml = Ml.VarE (name_unmarshal (Names.tvar id)) in
  { marshal = expr_marshal_ml; unmarshal = expr_unmarshal_ml }

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

(* Tuple type *)

and resolve_tuple_typ ~(visiting : string list) (ctx : Ctx.t)
    (tparams : string list) (typ : Sl.typ) (typs : Sl.typ list) : t =
  let convs = List.map (resolve ~visiting ctx tparams) typs in
  let vars_m = List.mapi (fun i _ -> "x" ^ string_of_int i) typs in
  let marshal_calls_ml =
    List.map2
      (fun conv var -> apply_converter "resolved_" conv.marshal (Ml.VarE var))
      convs vars_m
  in
  let n = List.length typs in
  let vars_u = List.init n (fun i -> "v" ^ string_of_int i) in
  let unmarshal_calls_ml =
    List.map2
      (fun conv var -> apply_converter "resolved_" conv.unmarshal (Ml.VarE var))
      convs vars_u
  in
  let expr_marshal_ml =
    let pat_vars_ml = Ml.TupleP (List.map (fun var -> Ml.VarP var) vars_m) in
    let expr_tuple_ml =
      Ml.AppE
        ( Ml.LitE "Value.Make.tuple",
          [ Dynamic_gen.make_typ_expr typ; Ml.ListE marshal_calls_ml ] )
    in
    let expr_let_ml = Ml.LetE (pat_vars_ml, Ml.VarE "x__", expr_tuple_ml) in
    Ml.FunE ([ Ml.VarP "x__" ], expr_let_ml)
  in
  let expr_unmarshal_ml =
    let expr_get_ml = Ml.AppE (Ml.LitE "Value.Get.tuple", [ Ml.VarE "v__" ]) in
    let pat_vars_ml = Ml.ListP (List.map (fun var -> Ml.VarP var) vars_u) in
    let arm_ok_ml = (pat_vars_ml, Ml.TupleE unmarshal_calls_ml) in
    let arm_wild_ml = (Ml.WildP, Common.raise_unmatch "resolve: tuple") in
    let expr_match_ml = Ml.MatchE (expr_get_ml, [ arm_ok_ml; arm_wild_ml ]) in
    Ml.FunE ([ Ml.VarP "v__" ], expr_match_ml)
  in
  { marshal = expr_marshal_ml; unmarshal = expr_unmarshal_ml }

(* Option type *)

and resolve_opt_typ ~(visiting : string list) (ctx : Ctx.t)
    (tparams : string list) (typ : Sl.typ) (t : Sl.typ) : t =
  let conv = resolve ~visiting ctx tparams t in
  let expr_marshal_ml =
    let expr_map_ml =
      Ml.AppE (Ml.LitE "Option.map", [ conv.marshal; Ml.VarE "x__" ])
    in
    let expr_opt_ml =
      Ml.AppE
        ( Ml.LitE "Value.Make.opt",
          [ Dynamic_gen.make_typ_expr typ; expr_map_ml ] )
    in
    Ml.FunE ([ Ml.VarP "x__" ], expr_opt_ml)
  in
  let expr_unmarshal_ml =
    let expr_get_ml = Ml.AppE (Ml.LitE "Value.Get.opt", [ Ml.VarE "v__" ]) in
    let expr_map_ml =
      Ml.AppE (Ml.LitE "Option.map", [ conv.unmarshal; expr_get_ml ])
    in
    Ml.FunE ([ Ml.VarP "v__" ], expr_map_ml)
  in
  { marshal = expr_marshal_ml; unmarshal = expr_unmarshal_ml }

(* List type *)

and resolve_list_typ ~(visiting : string list) (ctx : Ctx.t)
    (tparams : string list) (typ : Sl.typ) (t : Sl.typ) : t =
  let conv = resolve ~visiting ctx tparams t in
  let expr_marshal_ml =
    let expr_map_ml =
      Ml.AppE (Ml.LitE "List.map", [ conv.marshal; Ml.VarE "x__" ])
    in
    let expr_list_ml =
      Ml.AppE
        ( Ml.LitE "Value.Make.list",
          [ Dynamic_gen.make_typ_expr typ; expr_map_ml ] )
    in
    Ml.FunE ([ Ml.VarP "x__" ], expr_list_ml)
  in
  let expr_unmarshal_ml =
    let expr_get_ml = Ml.AppE (Ml.LitE "Value.Get.list", [ Ml.VarE "v__" ]) in
    let expr_map_ml =
      Ml.AppE (Ml.LitE "List.map", [ conv.unmarshal; expr_get_ml ])
    in
    Ml.FunE ([ Ml.VarP "v__" ], expr_map_ml)
  in
  { marshal = expr_marshal_ml; unmarshal = expr_unmarshal_ml }

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
          let expr_field_ml = Ml.FieldE (x_ann, Names.field atom) in
          let expr_conv_ml =
            apply_converter "resolved_" conv.marshal expr_field_ml
          in
          Ml.TupleE [ expr_atom_ml; expr_conv_ml ])
        field_convs
    in
    let expr_str_ml =
      Ml.AppE
        ( Ml.LitE "Value.Make.str",
          [ Dynamic_gen.make_typ_expr typ; Ml.ListE field_exprs_ml ] )
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
      Ml.AnnotE (Ml.RecordE field_bindings_ml, Type.compile_typ ~tparams typ)
    in
    let expr_let_ml = Ml.LetE (pat_fields_ml, expr_str_ml, expr_record_ml) in
    Ml.FunE ([ Ml.VarP "v__" ], expr_let_ml)
  in
  { marshal = expr_marshal_ml; unmarshal = expr_unmarshal_ml }

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
          let expr_case_ml =
            compile_case_value
              (Dynamic_gen.make_mixop_expr mixop)
              (Ml.ListE marshal_calls_ml)
              (Dynamic_gen.make_typ_expr typ)
          in
          (pat_ml, expr_case_ml))
        per_ctor
    in
    let expr_match_ml = Ml.MatchE (Ml.VarE "x__", arms_ml) in
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
    Ml.FunE ([ Ml.VarP "v__" ], expr_match_ml)
  in
  { marshal = expr_marshal_ml; unmarshal = expr_unmarshal_ml }

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
        Ml.TupleE
          [ Ml.StrE name; Ml.TupleE [ expr_marshal_ml; expr_unmarshal_ml ] ])
      typs
  in
  let expr_seq_ml = Ml.AppE (Ml.LitE "List.to_seq", [ Ml.ListE entries_ml ]) in
  let expr_table_ml = Ml.AppE (Ml.LitE "Hashtbl.of_seq", [ expr_seq_ml ]) in
  Ml.Let ("interface_registry_", expr_table_ml)
