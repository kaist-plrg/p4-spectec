open Lang
module Typ = Runtime.Type
module Typdef = Typ.Typdef
open Util.Source

(* Booleans *)

let compile_bool_typ = Ml.AppE (Ml.LitE "Value.Get.bool", [ Ml.VarE "v" ])

(* Numbers *)

let compile_num_typ =
  let expr_num_ml = Ml.AppE (Ml.LitE "Value.Get.num", [ Ml.VarE "v" ]) in
  let arm_nat_ml =
    (Ml.VariantP (`Poly ("Nat", [ Ml.VarP "n_" ])), Ml.VarE "n_")
  in
  let arm_int_ml =
    (Ml.VariantP (`Poly ("Int", [ Ml.VarP "i_" ])), Ml.VarE "i_")
  in
  Ml.MatchE (expr_num_ml, [ arm_nat_ml; arm_int_ml ])

(* Texts *)

let compile_text_typ = Ml.AppE (Ml.LitE "Value.Get.text", [ Ml.VarE "v" ])

(* Variable typs *)

(* Structs *)

let compile_field_access (s : string) (expr_fields_ml : Ml.expr) : Ml.expr =
  let expr_pred_ml =
    Ml.LitE
      (Printf.sprintf "(fun ({ it; _ }, _) -> it = Atom.Atom \"%s\")"
         (String.escaped s))
  in
  Ml.AppE
    ( Ml.LitE "snd",
      [ Ml.AppE (Ml.LitE "List.find", [ expr_pred_ml; expr_fields_ml ]) ] )

let compile_struct_typ (typfields : Sl.typfield list) : Ml.expr =
  let field_bindings_ml =
    List.map
      (fun (atom, typ) ->
        let atom_str = Names.Ctor.atom atom in
        let field_id = Names.field atom in
        let expr_field_ml = compile_field_access atom_str (Ml.VarE "fields_") in
        let expr_unmarshal_ml =
          Ml.AppE (Ml.VarE ("unmarshal_" ^ Naming.name typ), [ expr_field_ml ])
        in
        (field_id, expr_unmarshal_ml))
      typfields
  in
  let pat_fields_ml = Ml.VarP "fields_" in
  let expr_str_ml = Ml.AppE (Ml.LitE "Value.Get.str", [ Ml.VarE "v" ]) in
  let expr_record_ml = Ml.RecordE field_bindings_ml in
  Ml.LetE (pat_fields_ml, expr_str_ml, expr_record_ml)

(* Variants *)

let compile_variant_typ (name : string)
    (ctors : (Domain.Mixop.t * Ml.ctor * Sl.typ list) list) : Ml.expr =
  let arms_ctor_ml =
    List.map
      (fun (mixop, ctor_ml, payload_typs) ->
        let pat_str, ids_arg_ml = Dynamic_gen.make_mixop_pat_string mixop in
        let exprs_payload_ml =
          List.map2
            (fun typ id_arg_ml ->
              Ml.AppE
                (Ml.VarE ("unmarshal_" ^ Naming.name typ), [ Ml.VarE id_arg_ml ]))
            payload_typs ids_arg_ml
        in
        let pat_ctor_ml = Ml.LitP pat_str in
        let expr_ctor_ml = Ml.VariantE (ctor_ml, exprs_payload_ml) in
        (pat_ctor_ml, expr_ctor_ml))
      ctors
  in
  let arm_unknown_ml =
    let expr_raise_ml =
      Common.raise_unmatch (Printf.sprintf "unmarshal_%s: unknown case" name)
    in
    (Ml.WildP, expr_raise_ml)
  in
  let expr_it_ml = Ml.FieldE (Ml.VarE "v", "it") in
  let pat_case_ml = Ml.VariantP (`Mono ("CaseV", [ Ml.VarP "vc_" ])) in
  let expr_match_ctor_ml =
    Ml.MatchE (Ml.VarE "vc_", arms_ctor_ml @ [ arm_unknown_ml ])
  in
  let arm_case_ml = (pat_case_ml, expr_match_ctor_ml) in
  let arm_wild_ml = (Ml.WildP, Common.raise_unmatch ("unmarshal_" ^ name)) in
  Ml.MatchE (expr_it_ml, [ arm_case_ml; arm_wild_ml ])

let compile_var_typ (ctx : Ctx.t) (id : Sl.id) (targs : Sl.targ list)
    (name : string) : Ml.expr =
  match Ctx.find_typdef ctx id with
  | Typdef.Param | Typdef.Defining _ ->
      Common.raise_unmatch ("unmarshal_" ^ name)
  | Typdef.Defined (tparams, deftyp) -> (
      let theta = Domain.Lib.TIdMap.of_lists tparams targs in
      match deftyp.it with
      | Il.PlainT typ_alias ->
          let typ_alias = Typ.Subst.subst_typ theta typ_alias in
          let name_alias = "unmarshal_" ^ Naming.name typ_alias in
          Ml.AppE (Ml.VarE name_alias, [ Ml.VarE "v" ])
      | Il.StructT typfields ->
          let typfields =
            List.map
              (fun (atom, typ) ->
                let typ = Typ.Subst.subst_typ theta typ in
                (atom, typ))
              typfields
          in
          compile_struct_typ typfields
      | Il.VariantT _ ->
          let ctors = Ctx.find_ctors_full ctx id in
          let ctors =
            List.map
              (fun (mixop, ctor_ml, typs) ->
                let typs = Typ.Subst.subst_typs theta typs in
                (mixop, ctor_ml, typs))
              ctors
          in
          compile_variant_typ name ctors)
  | Typdef.Extern -> Ml.AppE (Ml.LitE "Value.Get.extern", [ Ml.VarE "v" ])

(* Tuples *)

let compile_tuple_typ (name : string) (typs : Sl.typ list) : Ml.expr =
  let n = List.length typs in
  let vars = List.init n (fun i -> "v" ^ string_of_int i) in
  let unmarshal_calls =
    List.mapi
      (fun i typ ->
        Ml.AppE
          ( Ml.VarE ("unmarshal_" ^ Naming.name typ),
            [ Ml.VarE (List.nth vars i) ] ))
      typs
  in
  let expr_tuple_ml = Ml.AppE (Ml.LitE "Value.Get.tuple", [ Ml.VarE "v" ]) in
  let pat_vars_ml = Ml.ListP (List.map (fun var -> Ml.VarP var) vars) in
  let arm_ok_ml = (pat_vars_ml, Ml.TupleE unmarshal_calls) in
  let arm_wild_ml = (Ml.WildP, Common.raise_unmatch ("unmarshal_" ^ name)) in
  Ml.MatchE (expr_tuple_ml, [ arm_ok_ml; arm_wild_ml ])

(* Iterations *)

let compile_iter_opt_typ (typ : Sl.typ) : Ml.expr =
  let expr_opt_ml = Ml.AppE (Ml.LitE "Value.Get.opt", [ Ml.VarE "v" ]) in
  let arm_none_ml = (Ml.OptP None, Ml.OptE None) in
  let arm_some_ml =
    let pat_inner_ml = Ml.OptP (Some (Ml.VarP "v_inner_")) in
    let expr_unmarshal_ml =
      Ml.AppE (Ml.VarE ("unmarshal_" ^ Naming.name typ), [ Ml.VarE "v_inner_" ])
    in
    (pat_inner_ml, Ml.OptE (Some expr_unmarshal_ml))
  in
  Ml.MatchE (expr_opt_ml, [ arm_none_ml; arm_some_ml ])

let compile_iter_list_typ (typ : Sl.typ) : Ml.expr =
  let expr_list_ml = Ml.AppE (Ml.LitE "Value.Get.list", [ Ml.VarE "v" ]) in
  Ml.AppE
    ( Ml.LitE "List.map",
      [ Ml.VarE ("unmarshal_" ^ Naming.name typ); expr_list_ml ] )

let compile_iter_typ (typ : Sl.typ) (iter : Sl.iter) : Ml.expr =
  match iter with
  | Opt -> compile_iter_opt_typ typ
  | List -> compile_iter_list_typ typ

(* Entry point *)

let compile_body_typ (ctx : Ctx.t) (typ : Sl.typ) : Ml.expr =
  let name = Naming.name typ in
  match typ.it with
  | Il.BoolT -> compile_bool_typ
  | Il.NumT _ -> compile_num_typ
  | Il.TextT -> compile_text_typ
  | Il.VarT (id, targs) -> compile_var_typ ctx id targs name
  | Il.TupleT typs -> compile_tuple_typ name typs
  | Il.IterT (typ_inner, iter) -> compile_iter_typ typ_inner iter
  | Il.FuncT _ -> Common.raise_unmatch "unmarshal_func"

let compile (ctx : Ctx.t) (typ : Sl.typ) : Ml.funcdef =
  let name = "unmarshal_" ^ Naming.name typ in
  let param_ml = ("v", Some (Ml.NameT "Value.t")) in
  let typ_ret_ml = Some (Type.compile_typ ~tparams:[] typ) in
  let expr_body_ml = compile_body_typ ctx typ in
  (name, [], [ param_ml ], typ_ret_ml, expr_body_ml)
