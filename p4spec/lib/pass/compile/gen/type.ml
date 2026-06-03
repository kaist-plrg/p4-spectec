open Domain
open Lang
open Util.Source

(* Compiling typs *)

let rec compile_typ ~(tparams : string list) (typ : Sl.typ) : Ml.Ast.typ =
  match typ.it with
  | Il.BoolT -> compile_bool_typ
  | Il.NumT _ -> compile_num_typ
  | Il.TextT -> compile_text_typ
  | Il.VarT (id, targs) -> compile_var_typ ~tparams id targs
  | Il.TupleT typs -> compile_tuple_typ ~tparams typs
  | Il.IterT (typ, iter) -> compile_iter_typ ~tparams typ iter
  | Il.FuncT _ -> compile_func_typ

and compile_typs ~(tparams : string list) (typs : Sl.typ list) : Ml.Ast.typ list
    =
  List.map (compile_typ ~tparams) typs

(* Compiling boolean types *)

and compile_bool_typ : Ml.Ast.typ = Ml.Ast.BoolT

(* Compiling number types *)

and compile_num_typ : Ml.Ast.typ = Ml.Ast.BigintT

(* Compiling text types *)

and compile_text_typ : Ml.Ast.typ = Ml.Ast.StringT

(* Compiling variable types *)

and compile_var_typ ~(tparams : string list) (id : Sl.id) (targs : Sl.targ list)
    : Ml.Ast.typ =
  match targs with
  | [] when List.mem id.it tparams ->
      let id_ml = Ml.Names.tvar id.it in
      Ml.Ast.VarT id_ml
  | [] when not (List.mem id.it tparams) ->
      let id_ml = Ml.Names.var id.it in
      Ml.Ast.NameT id_ml
  | _ ->
      let id_ml = Ml.Names.var id.it in
      let targs_ml = compile_typs ~tparams targs in
      Ml.Ast.AppT (id_ml, targs_ml)

(* Compiling tuple types *)

and compile_tuple_typ ~(tparams : string list) (typs : Sl.typ list) : Ml.Ast.typ
    =
  let typs_ml = compile_typs ~tparams typs in
  Ml.Ast.TupleT typs_ml

(* Compiling iter types *)

and compile_iter_typ ~(tparams : string list) (typ : Sl.typ) (iter : Sl.iter) :
    Ml.Ast.typ =
  match iter with
  | Il.Opt ->
      let typ_ml = compile_typ ~tparams typ in
      Ml.Ast.AppT ("option", [ typ_ml ])
  | Il.List ->
      let typ_ml = compile_typ ~tparams typ in
      Ml.Ast.AppT ("list", [ typ_ml ])

(* Compiling func types *)

and compile_func_typ : Ml.Ast.typ = Ml.Ast.UnitT

(* Compiling deftyps *)

let rec compile_deftyp ~(tparams : string list) (id_parent : Sl.id)
    (deftyp : Sl.deftyp) : Ml.Ast.deftyp =
  match deftyp.it with
  | Il.PlainT typ -> compile_alias_deftyp ~tparams typ
  | Il.StructT typfields -> compile_struct_deftyp ~tparams typfields
  | Il.VariantT typcases -> compile_variant_deftyp ~tparams id_parent typcases

(* Compiling alias deftyps *)

and compile_alias_deftyp ~(tparams : string list) (typ : Sl.typ) : Ml.Ast.deftyp
    =
  let typ_ml = compile_typ ~tparams typ in
  Ml.Ast.AliasTD typ_ml

(* Compiling struct deftyps *)

and compile_typfield ~(tparams : string list) (typfield : Sl.typfield) :
    Ml.Ast.typfield =
  let atom, typ = typfield in
  let field_ml =
    Ml.Names.escape_keyword (Ml.Names.to_snake_case (Atom.ctor_of_atom atom.it))
  in
  let typ_ml = compile_typ ~tparams typ in
  (field_ml, typ_ml)

and compile_typfields ~(tparams : string list) (typfields : Sl.typfield list) :
    Ml.Ast.typfield list =
  List.map (compile_typfield ~tparams) typfields

and compile_struct_deftyp ~(tparams : string list)
    (typfields : Sl.typfield list) : Ml.Ast.deftyp =
  let typfields_ml = compile_typfields ~tparams typfields in
  Ml.Ast.RecordTD typfields_ml

(* Compiling variant deftyps *)

and is_unique_variant (typcases_all : Sl.typcase list) (id : Sl.id) =
  let count =
    typcases_all
    |> List.filter (fun (_, typorigin, _) ->
           let id_sub, _ = typorigin.it in
           id_sub.it = id.it)
    |> List.length
  in
  count = 1

and compile_typcase ~(tparams : string list) (id_parent : Sl.id)
    (typcases_all : Sl.typcase list) (typcase : Sl.typcase) : Ml.Ast.typcase =
  let nottyp, typorigin, _ = typcase in
  let id_origin, _ = typorigin.it in
  let ctor_ml =
    if is_unique_variant typcases_all id_origin then
      String.capitalize_ascii id_origin.it
    else
      Ml.Names.ctor_of_nottyp
        ~fallback:(String.capitalize_ascii id_parent.it)
        nottyp
  in
  let typs_arg = Domain.Mixfix.args nottyp.it in
  let typs_ml = compile_typs ~tparams typs_arg in
  (ctor_ml, typs_ml)

and dedup_typcases_ml (typcases_ml : Ml.Ast.typcase list) =
  let ctor_seen = Hashtbl.create 16 in
  List.map
    (fun (ctor_ml, typs_ml) ->
      let count = try Hashtbl.find ctor_seen ctor_ml with Not_found -> 0 in
      Hashtbl.replace ctor_seen ctor_ml (count + 1);
      let ctor_ml =
        if count = 0 then ctor_ml else ctor_ml ^ "_" ^ string_of_int count
      in
      (ctor_ml, typs_ml))
    typcases_ml

and compile_typcases ~(tparams : string list) (id_parent : Sl.id)
    (typcases : Sl.typcase list) : Ml.Ast.typcase list =
  typcases
  |> List.map (compile_typcase ~tparams id_parent typcases)
  |> dedup_typcases_ml

and compile_variant_deftyp ~(tparams : string list) (id_parent : Sl.id)
    (typcases : Sl.typcase list) : Ml.Ast.deftyp =
  let typcases_ml = compile_typcases ~tparams id_parent typcases in
  Ml.Ast.VariantTD typcases_ml

(* Compiling defs *)

let compile_def (def : Sl.def) : Ml.Ast.typdef option =
  match def.it with
  | Sl.TypD (id, tparams, deftyp, _) ->
      let tparams = List.map it tparams in
      let tparams_ml = List.map Ml.Names.var tparams in
      let id_ml = Ml.Names.var id.it in
      let deftyp_ml = compile_deftyp ~tparams id deftyp in
      Some (tparams_ml, id_ml, deftyp_ml)
  | Sl.ExternTypD (id, _) ->
      let id_ml = Ml.Names.var id.it in
      let deftyp_ml = Ml.Ast.AliasTD Ml.Ast.UnitT in
      Some ([], id_ml, deftyp_ml)
  | _ -> None

(* Compiling spec *)

let compile_defs (spec : Sl.spec) = spec |> List.filter_map compile_def
