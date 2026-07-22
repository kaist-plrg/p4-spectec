open Lang
module Collect = Collect
module Naming = Naming
module Dynamic_gen = Dynamic_gen
module Constpool = Constpool
module Marshal = Marshal
module Unmarshal = Unmarshal
module Note = Note
module Converter = Converter
module Typed = Typed
module Trampoline = Trampoline

let compile (ctx : Ctx.t) (typs : Sl.typ list) (typs_groups : Sl.typ list list)
    : Ml.toplevel list =
  let pool, marshal_groups =
    List.fold_left_map
      (List.fold_left_map (Marshal.compile ctx))
      Constpool.empty typs_groups
  in
  let toplevel_marshals_ml =
    List.filter_map
      (fun funcdefs ->
        match funcdefs with [] -> None | _ -> Some (Ml.LetRec funcdefs))
      marshal_groups
  in
  let unmarshal_groups =
    List.map (List.map (Unmarshal.compile ctx)) typs_groups
  in
  let toplevel_unmarshals_ml =
    List.filter_map
      (fun funcdefs ->
        match funcdefs with [] -> None | _ -> Some (Ml.LetRec funcdefs))
      unmarshal_groups
  in
  (* one mutually-recursive block: a mono family resolves a [set] field to the
     poly [eq_set], and a poly family resolves a ground field to a mono one *)
  let funcdefs_note_ml =
    List.concat_map (List.concat_map (Note.compile ctx)) typs_groups
    @ Note.compile_poly_all ctx
  in
  let toplevel_notes_ml =
    match funcdefs_note_ml with [] -> [] | _ -> [ Ml.LetRec funcdefs_note_ml ]
  in
  let pool, funcdefs_typed = Typed.compile ctx pool typs in
  let funcdefs_marshal_dispatch = Typed.compile_marshal_dispatch typs in
  (* [hash_typed] first: [make_case]'s element hash dictionaries call it *)
  let funcdef_hash_dispatch = Typed.compile_hash_dispatch typs in
  let toplevel_typed_ml =
    (funcdef_hash_dispatch :: funcdefs_typed) @ funcdefs_marshal_dispatch
    |> List.map (fun funcdef_ml -> Ml.LetRec [ funcdef_ml ])
  in
  let toplevel_consts_ml =
    List.rev_map (fun (var, expr_ml) -> Ml.Let (var, expr_ml)) pool.consts
  in
  let toplevel_converter_table_ml = Converter.compile_converter_table typs in
  (* notes first: [unmarshal]/[typed] construct via [mk_<t>], and marshal reads
     only [.it], so nothing notes depend on comes earlier *)
  toplevel_consts_ml @ toplevel_notes_ml @ toplevel_marshals_ml
  @ toplevel_unmarshals_ml @ toplevel_typed_ml @ [ toplevel_converter_table_ml ]
