open Lang
module Collect = Collect
module Naming = Naming
module Dynamic_gen = Dynamic_gen
module Constpool = Constpool
module Marshal = Marshal
module Unmarshal = Unmarshal
module Typed = Typed
module Converter = Converter
module Trampoline = Trampoline

let compile (ctx : Ctx.t) (typs : Sl.typ list) (typs_groups : Sl.typ list list)
    ~(tid_program : string) : Ml.toplevel list =
  ignore tid_program;
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
  let funcdefs_typed_ml = Typed.compile_marshal_dispatch typs in
  let toplevel_typed_ml =
    funcdefs_typed_ml |> List.map (fun fd -> Ml.LetRec [ fd ])
  in
  (* [case_of_typed]/[make_case_typed] share the marshal const pool (interned
     mixops) — thread [pool] through so its extra consts are emitted below. *)
  let pool, funcdefs_case_ml = Typed.compile_case_dispatch ctx pool typs in
  let toplevel_case_ml =
    funcdefs_case_ml |> List.map (fun fd -> Ml.LetRec [ fd ])
  in
  let toplevel_consts_ml =
    List.rev_map (fun (var, expr_ml) -> Ml.Let (var, expr_ml)) pool.consts
  in
  let toplevel_converter_table_ml = Converter.compile_converter_table typs in
  toplevel_consts_ml @ toplevel_marshals_ml @ toplevel_unmarshals_ml
  @ toplevel_typed_ml @ toplevel_case_ml @ [ toplevel_converter_table_ml ]
