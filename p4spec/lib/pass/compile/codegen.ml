open Lang

let compile_spec (path_out : string) (path_out_unparse : string option)
    (spec : Sl.spec) =
  (* Monomorphize the spec *)
  let spec, dispatch_table = Mono.monomorphize spec in
  (* Initialize context *)
  let ctx = Ctx.init spec in
  (* Type definitions *)
  let ctx, toplevel_typdefs_ml =
    let ctx, typdefs_ml = Gen.Type.compile_spec ctx spec in
    (ctx, Ml.TypeRec typdefs_ml)
  in
  (* Marshal/unmarshal *)
  let toplevels_interface_ml =
    let funcdefs_marshal_ml, funcdefs_unmarshal_ml =
      Gen.Interface.compile ctx spec
    in
    [ Ml.LetRec funcdefs_marshal_ml; Ml.LetRec funcdefs_unmarshal_ml ]
  in
  (* Functor *)
  let ctx, toplevels_functor_ml =
    let ctx, funcdefs_ml, cache_ids_func =
      Gen.Func.compile_spec ctx spec dispatch_table
    in
    let ctx, reldefs_ml, cache_ids_rel = Gen.Rel.compile_spec ctx spec in
    let all_cache_ids = cache_ids_func @ cache_ids_rel in
    let funcdef_eval_func_ml =
      Gen.Dispatch.compile_eval_func ctx spec dispatch_table
    in
    let funcdef_eval_rel_ml = Gen.Dispatch.compile_eval_rel ctx spec in
    let toplevels_cache_decls_ml =
      List.map
        (fun cache_id -> Ml.Let (cache_id, Ml.LitE "Hashtbl.create 256"))
        all_cache_ids
    in
    let toplevels_functor_ml =
      [ Ml.Raw Template.Functor.header ]
      @ toplevels_cache_decls_ml
      @ [
          Ml.Raw (Template.Functor.cache_section all_cache_ids);
          Ml.LetRec (funcdefs_ml @ reldefs_ml);
          Ml.LetRec [ funcdef_eval_func_ml; funcdef_eval_rel_ml ];
          Ml.Raw Template.Functor.footer;
        ]
    in
    (ctx, toplevels_functor_ml)
  in
  (* Prelude *)
  let toplevel_prelude_ml = Ml.Raw (Template.Prelude.prelude ctx) in
  (* Assemble *)
  let file_ml =
    [ toplevel_prelude_ml; toplevel_typdefs_ml ]
    @ toplevels_interface_ml @ toplevels_functor_ml
  in
  let out_str = Ml.Print.print_file file_ml in
  let oc = open_out path_out in
  output_string oc out_str;
  close_out oc;
  Option.iter
    (fun path_out_unparse ->
      Gen.Unparse.compile_spec ctx spec ~path_out:path_out_unparse)
    path_out_unparse
