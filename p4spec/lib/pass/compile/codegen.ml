open Lang

let compile_spec (path_out : string) (path_out_unparse : string option)
    (spec : Sl.spec) =
  (* Monomorphize the spec *)
  let spec, dispatch_table = Mono.monomorphize spec in
  (* Initialize context *)
  let ctx = Ctx.init spec in
  (* Type definitions — one Ml.TypeRec per SCC group *)
  let ctx, toplevel_typdefs_ml =
    let type_groups = Scc.Type.compute spec in
    let ctx, typdef_groups_ml = Gen.Type.compile_spec_scc ctx type_groups in
    let tops =
      List.filter_map
        (fun typdefs_ml ->
          match typdefs_ml with [] -> None | _ -> Some (Ml.TypeRec typdefs_ml))
        typdef_groups_ml
    in
    (ctx, tops)
  in
  (* Marshal/unmarshal — one Ml.LetRec per SCC group *)
  let toplevels_interface_ml =
    let marshal_groups, unmarshal_groups = Gen.Interface.compile ctx spec in
    let to_tops groups =
      List.filter_map
        (fun funcdefs ->
          match funcdefs with [] -> None | _ -> Some (Ml.LetRec funcdefs))
        groups
    in
    to_tops marshal_groups @ to_tops unmarshal_groups
  in
  (* Functor *)
  let ctx, toplevels_functor_ml =
    let scc_groups = Scc.Call.compute spec in
    let ctx, toplevels_groups_ml, all_cache_ids =
      List.fold_left
        (fun (ctx, tops_acc, cache_acc) group ->
          let ctx, funcdefs_ml, cids_f =
            Gen.Func.compile_group ctx group dispatch_table
          in
          let ctx, reldefs_ml, cids_r = Gen.Rel.compile_group ctx group in
          let combined = funcdefs_ml @ reldefs_ml in
          let tops_acc =
            if combined = [] then tops_acc
            else tops_acc @ [ Ml.LetRec combined ]
          in
          (ctx, tops_acc, cache_acc @ cids_f @ cids_r))
        (ctx, [], []) scc_groups
    in
    let funcdef_eval_func_ml =
      Gen.Dispatch.compile_eval_func ctx spec dispatch_table
    in
    let funcdef_eval_rel_ml = Gen.Dispatch.compile_eval_rel ctx spec in
    let toplevels_cache_decls_ml =
      List.map
        (fun cache_id -> Ml.Let (cache_id, Ml.LitE "H__.create 4096"))
        all_cache_ids
    in
    let toplevels_functor_ml =
      [ Ml.Raw Template.Functor.header; Ml.Raw Template.Functor.h_module ]
      @ toplevels_cache_decls_ml
      @ [ Ml.Raw (Template.Functor.cache_section all_cache_ids) ]
      @ toplevels_groups_ml
      @ [
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
    [ toplevel_prelude_ml ] @ toplevel_typdefs_ml @ toplevels_interface_ml
    @ toplevels_functor_ml
  in
  let out_str = Ml.Print.print_file file_ml in
  let oc = open_out path_out in
  output_string oc out_str;
  close_out oc;
  Option.iter
    (fun path_out_unparse ->
      Gen.Unparse.compile_spec ctx spec ~path_out:path_out_unparse)
    path_out_unparse
