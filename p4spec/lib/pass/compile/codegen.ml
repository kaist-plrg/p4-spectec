open Lang

let compile_spec (path_out : string) (spec : Sl.spec) =
  let spec_sl, _dispatch_table = Mono.monomorphize spec in
  let _ctx = Gen.Ctx.init () in
  (* Prelude *)
  let toplevel_prelude_ml = Ml.Ast.Raw Prelude.prelude in
  (* Type definitions *)
  let toplevel_typdefs_ml =
    let typdefs_ml = Gen.Type.compile_defs spec_sl in
    Ml.Ast.TypeRec typdefs_ml
  in
  (* Functor *)
  let toplevel_functor_ml =
    [ Ml.Ast.Raw Functor.header; Ml.Ast.Raw Functor.footer ]
  in
  (* Assemble the file *)
  let file_ml =
    [ toplevel_prelude_ml; toplevel_typdefs_ml ] @ toplevel_functor_ml
  in
  let out_str = Ml.Print.print_file file_ml in
  let oc = open_out path_out in
  output_string oc out_str;
  close_out oc
