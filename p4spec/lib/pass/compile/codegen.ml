open Lang

let compile_spec (path_out : string) (spec : Sl.spec) =
  let spec_sl, _dispatch_table = Mono.monomorphize spec in
  let ctx = Gen.Ctx.init () in
  (* Type definitions *)
  let toplevel_typdefs_ml =
    let typdefs_ml = Gen.Type.compile_spec spec_sl in
    Ml.TypeRec typdefs_ml
  in
  (* Functor *)
  let ctx, toplevel_functor_ml =
    (* Function definitions *)
    let ctx, funcdefs_ml = Gen.Func.compile_spec ctx spec_sl in
    let toplevel_functor_ml =
      [ Ml.Raw Functor.header; Ml.LetRec funcdefs_ml; Ml.Raw Functor.footer ]
    in
    (ctx, toplevel_functor_ml)
  in
  (* Prelude *)
  let toplevel_prelude_ml = Ml.Raw (Prelude.prelude ctx) in
  (* Assemble the file *)
  let file_ml =
    [ toplevel_prelude_ml; toplevel_typdefs_ml ] @ toplevel_functor_ml
  in
  let out_str = Ml.Print.print_file file_ml in
  let oc = open_out path_out in
  output_string oc out_str;
  close_out oc
