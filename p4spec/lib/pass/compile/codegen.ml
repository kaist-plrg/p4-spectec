open Lang
module Filesys = Util.Filesys

(* Create [dir] if missing, then delete any stale [*.ml] in it *)

let prepare_dir (dir : string) : unit =
  if not (Sys.file_exists dir) then Filesys.mkdir dir;
  Sys.readdir dir
  |> Array.iter (fun f ->
         if Filename.check_suffix f ".ml" then
           Sys.remove (Filename.concat dir f))

(* Entry point *)

let compile_spec ?(name = "") (path_out : string)
    (path_out_unparse : string option) (loc_split : int) (spec : Sl.spec) =
  (* Initialize context *)
  let ctx = Ctx.init spec in
  (* Type definitions *)
  let ctx, toplevel_typdefs_ml =
    let defs_typ_groups = Scc.Type.compute spec in
    let ctx, typdef_groups_ml = Gen.Type.compile_spec_scc ctx defs_typ_groups in
    let toplevel_typedefs_ml =
      List.filter_map
        (fun typdefs_ml ->
          match typdefs_ml with [] -> None | _ -> Some (Ml.TypeRec typdefs_ml))
        typdef_groups_ml
    in
    (ctx, toplevel_typedefs_ml)
  in
  (* Marshal/unmarshal *)
  let toplevels_interface_ml =
    let typs = Gen.Interface.Collect.collect_types ctx spec in
    let typs_groups = Scc.Converter.compute ctx typs in
    Gen.Interface.compile ctx typs typs_groups
  in
  (* Relations and functions *)
  let ctx, toplevels_groups_ml =
    let defs_call_groups = Scc.Call.compute spec in
    List.fold_left
      (fun (ctx, toplevels_ml) defs_call_group ->
        let ctx, funcdefs_ml = Gen.Ast.Func.compile_group ctx defs_call_group in
        let ctx, reldefs_ml = Gen.Ast.Rel.compile_group ctx defs_call_group in
        let defs_ml = funcdefs_ml @ reldefs_ml in
        let toplevels_ml =
          if defs_ml = [] then toplevels_ml
          else toplevels_ml @ [ Ml.LetRec defs_ml ]
        in
        (ctx, toplevels_ml))
      (ctx, []) defs_call_groups
  in
  (* Dispatch entry points: [eval_func]/[eval_rel]/[eval_program] *)
  let toplevels_dispatch_ml =
    let funcdef_eval_func_ml = Gen.Dispatch.compile_eval_func ctx spec in
    let funcdef_eval_rel_ml = Gen.Dispatch.compile_eval_rel ctx spec in
    let funcdef_eval_func_native_ml =
      Gen.Dispatch.compile_eval_func_native spec
    in
    let funcdef_eval_rel_native_ml = Gen.Dispatch.compile_eval_rel_native spec in
    [
      Ml.Raw Template.Converter.converter_table;
      Ml.LetRec [ funcdef_eval_func_ml; funcdef_eval_rel_ml ];
      Ml.LetRec [ funcdef_eval_func_native_ml; funcdef_eval_rel_native_ml ];
      Ml.Raw Template.Functor.eval_program;
      Ml.Raw Template.Functor.unmarshal_program;
      (* Stable re-exports of the typed mixop bridges, so [V_native] can bind them
         at [Spec_parts_<name>.Dispatch.*] without depending on which (unstable)
         [part_NNN] they bucket into. *)
      Ml.Let ("make_case_typed", Ml.VarE "make_case_typed");
      Ml.Let ("case_of_typed", Ml.VarE "case_of_typed");
      (* Typename-indexed marshal/unmarshal — the state-persist bridge
         [V_native.marshal]/[unmarshal] dispatch through. *)
      Ml.Let ("marshal_typed", Ml.VarE "marshal_typed");
      Ml.Let ("unmarshal_typed", Ml.VarE "unmarshal_typed");
    ]
  in
  (* Concatenate the top-level definitions and split into buckets *)
  let toplevels_ml =
    toplevel_typdefs_ml @ toplevels_interface_ml @ toplevels_groups_ml
  in
  let toplevels_ml_buckets = Template.Split.bucket loc_split toplevels_ml in
  let n_parts = List.length toplevels_ml_buckets in
  (* Emit the [compiled/] part-library next to [interp_ml.ml] *)
  let dirname_compiled =
    Filename.concat (Filename.dirname path_out) "compiled"
  in
  prepare_dir dirname_compiled;
  (* Emit [trampoline.ml], containing the prelude and the trampoline glue *)
  let s_trampoline =
    Template.Prelude.prelude ctx ^ "\n" ^ Template.Trampoline.glue ()
  in
  Filesys.write_file
    (Filename.concat dirname_compiled "trampoline.ml")
    s_trampoline;
  (* Emit the buckets to [part_NNN.ml] *)
  List.iteri
    (fun idx toplevels_ml ->
      let s_body = Ml.Print.print_file toplevels_ml in
      let s_content = Template.Split.prelude_part idx ^ "\n" ^ s_body in
      Filesys.write_file
        (Filename.concat dirname_compiled (Template.Split.name_part_file idx))
        s_content)
    toplevels_ml_buckets;
  (* Emit [dispatch.ml] *)
  let s_dispatch =
    Template.Split.prelude_dispatch n_parts
    ^ "\n"
    ^ Ml.Print.print_file toplevels_dispatch_ml
  in
  Filesys.write_file (Filename.concat dirname_compiled "dispatch.ml") s_dispatch;
  (* Emit [dune] *)
  Filesys.write_file
    (Filename.concat dirname_compiled "dune")
    (Template.Dune.dune name);
  (* Emit a functor shell at the [-o] path *)
  Filesys.write_file path_out (Template.Functor.make name);
  (* Emit an unparse helper (independent output) *)
  Option.iter
    (fun path_out_unparse ->
      Gen.Unparse.compile_spec ctx spec ~path_out:path_out_unparse)
    path_out_unparse
