open Lang

(* ── File helpers ── *)

let write_file (path : string) (content : string) : unit =
  let oc = open_out path in
  output_string oc content;
  close_out oc

(* Create [dir] if missing, then delete any stale [*.ml] in it so a rebuild with
   a different bucket count never leaves orphan part files behind. *)
let prepare_dir (dir : string) : unit =
  if not (Sys.file_exists dir) then Util.Filesys.mkdir dir;
  Sys.readdir dir
  |> Array.iter (fun f ->
         if Filename.check_suffix f ".ml" then
           Sys.remove (Filename.concat dir f))

(* ── Bucketing ── *)

let toplevel_lines (tl : Ml.toplevel) : int =
  let s = Ml.Print.print_toplevel tl in
  if s = "" then 0
  else String.fold_left (fun n c -> if c = '\n' then n + 1 else n) 1 s

(* Walk [tls] in topo order, accumulating into the current bucket until adding a
   group would exceed [target] lines, then start a new bucket. A group (one
   [Ml.toplevel] — an SCC [LetRec], a [TypeRec], or a const [Let]) is never
   split, so a single oversized SCC just yields an oversized bucket (the floor). *)
let bucket (target : int) (tls : Ml.toplevel list) : Ml.toplevel list list =
  let buckets, cur, _ =
    List.fold_left
      (fun (buckets, cur, cur_lines) tl ->
        let l = toplevel_lines tl in
        if cur <> [] && cur_lines + l > target then
          (buckets @ [ List.rev cur ], [ tl ], l)
        else (buckets, tl :: cur, cur_lines + l))
      ([], [], 0) tls
  in
  buckets @ if cur = [] then [] else [ List.rev cur ]

(* ── Entry ── *)

let compile_spec (path_out : string) (path_out_unparse : string option)
    (split_lines : int) (spec : Sl.spec) =
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
    let const_decls, marshal_groups, unmarshal_groups, typed_bridges =
      Gen.Interface.compile ctx spec
    in
    let to_tops groups =
      List.filter_map
        (fun funcdefs ->
          match funcdefs with [] -> None | _ -> Some (Ml.LetRec funcdefs))
        groups
    in
    (* Each typed bridge is one (non-recursive) top-level function. *)
    let typed_tops = List.map (fun fd -> Ml.LetRec [ fd ]) typed_bridges in
    const_decls @ to_tops marshal_groups @ to_tops unmarshal_groups
    @ typed_tops
  in
  (* Logic groups — one Ml.LetRec per SCC group, in topo order. The heavy code
     lives at module top-level (no longer inside the functor), reading the
     per-instance context [cur__]. *)
  let ctx, toplevels_groups_ml =
    let scc_groups = Scc.Call.compute spec in
    List.fold_left
      (fun (ctx, tops_acc) group ->
        let ctx, funcdefs_ml =
          Gen.Func.compile_group ctx group dispatch_table
        in
        let ctx, reldefs_ml = Gen.Rel.compile_group ctx group in
        let combined = funcdefs_ml @ reldefs_ml in
        let tops_acc =
          if combined = [] then tops_acc else tops_acc @ [ Ml.LetRec combined ]
        in
        (ctx, tops_acc))
      (ctx, []) scc_groups
  in
  (* Dispatch entry points — [eval_func]/[eval_rel]/[eval_program]. *)
  let toplevels_dispatch_ml =
    let funcdef_eval_func_ml =
      Gen.Dispatch.compile_eval_func ctx spec dispatch_table
    in
    let funcdef_eval_rel_ml = Gen.Dispatch.compile_eval_rel ctx spec in
    [
      Ml.LetRec [ funcdef_eval_func_ml; funcdef_eval_rel_ml ];
      Ml.Raw Template.Functor.eval_program;
    ]
  in
  (* The whole topo-ordered stream that becomes the [part_NNN.ml] files: types,
     then marshal/unmarshal, then the ctx-routed logic groups. Every cross-group
     reference is backwards in this order, so each part [open]s all prior parts
     and stays well-typed. *)
  let parts_stream =
    toplevel_typdefs_ml @ toplevels_interface_ml @ toplevels_groups_ml
  in
  let buckets = bucket split_lines parts_stream in
  let n_parts = List.length buckets in
  (* Emit the [compiled/] part-library next to [spec_compiled.ml]. *)
  let compiled_dir = Filename.concat (Filename.dirname path_out) "compiled" in
  prepare_dir compiled_dir;
  (* ctx.ml — prelude (opens/aliases/helpers/Option/List) + the ctx glue. *)
  let ctx_ml =
    Template.Prelude.prelude ctx ^ "\n" ^ Template.Ctx_glue.glue ()
  in
  write_file (Filename.concat compiled_dir "ctx.ml") ctx_ml;
  (* part_NNN.ml *)
  List.iteri
    (fun idx group_tls ->
      let body = Ml.Print.print_file group_tls in
      let content = Template.Split.part_header idx ^ "\n" ^ body in
      write_file
        (Filename.concat compiled_dir (Template.Split.part_file_name idx))
        content)
    buckets;
  (* dispatch.ml *)
  let dispatch_ml =
    Template.Split.dispatch_header n_parts
    ^ "\n"
    ^ Ml.Print.print_file toplevels_dispatch_ml
  in
  write_file (Filename.concat compiled_dir "dispatch.ml") dispatch_ml;
  (* dune *)
  write_file (Filename.concat compiled_dir "dune") Template.Split.dune;
  (* Thin functor shell at the original [-o] path. *)
  write_file path_out Template.Functor.make;
  (* Unparse helper (independent output). *)
  Option.iter
    (fun path_out_unparse ->
      Gen.Unparse.compile_spec ctx spec ~path_out:path_out_unparse)
    path_out_unparse
