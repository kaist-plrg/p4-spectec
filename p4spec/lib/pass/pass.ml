type error = Diagnostic.t

let ( let* ) = Result.bind

(* Stages *)

let parse_string = Frontend.Parse.parse_string
let parse_files = Frontend.Parse.parse_files
let elab_spec = Elaborate.elab_spec
let algo_spec = Algo.algo_spec
let struct_spec = Structure.struct_spec
let annotate_spec = Annotate.annotate_spec

(* Parsing *)

let parse paths_spec = parse_files paths_spec

(* Elaboration *)

let cache_elab = Hashtbl.create 8

let elab paths_spec =
  match Hashtbl.find_opt cache_elab paths_spec with
  | Some spec -> Ok spec
  | None ->
      let* spec_el = parse paths_spec in
      let* spec_il = elab_spec spec_el in
      Hashtbl.replace cache_elab paths_spec spec_il;
      Ok spec_il

(* Algorithmic conversion *)

let cache_algo = Hashtbl.create 8

let algo paths_spec =
  match Hashtbl.find_opt cache_algo paths_spec with
  | Some spec -> Ok spec
  | None ->
      let* spec_il = elab paths_spec in
      let* spec_al = algo_spec spec_il in
      Hashtbl.replace cache_algo paths_spec spec_al;
      Ok spec_al

(* Structuring *)

let structure_cache = Hashtbl.create 8

let structure ~(final : bool) paths_spec =
  match Hashtbl.find_opt structure_cache (final, paths_spec) with
  | Some spec -> Ok spec
  | None ->
      let* spec_al = algo paths_spec in
      let* spec_sl = struct_spec ~final spec_al in
      Hashtbl.replace structure_cache (final, paths_spec) spec_sl;
      Ok spec_sl

(* Annotation (prose) generation *)

let annotate paths_spec =
  let* spec_sl = structure ~final:false paths_spec in
  annotate_spec spec_sl
