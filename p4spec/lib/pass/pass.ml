module Elaborate = Elaborate
module Algo = Algo
module Structure = Structure
module Annotate = Annotate

(* Shortcuts *)

let expand_spec filenames =
  List.concat_map
    (fun filename ->
      if Sys_unix.is_directory_exn filename then
        Util.Filesys.collect_files ~suffix:".watsup" filename
      else [ filename ])
    filenames

(* Parsing *)

let parse paths_spec =
  paths_spec |> expand_spec |> List.concat_map Frontend.Parse.parse_file

(* Elaboration. *)

let cache_elab = Hashtbl.create 8

let elab paths_spec =
  match Hashtbl.find_opt cache_elab paths_spec with
  | Some spec -> spec
  | None ->
      let spec = paths_spec |> parse |> Elaborate.Elab.elab_spec in
      Hashtbl.replace cache_elab paths_spec spec;
      spec

(* Algorithmic conversion *)

let cache_algo = Hashtbl.create 8

let algo paths_spec =
  match Hashtbl.find_opt cache_algo paths_spec with
  | Some spec -> spec
  | None ->
      let spec = paths_spec |> elab |> Algo.algo_spec in
      Hashtbl.replace cache_algo paths_spec spec;
      spec

(* Structuring *)

let structure_cache = Hashtbl.create 8

let structure ~(final : bool) paths_spec =
  match Hashtbl.find_opt structure_cache (final, paths_spec) with
  | Some spec -> spec
  | None ->
      let spec = paths_spec |> algo |> Structure.Struct.struct_spec ~final in
      Hashtbl.replace structure_cache (final, paths_spec) spec;
      spec

(* Annotation (prose) generation *)

let annotate paths_spec =
  paths_spec |> structure ~final:false |> Annotate.annotate_spec
