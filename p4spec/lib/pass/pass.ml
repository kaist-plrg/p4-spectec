(* Errors *)

type error =
  | ParseError of Frontend.Parse.error
  | ElabError of Elaborate.error
  | AlgoError of Algo.error
  | StructError of Structure.error
  | ProseError of Annotate.error

let to_region_msg = function
  | ParseError e -> Frontend.Parse.to_region_msg e
  | ElabError e -> Elaborate.to_region_msg e
  | AlgoError e -> Algo.to_region_msg e
  | StructError e -> Structure.to_region_msg e
  | ProseError e -> Annotate.to_region_msg e

let ( let* ) = Result.bind

(* Stages *)

let parse_string str =
  Frontend.Parse.parse_string str |> Result.map_error (fun e -> ParseError e)

let parse_files paths =
  Frontend.Parse.parse_files paths |> Result.map_error (fun e -> ParseError e)

let elab_spec spec_el =
  Elaborate.elab_spec spec_el |> Result.map_error (fun e -> ElabError e)

let algo_spec spec_il =
  Algo.algo_spec spec_il |> Result.map_error (fun e -> AlgoError e)

let struct_spec ~final spec_al =
  Structure.struct_spec ~final spec_al
  |> Result.map_error (fun e -> StructError e)

let annotate_spec spec_sl =
  Annotate.annotate_spec spec_sl |> Result.map_error (fun e -> ProseError e)

(* File expansion *)

let expand_spec filenames =
  List.concat_map
    (fun filename ->
      if Sys_unix.is_directory_exn filename then
        Util.Filesys.collect_files ~suffix:".watsup" filename
      else [ filename ])
    filenames

(* Parsing *)

let parse paths_spec = paths_spec |> expand_spec |> parse_files

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
