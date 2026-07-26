open Util.Source
module Elaborate = Elaborate
module Algo = Algo
module Structure = Structure
module Annotate = Annotate

(* Errors *)

type error = { at : region; msg : string }

let string_of_error { at; msg } = Util.Error.string_of_error at msg
let to_region_msg { at; msg } = (at, msg)
let ( let* ) = Result.bind

(* Surface a pass failure as an [error]. Anything else (bugs, Stdlib
   exceptions) propagates untouched. *)

let guard f =
  try Ok (f ())
  with
  | Util.Error.ParseError (at, msg)
  | Util.Error.ElabError (at, msg)
  | Util.Error.AlgoError (at, msg)
  | Util.Error.StructError (at, msg)
  | Util.Error.ProseError (at, msg)
  ->
    Error { at; msg }

(* File expansion *)

let expand_spec filenames =
  List.concat_map
    (fun filename ->
      if Sys_unix.is_directory_exn filename then
        Util.Filesys.collect_files ~suffix:".watsup" filename
      else [ filename ])
    filenames

(* Parsing *)

let parse paths_spec =
  guard (fun () ->
      paths_spec |> expand_spec |> List.concat_map Frontend.Parse.parse_file)

(* Elaboration *)

let cache_elab = Hashtbl.create 8

let elab paths_spec =
  match Hashtbl.find_opt cache_elab paths_spec with
  | Some spec -> Ok spec
  | None ->
      let* spec_el = parse paths_spec in
      let* spec_il = guard (fun () -> Elaborate.Elab.elab_spec spec_el) in
      Hashtbl.replace cache_elab paths_spec spec_il;
      Ok spec_il

(* Algorithmic conversion *)

let cache_algo = Hashtbl.create 8

let algo paths_spec =
  match Hashtbl.find_opt cache_algo paths_spec with
  | Some spec -> Ok spec
  | None ->
      let* spec_il = elab paths_spec in
      let* spec_al = guard (fun () -> Algo.algo_spec spec_il) in
      Hashtbl.replace cache_algo paths_spec spec_al;
      Ok spec_al

(* Structuring *)

let structure_cache = Hashtbl.create 8

let structure ~(final : bool) paths_spec =
  match Hashtbl.find_opt structure_cache (final, paths_spec) with
  | Some spec -> Ok spec
  | None ->
      let* spec_al = algo paths_spec in
      let* spec_sl =
        guard (fun () -> Structure.Struct.struct_spec ~final spec_al)
      in
      Hashtbl.replace structure_cache (final, paths_spec) spec_sl;
      Ok spec_sl

(* Annotation (prose) generation *)

let annotate paths_spec =
  let* spec_sl = structure ~final:false paths_spec in
  guard (fun () -> Annotate.annotate_spec spec_sl)
