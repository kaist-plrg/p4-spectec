module Elaborate = Elaborate
module Structure = Structure
module Prose = Prose
module Compile = Compile

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

(* Elaboration *)

let elab paths_spec = paths_spec |> parse |> Elaborate.Elab.elab_spec

(* Structuring *)

let structure ~(final : bool) paths_spec =
  paths_spec |> elab |> Structure.Struct.struct_spec ~final

(* Prose generation *)

let prosify paths_spec =
  paths_spec |> structure ~final:false |> Prose.Prosify.prosify_spec

(* Compilation *)

let compile paths_spec path_out =
  paths_spec |> structure ~final:true |> Compile.Codegen.compile_spec path_out
