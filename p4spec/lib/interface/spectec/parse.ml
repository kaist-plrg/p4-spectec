open Lang

let parse_files (filenames_spec : string list) : Il.spec =
  filenames_spec
  |> List.concat_map Frontend.Parse.parse_file
  |> Pass.Elaborate.Elab.elab_spec
