module Value = Runtime.Value

let parse_files (filenames_spec : string list) : Value.t =
  let spec_il =
    filenames_spec
    |> List.concat_map Frontend.Parse.parse_file
    |> Pass.Elaborate.Elab.elab_spec
  in
  Boot.boot_specIL spec_il

let parse_string (_filename : string) (str : string) : Value.t =
  let spec_il =
    str |> Frontend.Parse.parse_string |> Pass.Elaborate.Elab.elab_spec
  in
  Boot.boot_specIL spec_il
