module Value = Runtime.Value
module Run = Runtime.Dynamic_Runner.Signature

let parse_files (mode : Run.mode) (filenames_spec : string list) : Value.t =
  let spec_el = filenames_spec |> List.concat_map Frontend.Parse.parse_file in
  let spec_il = spec_el |> Pass.Elaborate.Elab.elab_spec in
  match mode with
  | IL_mode -> spec_il |> Ili.Boot.boot_specIL
  | SL_mode ->
      spec_il
      |> Pass.Structure.Struct.struct_spec ~final:true
      |> Sli.Boot.boot_specSL
  | Empty_mode -> assert false

let parse_string (mode : Run.mode) (_filename : string) (str : string) : Value.t
    =
  let spec_el = str |> Frontend.Parse.parse_string in
  let spec_il = spec_el |> Pass.Elaborate.Elab.elab_spec in
  match mode with
  | IL_mode -> spec_il |> Ili.Boot.boot_specIL
  | SL_mode ->
      spec_il
      |> Pass.Structure.Struct.struct_spec ~final:true
      |> Sli.Boot.boot_specSL
  | Empty_mode -> assert false
