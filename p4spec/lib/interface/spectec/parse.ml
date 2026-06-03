module Value = Runtime.Value
module Run = Runtime.Dynamic_Runner.Signature

let parse_files (mode : Run.mode) (paths_spec : string list) : Value.t =
  match mode with
  | IL_mode -> paths_spec |> Pass.elab |> Ili.Boot.boot_spec
  | SL_mode -> paths_spec |> Pass.structure ~final:true |> Sli.Boot.boot_spec
  | ML_mode -> paths_spec |> Pass.structure ~final:true |> Sli.Boot.boot_spec
  | Empty_mode -> assert false

let parse_string (mode : Run.mode) (_path : string) (str : string) : Value.t =
  match mode with
  | IL_mode ->
      str |> Frontend.Parse.parse_string |> Pass.Elaborate.Elab.elab_spec
      |> Ili.Boot.boot_spec
  | SL_mode ->
      str |> Frontend.Parse.parse_string |> Pass.Elaborate.Elab.elab_spec
      |> Pass.Structure.Struct.struct_spec ~final:true
      |> Sli.Boot.boot_spec
  | ML_mode ->
      str |> Frontend.Parse.parse_string |> Pass.Elaborate.Elab.elab_spec
      |> Pass.Structure.Struct.struct_spec ~final:true
      |> Sli.Boot.boot_spec
  | Empty_mode -> assert false
