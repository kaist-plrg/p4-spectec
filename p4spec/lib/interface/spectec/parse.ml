module Value = Runtime.Value
module Run = Runtime.Dynamic_Runner.Signature

(* Boot functors instantiated at [V_value]; a later task adds [V_native]. *)

module Boot_ili_value = Ili.Boot.Make (Runtime.Valrep.V_value)
module Boot_sli_value = Sli.Boot.Make (Runtime.Valrep.V_value)

let parse_files (mode : Run.mode) (paths_spec : string list) : Value.t =
  match mode with
  | IL_mode -> paths_spec |> Pass.elab |> Boot_ili_value.boot_spec
  | SL_mode ->
      paths_spec |> Pass.structure ~final:true |> Boot_sli_value.boot_spec
  | ML_mode ->
      paths_spec |> Pass.structure ~final:true |> Boot_sli_value.boot_spec
  | Empty_mode -> assert false

let parse_string (mode : Run.mode) (_path : string) (str : string) : Value.t =
  match mode with
  | IL_mode ->
      str |> Frontend.Parse.parse_string |> Pass.Elaborate.Elab.elab_spec
      |> Boot_ili_value.boot_spec
  | SL_mode ->
      str |> Frontend.Parse.parse_string |> Pass.Elaborate.Elab.elab_spec
      |> Pass.Structure.Struct.struct_spec ~final:true
      |> Boot_sli_value.boot_spec
  | ML_mode ->
      str |> Frontend.Parse.parse_string |> Pass.Elaborate.Elab.elab_spec
      |> Pass.Structure.Struct.struct_spec ~final:true
      |> Boot_sli_value.boot_spec
  | Empty_mode -> assert false
