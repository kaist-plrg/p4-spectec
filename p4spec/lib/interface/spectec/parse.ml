module Value = Runtime.Value
module Run = Runtime.Dynamic_Runner.Signature

(* Boot at [V_value] rep (IL/SL) and [V_native] rep (compiled ML).
   SL needs [V_native]; IL never runs under [ML_mode]. *)

module Boot_ili_value = Ili.Boot.Make (Runtime.Valrep.V_value)
module Boot_sli_value = Sli.Boot.Make (Runtime.Valrep.V_value)
module Boot_sli_native = Sli.Boot.Make (Backend_ocaml.Val_native.V_native)

let parse_files (mode : Run.mode) (paths_spec : string list) : Value.t =
  match mode with
  | IL_mode -> paths_spec |> Pass.elab |> Boot_ili_value.boot_spec
  | SL_mode ->
      paths_spec |> Pass.structure ~final:true |> Boot_sli_value.boot_spec
  | ML_mode ->
      (Obj.magic
         (paths_spec |> Pass.structure ~final:true
        |> Boot_sli_native.boot_spec)
        : Value.t)
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
      (Obj.magic
         (str |> Frontend.Parse.parse_string |> Pass.Elaborate.Elab.elab_spec
        |> Pass.Structure.Struct.struct_spec ~final:true
        |> Boot_sli_native.boot_spec)
        : Value.t)
  | Empty_mode -> assert false
