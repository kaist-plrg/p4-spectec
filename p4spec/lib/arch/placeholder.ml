module IO = Runtime_simulator.Io
module Sim = Runtime_simulator.Simulator
open Error

module Make (Interp : Sim.INTERP) : Sim.ARCH = struct
  let eval_extern_func_call (_values_input : Sl.Ast.value list) :
      Sl.Ast.value list =
    error_no_region
      "eval_extern_func_call not implemented for the placeholder simulator"

  let eval_extern_method_call (_values_input : Sl.Ast.value list) :
      Sl.Ast.value list =
    error_no_region
      "eval_extern_method_call not implemented for the placeholder simulator"

  let init_pipe (_spec : Sl.Ast.spec) (_includes_p4 : string list)
      (_filename_p4 : string) : Sl.Ast.value * Sl.Ast.value =
    error_no_region "init_pipe not implemented for the placeholder simulator"

  let drive_pipe (_value_ctx : Sl.Ast.value) (_value_sto : Sl.Ast.value)
      (_rx : IO.rx) : Sl.Ast.value * Sl.Ast.value * IO.tx option =
    error_no_region "drive_pipe not implemented for the placeholder simulator"
end
