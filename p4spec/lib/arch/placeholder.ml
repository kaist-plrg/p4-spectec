open Interface.Wrap
module Value = Runtime_dynamic.Value
module IO = Runtime_simulator.Io
module Sim = Runtime_simulator.Simulator
open Error

module Make (Interp_IL : Sim.INTERP_IL) (Interp_SL : Sim.INTERP_SL) : Sim.ARCH =
struct
  let eval_extern_init (_values_input : Value.t list) : Value.t =
    wrap_extern_v "externState" `Null

  let eval_extern_func_call (_values_input : Value.t list) : Value.t list =
    error_no_region
      "eval_extern_func_call not implemented for the placeholder simulator"

  let eval_extern_method_call (_values_input : Value.t list) : Value.t list =
    error_no_region
      "eval_extern_method_call not implemented for the placeholder simulator"

  let init_pipe (_spec : Sim.spec) (_includes_p4 : string list)
      (_filename_p4 : string) : Value.t * Value.t =
    error_no_region "init_pipe not implemented for the placeholder simulator"

  let drive_pipe (_value_ctx : Value.t) (_value_sto : Value.t) (_rx : IO.rx) :
      Value.t * Value.t * IO.tx option =
    error_no_region "drive_pipe not implemented for the placeholder simulator"
end
