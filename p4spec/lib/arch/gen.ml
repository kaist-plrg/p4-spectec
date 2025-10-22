module Sim = Runtime_simulator.Simulator
module Runner = Runtime_simulator.Runner
open Error

let gen arch =
  match arch with
  | "v1model" ->
      (module Runner.Make (V1model.Make) (Interp_sl_new.Interp.Make)
      : Sim.DRIVER)
  | _ ->
      Format.asprintf "architecture %s is not supported" arch |> error_no_region
