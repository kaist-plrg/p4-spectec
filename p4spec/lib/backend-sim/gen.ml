module Sim = Runtime.Sim.Simulator
open Error

let gen arch =
  match arch with
  | "v1model" ->
      (module Runner.Make (V1model.Pipe.Make) (Interp_il.Interp.Make)
                (Interp_sl.Interp.Make) : Sim.DRIVER)
  | "ebpf" ->
      (module Runner.Make (Ebpf.Pipe.Make) (Interp_il.Interp.Make)
                (Interp_sl.Interp.Make) : Sim.DRIVER)
  | _ ->
      Format.asprintf "architecture %s is not supported" arch |> error_no_region

let gen_placeholder () =
  (module Runner.Make (Placeholder.Make) (Interp_il.Interp.Make)
            (Interp_sl.Interp.Make) : Sim.DRIVER)
