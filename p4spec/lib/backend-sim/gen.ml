module Sim = Runtime.Sim.Signature
open Error

let gen_p4 arch =
  match arch with
  | "v1model" ->
      (module Make.Make (Interface.P4) (V1model.Pipe.Make)
                (Interp_il.Interp.Make)
                (Interp_sl.Interp.Make) : Sim.SIM)
  | "ebpf" ->
      (module Make.Make (Interface.P4) (Ebpf.Pipe.Make) (Interp_il.Interp.Make)
                (Interp_sl.Interp.Make) : Sim.SIM)
  | "psa" ->
      (module Make.Make (Interface.P4) (Psa.Pipe.Make) (Interp_il.Interp.Make)
                (Interp_sl.Interp.Make) : Sim.SIM)
  | _ ->
      Format.asprintf "architecture %s is not supported" arch |> error_no_region

let gen_p4_placeholder () =
  (module Make.Make (Interface.P4) (Placeholder.Make) (Interp_il.Interp.Make)
            (Interp_sl.Interp.Make) : Sim.SIM)
