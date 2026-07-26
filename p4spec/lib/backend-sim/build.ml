module Sim = Runtime.Sim.Signature
open Error

let gen_p4 arch =
  match arch with
  | "v1model" ->
      (module Make.Make (Interface.P4) (V1model.Pipe.Make)
                (Interp_al.Interp.Make)
                (Interp_sl.Interp.Make)
                (Interp_pl.Interp.Make) : Sim.SIM)
  | "ebpf" ->
      (module Make.Make (Interface.P4) (Ebpf.Pipe.Make) (Interp_al.Interp.Make)
                (Interp_sl.Interp.Make)
                (Interp_pl.Interp.Make) : Sim.SIM)
  | "psa" ->
      (module Make.Make (Interface.P4) (Psa.Pipe.Make) (Interp_al.Interp.Make)
                (Interp_sl.Interp.Make)
                (Interp_pl.Interp.Make) : Sim.SIM)
  | _ ->
      Format.asprintf "architecture %s is not supported" arch |> error_no_region

let gen_p4_placeholder () =
  (module Make.Make (Interface.P4) (Placeholder.Make) (Interp_al.Interp.Make)
            (Interp_sl.Interp.Make)
            (Interp_pl.Interp.Make) : Sim.SIM)

let build ?(cache = true) ?(det = false) ?(guard = false)
    ?(arch : string option) (spec_sim : Sim.spec) =
  let (module Simulator) =
    match arch with Some arch -> gen_p4 arch | None -> gen_p4_placeholder ()
  in
  Simulator.init ~cache ~det ~guard spec_sim;
  (module Simulator : Sim.SIM)
