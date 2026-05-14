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

let build ?(cache = true) ?(det = false) ?(guard = false)
    ?(arch : string option) ~(final : bool) (mode : Sim.mode)
    (paths_spec : string list) =
  let spec_sim =
    match mode with
    | IL_mode ->
        let spec_il = Pass.elab paths_spec in
        (IL spec_il : Sim.spec)
    | SL_mode ->
        let spec_sl = Pass.structure ~final paths_spec in
        (SL spec_sl : Sim.spec)
    | Empty_mode -> assert false
  in
  let (module Simulator) =
    match arch with Some arch -> gen_p4 arch | None -> gen_p4_placeholder ()
  in
  Simulator.init ~cache ~det ~guard spec_sim;
  (spec_sim, (module Simulator : Sim.SIM))
