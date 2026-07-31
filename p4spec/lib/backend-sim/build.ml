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
    ?(arch : string option) ~(final : bool) (mode : Sim.mode)
    (paths_spec : string list) =
  let spec_sim =
    match mode with
    | AL_mode ->
        let spec_al = Pass.algo paths_spec in
        (AL spec_al : Sim.spec)
    | SL_mode ->
        let spec_sl = Pass.structure ~final paths_spec in
        (SL spec_sl : Sim.spec)
    | PL_mode ->
        let spec_pl = Pass.annotate paths_spec in
        (PL spec_pl : Sim.spec)
    | Empty_mode -> assert false
  in
  let (module Simulator) =
    match arch with Some arch -> gen_p4 arch | None -> gen_p4_placeholder ()
  in
  Simulator.init ~cache ~det ~guard spec_sim;
  (spec_sim, (module Simulator : Sim.SIM))
