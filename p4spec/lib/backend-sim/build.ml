module Sim = Runtime.Sim.Signature
open Util.Source

let ( let* ) = Result.bind

let gen_p4 (arch : string) : ((module Sim.SIM), Sim.error) result =
  match arch with
  | "v1model" ->
      Ok
        (module Make.Make (Interface.P4) (V1model.Pipe.Make)
                  (Interp_al.Interp.Make)
                  (Interp_sl.Interp.Make)
                  (Interp_pl.Interp.Make) : Sim.SIM)
  | "ebpf" ->
      Ok
        (module Make.Make (Interface.P4) (Ebpf.Pipe.Make)
                  (Interp_al.Interp.Make)
                  (Interp_sl.Interp.Make)
                  (Interp_pl.Interp.Make) : Sim.SIM)
  | "psa" ->
      Ok
        (module Make.Make (Interface.P4) (Psa.Pipe.Make) (Interp_al.Interp.Make)
                  (Interp_sl.Interp.Make)
                  (Interp_pl.Interp.Make) : Sim.SIM)
  | _ ->
      Error
        {
          Sim.at = no_region;
          msg = Format.asprintf "architecture %s is not supported" arch;
        }

let gen_p4_placeholder () : (module Sim.SIM) =
  (module Make.Make (Interface.P4) (Placeholder.Make) (Interp_al.Interp.Make)
            (Interp_sl.Interp.Make)
            (Interp_pl.Interp.Make) : Sim.SIM)

let gen_nano () =
  (module Make.Make (Interface.NanoP4) (Nano_switch.Pipe.Make)
            (Interp_al.Interp.Make)
            (Interp_sl.Interp.Make)
            (Interp_pl.Interp.Make) : Sim.SIM)

let build ?(cache = true) ?(det = false) ?(guard = false)
    ?(arch : string option) (spec_sim : Sim.spec) :
    ((module Sim.SIM), Sim.error) result =
  let* simulator =
    match arch with
    | Some arch -> gen_p4 arch
    | None -> Ok (gen_p4_placeholder ())
  in
  let (module Simulator : Sim.SIM) = simulator in
  let* () = Simulator.init ~cache ~det ~guard spec_sim in
  Ok simulator

let build_nano ?(cache = true) ?(det = false) ?(guard = false) ~(final : bool)
    (mode : Sim.mode) (paths_spec : string list) =
  let unwrap_pass = function
    | Ok spec -> spec
    | Error e ->
        let at, msg = Pass.to_region_msg e in
        raise (Interp_common.Error.InterpError (at, msg))
  in
  let spec_sim =
    match mode with
    | AL_mode -> (AL (unwrap_pass (Pass.algo paths_spec)) : Sim.spec)
    | SL_mode ->
        (SL (unwrap_pass (Pass.structure ~final paths_spec)) : Sim.spec)
    | PL_mode -> (PL (unwrap_pass (Pass.annotate paths_spec)) : Sim.spec)
    | Empty_mode -> assert false
  in
  let (module Simulator) = gen_nano () in
  (match Simulator.init ~cache ~det ~guard spec_sim with
  | Ok () -> ()
  | Error { Sim.at; msg } -> raise (Interp_common.Error.InterpError (at, msg)));
  (spec_sim, (module Simulator : Sim.SIM))
