module Run = Runtime.Dynamic_Runner.Signature
open Error

(* Building a tower *)

let build_target ?(cache = true) ?(det = false) ?(guard = false)
    (level : Config.level) =
  (* Create the target runner *)
  let (module Runner_target) =
    match level.interface with
    | P4_interface -> (module P4.Make () : Run.RUNNER)
    | IL_interface ->
        let module Interface_SpecTec = Interface.SpecTec_IL in
        (module Runner.Make.Make_rec
                  (Interface_SpecTec)
                  (Spectec.Make_null (Interface_SpecTec))
                  (Interp_il.Interp.Make)
                  (Interp_sl.Interp.Make) : Run.RUNNER)
    | SL_interface ->
        let module Interface_SpecTec = Interface.SpecTec_SL in
        (module Runner.Make.Make_rec
                  (Interface_SpecTec)
                  (Spectec.Make_null (Interface_SpecTec))
                  (Interp_il.Interp.Make)
                  (Interp_sl.Interp.Make) : Run.RUNNER)
  in
  (* Initialize the target runner, as an SL spec *)
  let spec =
    let spec_sl = Pass.structure ~final:true [ level.layer.specdir ] in
    (SL spec_sl : Run.spec)
  in
  Runner_target.init ~cache ~det ~guard spec;
  (module Runner_target : Run.RUNNER)

let build_interm ?(cache = true) ?(det = false) ?(guard = false)
    (module Runner_above : Run.RUNNER) (level : Config.level) =
  (* Create the intermediate runner *)
  let (module Interface_SpecTec) =
    match level.interface with
    | P4_interface ->
        error_no_region "P4 interface not supported outside of target level"
    | IL_interface -> (module Interface.SpecTec_IL : Spectec.INTERFACE_SPECTEC)
    | SL_interface -> (module Interface.SpecTec_SL : Spectec.INTERFACE_SPECTEC)
  in
  let (module Runner) =
    (module Runner.Make.Make_nonrec
              (Interface_SpecTec)
              (Spectec.Make_parametric (Runner_above) (Interface_SpecTec))
              (Interp_il.Interp.Make)
              (Interp_sl.Interp.Make) : Run.RUNNER)
  in
  (* Initialize the runner, as an SL spec *)
  let spec =
    let spec_sl = Pass.structure ~final:true [ level.layer.specdir ] in
    (SL spec_sl : Run.spec)
  in
  Runner.init ~cache ~det ~guard spec;
  (module Runner : Run.RUNNER)

let build_boot ?(cache = true) ?(det = false) ?(guard = false)
    (module Runner_above : Run.RUNNER) (mode : Run.mode) (level : Config.level)
    =
  (* Create the booter *)
  let (module Interface_SpecTec) =
    match level.interface with
    | P4_interface ->
        error_no_region "P4 interface not supported outside of target level"
    | IL_interface -> (module Interface.SpecTec_IL : Spectec.INTERFACE_SPECTEC)
    | SL_interface -> (module Interface.SpecTec_SL : Spectec.INTERFACE_SPECTEC)
  in
  let (module Booter) =
    (module Runner.Make.Make_nonrec
              (Interface_SpecTec)
              (Spectec.Make_parametric (Runner_above) (Interface_SpecTec))
              (Interp_il.Interp.Make)
              (Interp_sl.Interp.Make) : Run.RUNNER)
  in
  (* Initialize the booter, as mode *)
  let spec =
    match mode with
    | IL_mode ->
        let spec_il = Pass.elab [ level.layer.specdir ] in
        (IL spec_il : Run.spec)
    | SL_mode ->
        let spec_sl = Pass.structure ~final:true [ level.layer.specdir ] in
        (SL spec_sl : Run.spec)
    | Empty_mode -> assert false
  in
  Booter.init ~cache ~det ~guard spec;
  (spec, (module Booter : Run.RUNNER))

let build_tower ?(cache = true) ?(det = false) ?(guard = false)
    (tower : Config.tower) =
  (* Build the target runner *)
  let runner_target = build_target ~cache ~det ~guard tower.level_target in
  (* Reverse the levels, so that we build levels from the target to boot *)
  let levels = tower.level_boot :: tower.levels_interm |> List.rev in
  let spec_boot = ref None in
  let booter, runners_interm =
    levels
    |> List.mapi (fun idx level ->
           if idx = List.length levels - 1 then (true, level) else (false, level))
    |> List.fold_left
         (fun ((module Runner_above : Run.RUNNER), runners) (last, level) ->
           if not last then
             let runner_interm =
               build_interm ~cache ~det ~guard (module Runner_above) level
             in
             (runner_interm, runner_interm :: runners)
           else
             let spec, booter =
               build_boot ~cache ~det ~guard
                 (module Runner_above)
                 tower.mode level
             in
             spec_boot := Some spec;
             (booter, runners))
         (runner_target, [])
  in
  let spec_boot =
    match !spec_boot with Some spec -> spec | None -> assert false
  in
  (spec_boot, runner_target, runners_interm, booter)

let build_null ?(cache = true) ?(det = false) ?(guard = false) (mode : Run.mode)
    (interface : Config.interface) (paths_spec : string list) =
  let (module Interface_SpecTec) =
    match interface with
    | P4_interface ->
        error_no_region "P4 interface not supported outside of target level"
    | IL_interface -> (module Interface.SpecTec_IL : Spectec.INTERFACE_SPECTEC)
    | SL_interface -> (module Interface.SpecTec_SL : Spectec.INTERFACE_SPECTEC)
  in
  let (module Runner) =
    (module Runner.Make.Make_rec
              (Interface_SpecTec)
              (Spectec.Make_null (Interface_SpecTec))
              (Interp_il.Interp.Make)
              (Interp_sl.Interp.Make) : Run.RUNNER)
  in
  let spec =
    match mode with
    | IL_mode ->
        let spec_il = Pass.elab paths_spec in
        (IL spec_il : Run.spec)
    | SL_mode ->
        let spec_sl = Pass.structure ~final:true paths_spec in
        (SL spec_sl : Run.spec)
    | Empty_mode -> assert false
  in
  Runner.init ~cache ~det ~guard spec;
  (spec, (module Runner : Run.RUNNER))
