module Run = Runtime.Dynamic_Runner.Signature
open Error

(* Specs *)

let structured_spec (paths_spec : string list) : Run.spec =
  let spec_sl = Pass.structure ~final:true paths_spec in
  (SL spec_sl : Run.spec)

let spec_of_mode (mode : Run.mode) (paths_spec : string list) : Run.spec =
  match mode with
  | AL_mode ->
      let spec_al = Pass.algo paths_spec in
      (AL spec_al : Run.spec)
  | SL_mode ->
      let spec_sl = Pass.structure ~final:true paths_spec in
      (SL spec_sl : Run.spec)
  | PL_mode ->
      let spec_pl = Pass.annotate paths_spec in
      (PL spec_pl : Run.spec)
  | Empty_mode -> assert false

(* Building a tower *)

let build_target ?(cache = true) ?(det = false) ?(guard = false)
    (level : Config.level) (spec : Run.spec) =
  (* Create the target runner *)
  let (module Runner_target) =
    match level.interface with
    | P4_interface -> (module P4.Make () : Run.RUNNER)
    | AL_interface ->
        let module Interface_SpecTec = Interface.SpecTec_AL in
        (module Runner.Make.Make_rec
                  (Interface_SpecTec)
                  (Spectec.Make_null (Interface_SpecTec))
                  (Interp_al.Interp.Make)
                  (Interp_sl.Interp.Make)
                  (Interp_pl.Interp.Make) : Run.RUNNER)
    | SL_interface ->
        let module Interface_SpecTec = Interface.SpecTec_SL in
        (module Runner.Make.Make_rec
                  (Interface_SpecTec)
                  (Spectec.Make_null (Interface_SpecTec))
                  (Interp_al.Interp.Make)
                  (Interp_sl.Interp.Make)
                  (Interp_pl.Interp.Make) : Run.RUNNER)
  in
  Runner_target.init ~cache ~det ~guard spec;
  (module Runner_target : Run.RUNNER)

let build_interm ?(cache = true) ?(det = false) ?(guard = false)
    (module Runner_above : Run.RUNNER) (level : Config.level) (spec : Run.spec)
    =
  (* Create the intermediate runner *)
  let (module Interface_SpecTec) =
    match level.interface with
    | P4_interface ->
        error_no_region "P4 interface not supported outside of target level"
    | AL_interface -> (module Interface.SpecTec_AL : Spectec.INTERFACE_SPECTEC)
    | SL_interface -> (module Interface.SpecTec_SL : Spectec.INTERFACE_SPECTEC)
  in
  let (module Runner) =
    (module Runner.Make.Make_nonrec
              (Interface_SpecTec)
              (Spectec.Make_parametric (Runner_above) (Interface_SpecTec))
              (Interp_al.Interp.Make)
              (Interp_sl.Interp.Make)
              (Interp_pl.Interp.Make) : Run.RUNNER)
  in
  Runner.init ~cache ~det ~guard spec;
  (module Runner : Run.RUNNER)

let build_boot ?(cache = true) ?(det = false) ?(guard = false)
    (module Runner_above : Run.RUNNER) (level : Config.level) (spec : Run.spec)
    =
  (* Create the booter *)
  let (module Interface_SpecTec) =
    match level.interface with
    | P4_interface ->
        error_no_region "P4 interface not supported outside of target level"
    | AL_interface -> (module Interface.SpecTec_AL : Spectec.INTERFACE_SPECTEC)
    | SL_interface -> (module Interface.SpecTec_SL : Spectec.INTERFACE_SPECTEC)
  in
  let (module Booter) =
    (module Runner.Make.Make_nonrec
              (Interface_SpecTec)
              (Spectec.Make_parametric (Runner_above) (Interface_SpecTec))
              (Interp_al.Interp.Make)
              (Interp_sl.Interp.Make)
              (Interp_pl.Interp.Make) : Run.RUNNER)
  in
  Booter.init ~cache ~det ~guard spec;
  (module Booter : Run.RUNNER)

let build_tower ?(cache = true) ?(det = false) ?(guard = false)
    (tower : Config.tower) =
  (* Build the target runner *)
  let spec_target = structured_spec [ tower.level_target.layer.specdir ] in
  let runner_target =
    build_target ~cache ~det ~guard tower.level_target spec_target
  in
  (* Reverse the levels, so that we build levels from the target to boot *)
  let levels = tower.level_boot :: tower.levels_interm |> List.rev in
  let n = List.length levels in
  let level_specs =
    levels
    |> List.mapi (fun idx level -> (idx = n - 1, level))
    |> List.map (fun (is_boot, (level : Config.level)) ->
           let spec =
             if is_boot then spec_of_mode tower.mode [ level.layer.specdir ]
             else structured_spec [ level.layer.specdir ]
           in
           (is_boot, level, spec))
  in
  let spec_boot = ref None in
  let booter, runners_interm =
    level_specs
    |> List.fold_left
         (fun ((module Runner_above : Run.RUNNER), runners)
              (is_boot, level, spec) ->
           if not is_boot then
             let runner_interm =
               build_interm ~cache ~det ~guard (module Runner_above) level spec
             in
             (runner_interm, runner_interm :: runners)
           else
             let booter =
               build_boot ~cache ~det ~guard (module Runner_above) level spec
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
    | AL_interface -> (module Interface.SpecTec_AL : Spectec.INTERFACE_SPECTEC)
    | SL_interface -> (module Interface.SpecTec_SL : Spectec.INTERFACE_SPECTEC)
  in
  let (module Runner) =
    (module Runner.Make.Make_rec
              (Interface_SpecTec)
              (Spectec.Make_null (Interface_SpecTec))
              (Interp_al.Interp.Make)
              (Interp_sl.Interp.Make)
              (Interp_pl.Interp.Make) : Run.RUNNER)
  in
  let spec = spec_of_mode mode paths_spec in
  Runner.init ~cache ~det ~guard spec;
  (spec, (module Runner : Run.RUNNER))
