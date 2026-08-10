module Run = Runtime.Dynamic_Runner.Signature
open Util.Source

let ( let* ) = Result.bind

let rec map_result f = function
  | [] -> Ok []
  | x :: xs ->
      let* y = f x in
      let* ys = map_result f xs in
      Ok (y :: ys)

(* Specs *)

let structured_spec (paths_spec : string list) : (Run.spec, Pass.error) result =
  let* spec_sl = Pass.structure ~final:true paths_spec in
  Ok (SL spec_sl : Run.spec)

let spec_of_mode (mode : Run.mode) (paths_spec : string list) :
    (Run.spec, Pass.error) result =
  match mode with
  | AL_mode ->
      let* spec_al = Pass.algo paths_spec in
      Ok (AL spec_al : Run.spec)
  | SL_mode ->
      let* spec_sl = Pass.structure ~final:true paths_spec in
      Ok (SL spec_sl : Run.spec)
  | PL_mode ->
      let* spec_pl = Pass.annotate paths_spec in
      Ok (PL spec_pl : Run.spec)
  | Empty_mode -> assert false

(* The specs of a tower, paired with the level each one belongs to, ordered from
   the target down to the boot level *)

type tower_specs = {
  target : Config.level * Run.spec;
  interms : (Config.level * Run.spec) list;
  boot : Config.level * Run.spec;
}

let specs_of_tower (tower : Config.tower) : (tower_specs, Pass.error) result =
  let* spec_target = structured_spec [ tower.level_target.layer.specdir ] in
  let* specs_interm =
    tower.levels_interm |> List.rev
    |> map_result (fun (level : Config.level) ->
           let* spec = structured_spec [ level.layer.specdir ] in
           Ok (level, spec))
  in
  let* spec_boot = spec_of_mode tower.mode [ tower.level_boot.layer.specdir ] in
  Ok
    {
      target = (tower.level_target, spec_target);
      interms = specs_interm;
      boot = (tower.level_boot, spec_boot);
    }

(* The SpecTec interface backing a non-target level *)

let spectec_interface_of (interface : Config.interface) :
    ((module Spectec.INTERFACE_SPECTEC), Run.error) result =
  match interface with
  | P4_interface ->
      Error
        {
          Run.at = no_region;
          msg = "P4 interface not supported outside of target level";
        }
  | AL_interface -> Ok (module Interface.SpecTec_AL : Spectec.INTERFACE_SPECTEC)
  | SL_interface -> Ok (module Interface.SpecTec_SL : Spectec.INTERFACE_SPECTEC)

(* Building a tower *)

let build_target ?(cache = true) ?(det = false) ?(guard = false)
    (level : Config.level) (spec : Run.spec) :
    ((module Run.RUNNER), Run.error) result =
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
  let* () = Runner_target.init ~cache ~det ~guard spec in
  Ok (module Runner_target : Run.RUNNER)

let build_interm ?(cache = true) ?(det = false) ?(guard = false)
    (module Runner_above : Run.RUNNER) (level : Config.level) (spec : Run.spec)
    : ((module Run.RUNNER), Run.error) result =
  (* Create the intermediate runner *)
  let* (module Interface_SpecTec : Spectec.INTERFACE_SPECTEC) =
    spectec_interface_of level.interface
  in
  let (module Runner) =
    (module Runner.Make.Make_nonrec
              (Interface_SpecTec)
              (Spectec.Make_parametric (Runner_above) (Interface_SpecTec))
              (Interp_al.Interp.Make)
              (Interp_sl.Interp.Make)
              (Interp_pl.Interp.Make) : Run.RUNNER)
  in
  let* () = Runner.init ~cache ~det ~guard spec in
  Ok (module Runner : Run.RUNNER)

let build_boot ?(cache = true) ?(det = false) ?(guard = false)
    (module Runner_above : Run.RUNNER) (level : Config.level) (spec : Run.spec)
    : ((module Run.RUNNER), Run.error) result =
  (* Create the booter *)
  let* (module Interface_SpecTec : Spectec.INTERFACE_SPECTEC) =
    spectec_interface_of level.interface
  in
  let (module Booter) =
    (module Runner.Make.Make_nonrec
              (Interface_SpecTec)
              (Spectec.Make_parametric (Runner_above) (Interface_SpecTec))
              (Interp_al.Interp.Make)
              (Interp_sl.Interp.Make)
              (Interp_pl.Interp.Make) : Run.RUNNER)
  in
  let* () = Booter.init ~cache ~det ~guard spec in
  Ok (module Booter : Run.RUNNER)

let build_tower ?(cache = true) ?(det = false) ?(guard = false)
    (specs : tower_specs) : ((module Run.RUNNER), Run.error) result =
  (* Build the levels from the target down to the boot level *)
  let level_target, spec_target = specs.target in
  let* runner_target =
    build_target ~cache ~det ~guard level_target spec_target
  in
  let* runner_above =
    List.fold_left
      (fun acc (level, spec) ->
        let* runner_above = acc in
        let (module Runner_above : Run.RUNNER) = runner_above in
        build_interm ~cache ~det ~guard (module Runner_above) level spec)
      (Ok runner_target) specs.interms
  in
  let (module Runner_above : Run.RUNNER) = runner_above in
  let level_boot, spec_boot = specs.boot in
  build_boot ~cache ~det ~guard (module Runner_above) level_boot spec_boot

let build_null ?(cache = true) ?(det = false) ?(guard = false)
    (interface : Config.interface) (spec : Run.spec) :
    ((module Run.RUNNER), Run.error) result =
  let* (module Interface_SpecTec : Spectec.INTERFACE_SPECTEC) =
    spectec_interface_of interface
  in
  let (module Runner) =
    (module Runner.Make.Make_rec
              (Interface_SpecTec)
              (Spectec.Make_null (Interface_SpecTec))
              (Interp_al.Interp.Make)
              (Interp_sl.Interp.Make)
              (Interp_pl.Interp.Make) : Run.RUNNER)
  in
  let* () = Runner.init ~cache ~det ~guard spec in
  Ok (module Runner : Run.RUNNER)
