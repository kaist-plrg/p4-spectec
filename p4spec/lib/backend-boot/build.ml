module Run = Runtime.Dynamic_Runner.Signature
open Error

(* ML interpreter functor selection *)

module type INTERP_ML = sig
  module Make : functor (_ : Run.INTERFACE) (_ : Run.EXTERN) () -> Run.INTERP_ML
end

let interp_ml_of_interface (interface : Config.interface) : (module INTERP_ML) =
  match interface with
  | Config.P4_interface -> (module Backend_ocaml.Interp_ml : INTERP_ML)
  | Config.IL_interface -> (module Backend_ocaml_il.Interp_ml : INTERP_ML)
  | Config.SL_interface -> (module Backend_ocaml_sl.Interp_ml : INTERP_ML)

(* The [Spectec.Make_null]/[Make_parametric] value representation: under
   [ML_mode] the compiled interface's own native [V], matching the
   representation compiled code actually reads/writes at the [Run.EXTERN]
   boundary; [Valrep.V_value] (the interpreted representation) otherwise. *)

let valrep_of_mode_interface (mode : Run.mode) (interface : Config.interface) :
    (module Runtime.Valrep.VAL) =
  match mode with
  | Run.ML_mode -> (
      match interface with
      | Config.P4_interface ->
          (module Backend_ocaml.Val_native.V_native : Runtime.Valrep.VAL)
      | Config.IL_interface ->
          (module Backend_ocaml_il.Val_native.V_native : Runtime.Valrep.VAL)
      | Config.SL_interface ->
          (module Backend_ocaml_sl.Val_native.V_native : Runtime.Valrep.VAL))
  | Run.IL_mode | Run.SL_mode | Run.Empty_mode ->
      (module Runtime.Valrep.V_value : Runtime.Valrep.VAL)

(* A [spec] value for [paths_spec]/[level], given an execution mode. [ML_mode]
   needs no parse/elaborate — the compiled code is already linked in. *)

let spec_of_paths (mode : Run.mode) (paths_spec : string list) : Run.spec =
  match mode with
  | IL_mode -> IL (Pass.elab paths_spec)
  | SL_mode -> SL (Pass.structure ~final:true paths_spec)
  | ML_mode -> ML
  | Empty_mode -> assert false

let spec_of_level (level : Config.level) : Run.spec =
  spec_of_paths level.mode [ level.layer.specdir ]

(* Building a tower *)

let build_target ?(cache = true) ?(det = false) ?(guard = false)
    (level : Config.level) =
  (* Create the target runner *)
  let (module Runner_target) =
    match level.interface with
    | P4_interface -> (module P4.Make () : Run.RUNNER)
    | IL_interface ->
        let (module V) = valrep_of_mode_interface level.mode IL_interface in
        let module Interface_SpecTec = Interface.SpecTec_IL.Make (V) in
        let (module Interp_ml) = interp_ml_of_interface IL_interface in
        (module Runner.Make.Make_rec
                  (Interface_SpecTec)
                  (Spectec.Make_null (V) (Interface_SpecTec))
                  (Interp_il.Interp.Make)
                  (Interp_sl.Interp.Make)
                  (Interp_ml.Make) : Run.RUNNER)
    | SL_interface ->
        let (module V) = valrep_of_mode_interface level.mode SL_interface in
        let module Interface_SpecTec = Interface.SpecTec_SL.Make (V) in
        let (module Interp_ml) = interp_ml_of_interface SL_interface in
        (module Runner.Make.Make_rec
                  (Interface_SpecTec)
                  (Spectec.Make_null (V) (Interface_SpecTec))
                  (Interp_il.Interp.Make)
                  (Interp_sl.Interp.Make)
                  (Interp_ml.Make) : Run.RUNNER)
  in
  Runner_target.init ~cache ~det ~guard (spec_of_level level);
  (module Runner_target : Run.RUNNER)

(* Build a non-target level (intermediate or boot) above [Runner_above],
   relaying extern calls to it via [Spectec.Make_parametric]. Returns the
   level's [spec] alongside its runner: the boot level's caller needs the
   spec (for [Inst.Hook.init_spec]), intermediate levels' callers discard it. *)

let build_level ?(cache = true) ?(det = false) ?(guard = false)
    (module Runner_above : Run.RUNNER) (level : Config.level) =
  let (module V) = valrep_of_mode_interface level.mode level.interface in
  let (module Interface_SpecTec) =
    match level.interface with
    | P4_interface ->
        error_no_region "P4 interface not supported outside of target level"
    | IL_interface ->
        (module Interface.SpecTec_IL.Make (V) : Spectec.INTERFACE_SPECTEC
          with type vt = V.t)
    | SL_interface ->
        (module Interface.SpecTec_SL.Make (V) : Spectec.INTERFACE_SPECTEC
          with type vt = V.t)
  in
  let (module Interp_ml) = interp_ml_of_interface level.interface in
  let (module Runner) =
    (module Runner.Make.Make_nonrec
              (Interface_SpecTec)
              (Spectec.Make_parametric (V) (Runner_above) (Interface_SpecTec))
              (Interp_il.Interp.Make)
              (Interp_sl.Interp.Make)
              (Interp_ml.Make) : Run.RUNNER)
  in
  let spec = spec_of_level level in
  Runner.init ~cache ~det ~guard spec;
  (spec, (module Runner : Run.RUNNER))

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
           let spec, (module Runner) =
             build_level ~cache ~det ~guard (module Runner_above) level
           in
           if last then (
             spec_boot := Some spec;
             ((module Runner : Run.RUNNER), runners))
           else
             ( (module Runner : Run.RUNNER),
               (module Runner : Run.RUNNER) :: runners ))
         (runner_target, [])
  in
  let spec_boot =
    match !spec_boot with Some spec -> spec | None -> assert false
  in
  (spec_boot, runner_target, runners_interm, booter)

let build_null ?(cache = true) ?(det = false) ?(guard = false) (mode : Run.mode)
    (interface : Config.interface) (paths_spec : string list) =
  let (module V) = valrep_of_mode_interface mode interface in
  let (module Interface_SpecTec) =
    match interface with
    | P4_interface ->
        error_no_region "P4 interface not supported outside of target level"
    | IL_interface ->
        (module Interface.SpecTec_IL.Make (V) : Spectec.INTERFACE_SPECTEC
          with type vt = V.t)
    | SL_interface ->
        (module Interface.SpecTec_SL.Make (V) : Spectec.INTERFACE_SPECTEC
          with type vt = V.t)
  in
  let (module Interp_ml) = interp_ml_of_interface interface in
  let (module Runner) =
    (module Runner.Make.Make_rec
              (Interface_SpecTec)
              (Spectec.Make_null (V) (Interface_SpecTec))
              (Interp_il.Interp.Make)
              (Interp_sl.Interp.Make)
              (Interp_ml.Make) : Run.RUNNER)
  in
  let spec = spec_of_paths mode paths_spec in
  Runner.init ~cache ~det ~guard spec;
  (spec, (module Runner : Run.RUNNER))
