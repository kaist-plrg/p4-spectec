module Error = Error
module Run = Runtime.Dynamic_Runner.Signature
module Sim = Runtime.Sim.Signature
module Config = Backend_boot.Config
module Boot_build = Backend_boot.Build

type 'a result = ('a, Error.t) Stdlib.result

let ( let* ) = Result.bind

(* Spec transformations *)

let parse (paths_spec : string list) : Lang.El.spec result =
  Pass.parse paths_spec |> Result.map_error (fun e -> Error.PassError e)

let elab (paths_spec : string list) : Lang.Il.spec result =
  Pass.elab paths_spec |> Result.map_error (fun e -> Error.PassError e)

let algo (paths_spec : string list) : Lang.Al.spec result =
  Pass.algo paths_spec |> Result.map_error (fun e -> Error.PassError e)

let structure ~(final : bool) (paths_spec : string list) : Lang.Sl.spec result =
  Pass.structure ~final paths_spec
  |> Result.map_error (fun e -> Error.PassError e)

let annotate (paths_spec : string list) : Lang.Pl.spec result =
  Pass.annotate paths_spec |> Result.map_error (fun e -> Error.PassError e)

(* Document generation *)

let splice (paths_spec : string list) (path_pairs : (string * string) list) :
    unit result =
  let* spec_el = parse paths_spec in
  let* spec_pl = annotate paths_spec in
  Backend_splice.splice_files spec_el spec_pl path_pairs
  |> Result.map_error (fun error -> Error.SpliceError error)

let spec_of_mode (mode : Run.mode) (paths_spec : string list) : Run.spec result
    =
  Boot_build.spec_of_mode mode paths_spec
  |> Result.map_error (fun e -> Error.PassError e)

(* Simulator, for the P4 target *)

let build_sim ?(cache = true) ?(det = false) ?(guard = false)
    ?(arch : string option) (spec_sim : Sim.spec) : (module Sim.SIM) result =
  Backend_sim.Build.build ~cache ~det ~guard ?arch spec_sim
  |> Result.map_error (fun e -> Error.RunError e)

(* Runners, for the meta-circular interpreter *)

let build_null ?(cache = true) ?(det = false) ?(guard = false)
    (interface : Config.interface) (spec : Run.spec) :
    (module Run.RUNNER) result =
  Boot_build.build_null ~cache ~det ~guard interface spec
  |> Result.map_error (fun e -> Error.RunError e)

let tower_of_file (path_tower : string) (target : Config.target) :
    Config.tower result =
  try Ok (Config.tower_of_file path_tower target) with
  | Failure msg | Sys_error msg | Yojson.Json_error msg ->
      Error (Error.CommandError msg)
  | Yojson.Basic.Util.Type_error (msg, _) -> Error (Error.CommandError msg)

let build_tower ?(cache = true) ?(det = false) ?(guard = false)
    (tower : Config.tower) : (Run.spec * (module Run.RUNNER)) result =
  let* (specs : Boot_build.tower_specs) =
    Boot_build.specs_of_tower tower
    |> Result.map_error (fun e -> Error.PassError e)
  in
  let* booter =
    Boot_build.build_tower ~cache ~det ~guard specs
    |> Result.map_error (fun e -> Error.RunError e)
  in
  let _, spec_boot = specs.boot in
  Ok (spec_boot, booter)

(* Negative test generation *)

let fuzzer (fuel : int) (spec_sl : Lang.Sl.spec) (relname : string)
    (includes_p4 : string list) (gendir : string)
    (name_campaign : string option) (randseed : int option)
    (logmode : Backend_testgen_neg.Modes.logmode)
    (bootmode : Backend_testgen_neg.Modes.bootmode)
    (mutationmode : Backend_testgen_neg.Modes.mutationmode)
    (covermode : Backend_testgen_neg.Modes.covermode) : unit result =
  Backend_testgen_neg.Gen.fuzzer fuel spec_sl relname includes_p4 gendir
    name_campaign randseed logmode bootmode mutationmode covermode
  |> Result.map_error (fun e -> Error.RunError e)

let debug_dangling (spec_sl : Lang.Sl.spec) (relname : string)
    (includes_p4 : string list) (path_p4 : string) (debugdir : string)
    (iid : int) : unit result =
  Backend_testgen_neg.Derive.debug_dangling spec_sl relname includes_p4 path_p4
    debugdir iid
  |> Result.map_error (fun e -> Error.RunError e)
