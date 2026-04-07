open Pass
module Sim = Runtime.Sim.Simulator
module Filesys = Util.Filesys
open Util.Source

let version = "0.1"

(* Statistics *)

type stat = { durations : float list; exclude_run : int; fail_run : int }

let empty_stat = { durations = []; exclude_run = 0; fail_run = 0 }

let merge_stat (stat_a, log_a) (stat_b, log_b) =
  ({
    durations = stat_a.durations @ stat_b.durations;
    exclude_run = stat_a.exclude_run + stat_b.exclude_run;
    fail_run = stat_a.fail_run + stat_b.fail_run;
  }, log_a ^ "\n" ^ log_b)

let log_stat name stat total : unit =
  let excludes = stat.exclude_run in
  let fails = stat.fail_run in
  let passes = total - excludes - fails in
  let exclude_rate = float_of_int excludes /. float_of_int total *. 100.0 in
  let pass_rate = float_of_int passes /. float_of_int total *. 100.0 in
  let fail_rate = float_of_int fails /. float_of_int total *. 100.0 in
  let durations = List.sort compare stat.durations in
  let duration_total = List.fold_left ( +. ) 0.0 durations in
  let duration_avg = duration_total /. float_of_int total in
  let duration_max = durations |> List.rev |> List.hd in
  let duration_min = durations |> List.hd in
  Format.asprintf
    "%s: [EXCLUDE] %d/%d (%.2f%%) [PASS] %d/%d (%.2f%%) [FAIL] %d/%d (%.2f%%)"
    name excludes total exclude_rate passes total pass_rate fails total
    fail_rate
  |> print_endline;
  Format.eprintf "%s: [TOTAL] %.6f [AVG] %.6f [MAX] %.6f [MIN] %.6f\n" name
    duration_total duration_avg duration_max duration_min

(* Exceptions *)

exception TestRunErr of string * region * float
exception TestRunNegErr of float
exception TestParseFileErr of string * region * float
exception TestParseStringErr of string * region * float
exception TestParseRoundtripErr of string * float
exception TestUnknownErr of float

(* Timer *)

let start () = Util.Time.now ()
let stop start = Util.Time.now () -. start

(* Operations *)

let frontend specdir =
  specdir
  |> Filesys.collect_files ~suffix:".watsup"
  |> List.concat_map Frontend.Parse.parse_file

let elab specdir = specdir |> frontend |> Elaborate.Elab.elab_spec
let structure specdir = specdir |> elab |> Structure.Struct.struct_spec
let prosify specdir = specdir |> structure |> Prose.Prosify.prosify_spec

let driver ?(det = false) ?(arch : string option) mode specdir =
  let spec_sim =
    match mode with
    | `IL ->
        let spec_il = elab specdir in
        (Runtime.Sim.Simulator.IL spec_il : Runtime.Sim.Simulator.spec)
    | `SL ->
        let spec_sl = structure specdir in
        (Runtime.Sim.Simulator.SL spec_sl : Runtime.Sim.Simulator.spec)
  in
  let (module Driver) =
    match arch with
    | Some arch -> Backend_sim.Gen.gen arch
    | None -> Backend_sim.Gen.gen_placeholder ()
  in
  Driver.init ~det spec_sim;
  (spec_sim, (module Driver : Runtime.Sim.Simulator.DRIVER))

let run_with_instr (module Driver : Runtime.Sim.Simulator.DRIVER) spec_sim
    relname includes_p4 filename_p4 =
  let (module IH : Inst.Handler.HANDLER), read_coverage_instr =
    Inst.Coverage_instr.make ()
  in
  Inst.Hook.register [ (module IH : Inst.Handler.HANDLER) ];
  Inst.Hook.init_spec spec_sim;
  let result = Driver.run_program relname includes_p4 filename_p4 in
  Inst.Hook.finish ();
  let cover = read_coverage_instr () in
  (result, cover)

let run_with_dangling (module Driver : Runtime.Sim.Simulator.DRIVER) spec_sim
    relname includes_p4 filename_p4 =
  let (module DH : Inst.Handler.HANDLER), read_coverage_dangling =
    Inst.Coverage_dangling.make ()
  in
  Inst.Hook.register [ (module DH : Inst.Handler.HANDLER) ];
  Inst.Hook.init_spec spec_sim;
  let result = Driver.run_program relname includes_p4 filename_p4 in
  Inst.Hook.finish ();
  let cover = read_coverage_dangling () in
  (result, cover)

let sim_with_instr (module Driver : Runtime.Sim.Simulator.DRIVER) spec_sim
    includes_p4 filename_p4 filename_stf =
  let (module IH : Inst.Handler.HANDLER), read_coverage_instr =
    Inst.Coverage_instr.make ()
  in
  Inst.Hook.register [ (module IH : Inst.Handler.HANDLER) ];
  Inst.Hook.init_spec spec_sim;
  let result = Driver.run_stf_test Format.std_formatter includes_p4 filename_p4 filename_stf in
  Inst.Hook.finish ();
  let cover = read_coverage_instr () in
  (result, cover)

let sim_with_dangling (module Driver : Runtime.Sim.Simulator.DRIVER) spec_sim
    includes_p4 filename_p4 filename_stf =
  let (module DH : Inst.Handler.HANDLER), read_coverage_dangling =
    Inst.Coverage_dangling.make ()
  in
  Inst.Hook.register [ (module DH : Inst.Handler.HANDLER) ];
  Inst.Hook.init_spec spec_sim;
  let result = Driver.run_stf_test Format.std_formatter includes_p4 filename_p4 filename_stf in
  Inst.Hook.finish ();
  let cover = read_coverage_dangling () in
  (result, cover)

let cover_run_instr ?(arch : string option) mode filenames_spec relname
    includes_p4 filenames_p4 =
  let spec_sim, (module Driver) = driver ?arch mode filenames_spec in
  let spec_sl =
    match spec_sim with
    | Runtime.Sim.Simulator.SL spec_sl -> spec_sl
    | _ -> assert false
  in
  let cover_multi = Coverage.Instr.Multi.init spec_sl in
  let cover_multi =
    List.fold_left
      (fun cover_multi filename_p4 ->
        let _, cover_single =
          run_with_instr
            (module Driver)
            spec_sim relname includes_p4 filename_p4
        in
        Coverage.Instr.Multi.extend cover_multi filename_p4 cover_single)
      cover_multi filenames_p4
  in
  Coverage.Instr.Log.log_spec ~filename_cov_opt:None cover_multi spec_sl

let cover_run_dangling ?(arch : string option) mode filenames_spec relname
    includes_p4 filenames_p4 =
  let spec_sim, (module Driver) = driver ?arch mode filenames_spec in
  let spec_sl =
    match spec_sim with
    | Runtime.Sim.Simulator.SL spec_sl -> spec_sl
    | _ -> assert false
  in
  let cover_multi = Coverage.Dangling.Multi.init spec_sl in
  let cover_multi =
    List.fold_left
      (fun cover_multi filename_p4 ->
        let program_result, cover_single =
          run_with_dangling
            (module Driver)
            spec_sim relname includes_p4 filename_p4
        in
        let wellformed, welltyped =
          match program_result with
          | Pass _ -> (true, true)
          | Fail (`Syntax _) -> (true, false)
          | Fail (`Runtime _) -> (false, false)
        in
        Coverage.Dangling.Multi.extend cover_multi filename_p4 wellformed
          welltyped cover_single)
      cover_multi filenames_p4
  in
  Coverage.Dangling.Multi.log ~filename_cov_opt:None cover_multi

let cover_sim_instr ?(arch : string option) mode filenames_spec includes_p4
    filenames_p4 filenames_stf =
  let spec_sim, (module Driver) = driver ?arch mode filenames_spec in
  let spec_sl =
    match spec_sim with
    | Runtime.Sim.Simulator.SL spec_sl -> spec_sl
    | _ -> assert false
  in
  let cover_multi = Coverage.Instr.Multi.init spec_sl in
  let cover_multi =
    List.fold_left2
      (fun cover_multi filename_p4 filename_stf ->
        let _, cover_single =
          sim_with_instr
            (module Driver)
            spec_sim includes_p4 filename_p4 filename_stf
        in
        Coverage.Instr.Multi.extend cover_multi filename_p4 cover_single)
      cover_multi filenames_p4 filenames_stf
  in
  Coverage.Instr.Log.log_spec ~filename_cov_opt:None cover_multi spec_sl

let cover_sim_dangling ?(arch : string option) mode filenames_spec includes_p4
    filenames_p4 filenames_stf =
  let spec_sim, (module Driver) = driver ?arch mode filenames_spec in
  let spec_sl =
    match spec_sim with
    | Runtime.Sim.Simulator.SL spec_sl -> spec_sl
    | _ -> assert false
  in
  let cover_multi = Coverage.Dangling.Multi.init spec_sl in
  let cover_multi =
    List.fold_left2
      (fun cover_multi filename_p4 filename_stf ->
        let program_result, cover_single =
          sim_with_dangling
            (module Driver)
            spec_sim includes_p4 filename_p4 filename_stf
        in
        let wellformed, welltyped =
          match program_result with
          | Pass -> (true, true)
          | Fail (`Syntax _) -> (true, false)
          | Fail (`Runtime _) -> (false, false)
        in
        Coverage.Dangling.Multi.extend cover_multi filename_p4 wellformed
          welltyped cover_single)
      cover_multi filenames_p4 filenames_stf
  in
  Coverage.Dangling.Multi.log ~filename_cov_opt:None cover_multi
