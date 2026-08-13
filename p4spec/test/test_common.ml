open Runtime.Sim.Signature
module Error = P4spectec.Error
module Filesys = Util.Filesys
open Util.Source

let version = "0.1"
let ( let* ) = Result.bind

let build_sim ?cache ?det ?guard ?(arch : string option) mode paths_spec =
  match
    let* spec_sim = P4spectec.spec_of_mode mode paths_spec in
    let* simulator = P4spectec.build_sim ?cache ?det ?guard ?arch spec_sim in
    Ok (spec_sim, simulator)
  with
  | Ok (spec_sim, simulator) -> (spec_sim, simulator)
  | Error e -> failwith (Error.to_string e)

(* Statistics *)

type stat = {
  durations : float list;
  exclude_run : int;
  fail_run : int;
  patch_run : int;
  patch_exclude_run : int;
  patch_fail_run : int;
  exclude_by_subdir : (string * int) list;
}

let empty_stat =
  {
    durations = [];
    exclude_run = 0;
    fail_run = 0;
    patch_run = 0;
    patch_exclude_run = 0;
    patch_fail_run = 0;
    exclude_by_subdir = [];
  }

let log_stat name stat total : unit =
  let excludes = stat.exclude_run in
  let fails = stat.fail_run in
  let passes = total - excludes - fails in
  let patches = stat.patch_run in
  let exclude_rate = float_of_int excludes /. float_of_int total *. 100.0 in
  let pass_rate = float_of_int passes /. float_of_int total *. 100.0 in
  let fail_rate = float_of_int fails /. float_of_int total *. 100.0 in
  let patch_rate = float_of_int patches /. float_of_int total *. 100.0 in
  let patch_excludes = stat.patch_exclude_run in
  let patch_fails = stat.patch_fail_run in
  let patch_passes = patches - patch_excludes - patch_fails in
  let patch_exclude_rate =
    if patches = 0 then 0.0
    else float_of_int patch_excludes /. float_of_int patches *. 100.0
  in
  let patch_pass_rate =
    if patches = 0 then 0.0
    else float_of_int patch_passes /. float_of_int patches *. 100.0
  in
  let patch_fail_rate =
    if patches = 0 then 0.0
    else float_of_int patch_fails /. float_of_int patches *. 100.0
  in
  let durations = List.sort compare stat.durations in
  let duration_total = List.fold_left ( +. ) 0.0 durations in
  let duration_avg = duration_total /. float_of_int total in
  let duration_max = durations |> List.rev |> List.hd in
  let duration_min = durations |> List.hd in
  Format.asprintf
    "%s: [EXCLUDE] %d/%d (%.2f%%) [PASS] %d/%d (%.2f%%) [FAIL] %d/%d (%.2f%%) \
     [PATCH] %d/%d (%.2f%%)"
    name excludes total exclude_rate passes total pass_rate fails total
    fail_rate patches total patch_rate
  |> print_endline;
  Format.asprintf
    "%s: [PATCH]: [EXCLUDE] %d/%d (%.2f%%) [PASS] %d/%d (%.2f%%) [FAIL] %d/%d \
     (%.2f%%)"
    name patch_excludes patches patch_exclude_rate patch_passes patches
    patch_pass_rate patch_fails patches patch_fail_rate
  |> print_endline;
  let exclude_by_subdir =
    stat.exclude_by_subdir
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  in
  if exclude_by_subdir <> [] then (
    Format.asprintf "%s [EXCLUDE by subdir]:" name |> print_endline;
    List.iter
      (fun (label, count) ->
        Format.asprintf "  %s: %d" label count |> print_endline)
      exclude_by_subdir);
  Format.eprintf "%s: [TOTAL] %.6f [AVG] %.6f [MAX] %.6f [MIN] %.6f\n" name
    duration_total duration_avg duration_max duration_min

(* Exceptions *)

exception TestRunErr of string * region * float
exception TestRunNegErr of float
exception TestParseFileErr of string * region * float
exception TestParseStringErr of string * region * float
exception TestParseRoundtripErr of float
exception TestUnknownErr of float

(* Timer *)

let start () = Util.Time.now ()
let stop start = Util.Time.now () -. start

(* Operations *)

let run_with_instr (module Simulator : SIM) spec_sim relname includes_p4 path_p4
    =
  let (module IH : Inst.Handler.HANDLER), read_coverage_instr =
    Inst.Coverage_instr.make ()
  in
  Inst.Hook.register [ (module IH : Inst.Handler.HANDLER) ];
  Inst.Hook.init_spec spec_sim;
  let result = Simulator.Interp.eval_program relname includes_p4 path_p4 in
  Inst.Hook.finish ();
  let cover = read_coverage_instr () in
  (result, cover)

let run_with_dangling (module Simulator : SIM) spec_sim relname includes_p4
    path_p4 =
  let (module DH : Inst.Handler.HANDLER), read_coverage_dangling =
    Inst.Coverage_dangling.make ()
  in
  Inst.Hook.register [ (module DH : Inst.Handler.HANDLER) ];
  Inst.Hook.init_spec spec_sim;
  let result = Simulator.Interp.eval_program relname includes_p4 path_p4 in
  Inst.Hook.finish ();
  let cover = read_coverage_dangling () in
  (result, cover)

let sim_with_instr (module Simulator : SIM) spec_sim includes_p4 path_p4
    path_stf =
  let (module IH : Inst.Handler.HANDLER), read_coverage_instr =
    Inst.Coverage_instr.make ()
  in
  Inst.Hook.register [ (module IH : Inst.Handler.HANDLER) ];
  Inst.Hook.init_spec spec_sim;
  let result = Simulator.run_stf_test includes_p4 path_p4 path_stf in
  Inst.Hook.finish ();
  let cover = read_coverage_instr () in
  (result, cover)

let sim_with_dangling (module Simulator : SIM) spec_sim includes_p4 path_p4
    path_stf =
  let (module DH : Inst.Handler.HANDLER), read_coverage_dangling =
    Inst.Coverage_dangling.make ()
  in
  Inst.Hook.register [ (module DH : Inst.Handler.HANDLER) ];
  Inst.Hook.init_spec spec_sim;
  let result = Simulator.run_stf_test includes_p4 path_p4 path_stf in
  Inst.Hook.finish ();
  let cover = read_coverage_dangling () in
  (result, cover)

let cover_run_instr ?(arch : string option) mode paths_spec relname includes_p4
    paths_p4 =
  let spec_sim, (module Simulator) = build_sim ?arch mode paths_spec in
  let spec_sl =
    match spec_sim with SL spec_sl -> spec_sl | _ -> assert false
  in
  let cover_multi = Coverage.Instr.Multi.init spec_sl in
  let cover_multi =
    List.fold_left
      (fun cover_multi path_p4 ->
        let _, cover_single =
          run_with_instr (module Simulator) spec_sim relname includes_p4 path_p4
        in
        Coverage.Instr.Multi.extend cover_multi path_p4 cover_single)
      cover_multi paths_p4
  in
  Coverage.Instr.Log.log_spec ~path_cov_opt:None cover_multi spec_sl

let cover_run_dangling ?(arch : string option) mode paths_spec relname
    includes_p4 paths_p4 =
  let spec_sim, (module Simulator) = build_sim ?arch mode paths_spec in
  let spec_sl =
    match spec_sim with SL spec_sl -> spec_sl | _ -> assert false
  in
  let cover_multi = Coverage.Dangling.Multi.init spec_sl in
  let cover_multi =
    List.fold_left
      (fun cover_multi path_p4 ->
        let program_result, cover_single =
          run_with_dangling
            (module Simulator)
            spec_sim relname includes_p4 path_p4
        in
        let wellformed, welltyped =
          match program_result with
          | Pass _ -> (true, true)
          | Fail (`Syntax _) -> (true, false)
          | Fail (`Runtime _) -> (false, false)
        in
        Coverage.Dangling.Multi.extend cover_multi path_p4 wellformed welltyped
          cover_single)
      cover_multi paths_p4
  in
  Coverage.Dangling.Multi.log ~path_cov_opt:None cover_multi

let cover_sim_instr ?(arch : string option) mode paths_spec includes_p4 paths_p4
    paths_stf =
  let spec_sim, (module Simulator) = build_sim ?arch mode paths_spec in
  let spec_sl =
    match spec_sim with SL spec_sl -> spec_sl | _ -> assert false
  in
  let cover_multi = Coverage.Instr.Multi.init spec_sl in
  let cover_multi =
    List.fold_left2
      (fun cover_multi path_p4 path_stf ->
        let _, cover_single =
          sim_with_instr
            (module Simulator)
            spec_sim includes_p4 path_p4 path_stf
        in
        Coverage.Instr.Multi.extend cover_multi path_p4 cover_single)
      cover_multi paths_p4 paths_stf
  in
  Coverage.Instr.Log.log_spec ~path_cov_opt:None cover_multi spec_sl

let cover_sim_dangling ?(arch : string option) mode paths_spec includes_p4
    paths_p4 paths_stf =
  let spec_sim, (module Simulator) = build_sim ?arch mode paths_spec in
  let spec_sl =
    match spec_sim with SL spec_sl -> spec_sl | _ -> assert false
  in
  let cover_multi = Coverage.Dangling.Multi.init spec_sl in
  let cover_multi =
    List.fold_left2
      (fun cover_multi path_p4 path_stf ->
        let program_result, cover_single =
          sim_with_dangling
            (module Simulator)
            spec_sim includes_p4 path_p4 path_stf
        in
        let wellformed, welltyped =
          match program_result with
          | Pass -> (true, true)
          | Fail (`Syntax _) -> (true, false)
          | Fail (`Runtime _) -> (false, false)
        in
        Coverage.Dangling.Multi.extend cover_multi path_p4 wellformed welltyped
          cover_single)
      cover_multi paths_p4 paths_stf
  in
  Coverage.Dangling.Multi.log ~path_cov_opt:None cover_multi
