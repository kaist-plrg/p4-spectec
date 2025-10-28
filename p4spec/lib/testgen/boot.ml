open Sl.Ast
module MCov = Runtime_testgen.Cov.Multiple
module Sim = Runtime_simulator.Simulator
open Util.Source

(* Measure initial coverage of phantoms *)

(* On cold boot, first measure the coverage of the seed *)

let boot_cold (module Runner : Sim.DRIVER) (spec : spec) (relname : string)
    (includes_p4 : string list) (excludes_p4 : string list)
    (dirname_p4 : string) : MCov.Cover.t =
  let excludes_p4 = Util.Filesys.collect_excludes excludes_p4 in
  let filenames_p4 = Util.Filesys.collect_files ~suffix:".p4" dirname_p4 in
  let filenames_p4 =
    List.filter
      (fun filename_p4 ->
        not (List.exists (String.equal filename_p4) excludes_p4))
      filenames_p4
  in
  Runner.cover_programs spec relname includes_p4 filenames_p4

(* On warm boot, load the coverage from a file *)

let boot_warm (filename_cov : string) : MCov.Cover.t = MCov.load filename_cov
