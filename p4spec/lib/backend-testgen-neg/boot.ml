module DCov_multi = Coverage.Dangling.Multi
module Sim = Runtime.Sim.Simulator

(* Measure initial coverage of phantoms *)

(* On cold boot, first measure the coverage of the seed *)

let boot_cold (module Driver : Sim.DRIVER) (spec : Sim.spec) (relname : string)
    (includes_p4 : string list) (excludes_p4 : string list)
    (dirname_p4 : string) : DCov_multi.t =
  let excludes_p4 = Util.Test.collect_excludes excludes_p4 in
  let filenames_p4 = Util.Filesys.collect_files ~suffix:".p4" dirname_p4 in
  let filenames_p4 =
    List.filter
      (fun filename_p4 ->
        not (List.exists (String.equal filename_p4) excludes_p4))
      filenames_p4
  in
  Runner.run_programs_with_dangling
    (module Driver)
    spec relname includes_p4 filenames_p4

(* On warm boot, load the coverage from a file *)

let boot_warm (filename_cov : string) : DCov_multi.t =
  DCov_multi.load filename_cov
