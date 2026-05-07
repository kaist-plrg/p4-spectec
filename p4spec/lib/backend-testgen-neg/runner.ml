open Lang
open Sl
module DCov_single = Coverage.Dangling.Single
module DCov_multi = Coverage.Dangling.Multi
module Dep = Runtime.Testgen_neg.Dep
module Sim = Runtime.Sim.Signature

(* Spec runners *)

let run_program_with_dangling (module Simulator : Sim.SIM) (spec : Sim.spec)
    (relname : string) (includes_p4 : string list) (filename_p4 : string) :
    Sim.program_result * DCov_single.t =
  let (module DH : Inst.Handler.HANDLER), read_coverage_dangling =
    Inst.Coverage_dangling.make ()
  in
  Inst.Hook.register [ (module DH : Inst.Handler.HANDLER) ];
  Inst.Hook.init_spec spec;
  let program_result =
    Simulator.Interp.eval_program relname includes_p4 filename_p4
  in
  Inst.Hook.finish ();
  let cover = read_coverage_dangling () in
  (program_result, cover)

let run_programs_with_dangling (module Simulator : Sim.SIM) (spec : Sim.spec)
    (relname : string) (includes_p4 : string list) (filenames_p4 : string list)
    : DCov_multi.t =
  let cover_multi =
    match spec with SL spec -> DCov_multi.init spec | _ -> assert false
  in
  List.fold_left
    (fun cover_multi filename_p4 ->
      let program_result, cover_single =
        run_program_with_dangling
          (module Simulator)
          spec relname includes_p4 filename_p4
      in
      let wellformed, welltyped =
        match program_result with
        | Pass _ -> (true, true)
        | Fail (`Syntax _) -> (false, false)
        | Fail (`Runtime _) -> (true, false)
      in
      DCov_multi.extend cover_multi filename_p4 wellformed welltyped
        cover_single)
    cover_multi filenames_p4

let run_program_internal_with_dangling (module Simulator : Sim.SIM)
    (spec : Sim.spec) (relname : string) (value_program : value) :
    Sim.rel_result * DCov_single.t =
  let (module DH : Inst.Handler.HANDLER), read_coverage_dangling =
    Inst.Coverage_dangling.make ()
  in
  Inst.Hook.register [ (module DH : Inst.Handler.HANDLER) ];
  Inst.Hook.init_spec spec;
  let rel_result = Simulator.Interp.eval_rel relname [ value_program ] in
  Inst.Hook.finish ();
  let cover = read_coverage_dangling () in
  (rel_result, cover)

let run_program_with_dangling_and_vdg ~(derive : bool)
    (module Simulator : Sim.SIM) (spec : Sim.spec) (relname : string)
    (includes_p4 : string list) (filename_p4 : string) :
    Sim.program_result * DCov_single.t * Dep.Graph.t =
  let (module DH : Inst.Handler.HANDLER), read_coverage_dangling =
    Inst.Coverage_dangling.make ()
  in
  let (module VH : Inst.Handler.HANDLER), read_vdg =
    Inst.Value_dependency.make ~derive ~cache_on:Simulator.Cache.cache_on
      ~cache_off:Simulator.Cache.cache_off
  in
  let handlers =
    [ (module DH : Inst.Handler.HANDLER); (module VH : Inst.Handler.HANDLER) ]
  in
  Inst.Hook.register handlers;
  Inst.Hook.init_spec spec;
  let program_result =
    Simulator.Interp.eval_program relname includes_p4 filename_p4
  in
  Inst.Hook.finish ();
  let cover = read_coverage_dangling () in
  let vdg = read_vdg () in
  (program_result, cover, vdg)
