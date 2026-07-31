open Domain.Lib
open Lang
open Sl
module DCov_single = Coverage.Dangling.Single
module DCov_multi = Coverage.Dangling.Multi
module Dep = Runtime.Testgen_neg.Dep
module Sim = Runtime.Sim.Signature
module F = Format
open Util.Source

(* Timeout exception for the fuzzing loop *)

exception Timeout

(* Overview of the fuzzing loop

   (#) Pre-loop: Measure the initial coverage of the dangling nodes

   (#) Loop
      1. For each danglings that were missed:
          A. Identify close-miss paths
          B. Randomly sample N close-miss paths
          C. For each close-miss path:
              i. Run SL interpreter on the program
              ii. Fetch derivations, i.e., a set of close-ASTs for the dangling
              iii. For each close-AST:
                    (1) Mutate the close-AST
                    (2) Reassemble the program with the mutated AST
                    (3) Run the SL interpreter on the mutated program
                    (4) See if it has covered the dangling
      2. Repeat the loop until the fuel is exhausted *)

(* Check if the mutated file is interesting,
   and if so, copy it to the output directory *)

let find_interesting (config : Config.t) (cover : DCov_single.t) :
    IIdSet.t * IIdSet.t =
  DCov_multi.Cover.fold
    (fun iid (branch_fuzz : DCov_multi.Branch.t)
         (iids_hit_new, iids_close_miss_new) ->
      let branch_single = DCov_single.Cover.find iid cover in
      match (branch_single.status, branch_fuzz.status) with
      (* Hits a new dangling *)
      | Hit, Miss _ ->
          let iids_hit_new = IIdSet.add iid iids_hit_new in
          (iids_hit_new, iids_close_miss_new)
      (* Adds a new close-miss *)
      | Miss (_ :: _), Miss [] ->
          let iids_close_miss_new = IIdSet.add iid iids_close_miss_new in
          (iids_hit_new, iids_close_miss_new)
      | _ -> (iids_hit_new, iids_close_miss_new))
    config.seed.cover
    (IIdSet.empty, IIdSet.empty)

let update_hit_new' (fuel : int) (iid : iid) (idx_seed : int)
    (strategy : string) (idx_method : int) (idx_mutation : int)
    (config : Config.t) (log : Logger.t) (path_hit_p4 : string)
    (kind : Mutate.kind) (welltyped : bool) (iids_hit_new : IIdSet.t) : unit =
  F.asprintf
    "[F %d] [P %d] [S %d] [%s %d] [M %d] %s hits %s (COUNT %d) (%s) (%s)" fuel
    iid idx_seed strategy idx_method idx_mutation path_hit_p4
    (IIdSet.to_string iids_hit_new)
    (IIdSet.cardinal iids_hit_new)
    (Mutate.string_of_kind kind)
    (if IIdSet.mem iid iids_hit_new then "GOODHIT" else "BADHIT")
  |> Logger.mark config.modes.logmode log;
  let oc = open_out_gen [ Open_append; Open_text ] 0o666 path_hit_p4 in
  F.asprintf "\n// Covered iids %s\n" (IIdSet.to_string iids_hit_new)
  |> output_string oc;
  close_out oc;
  (* Update the set of covered danglings *)
  Config.update_hit_seed config path_hit_p4 welltyped iids_hit_new

let update_hit_new (fuel : int) (iid : iid) (idx_seed : int) (strategy : string)
    (idx_method : int) (idx_mutation : int) (config : Config.t) (log : Logger.t)
    (path_gen_p4 : string) (kind : Mutate.kind) (iids_hit_new : IIdSet.t) : unit
    =
  (* Re-run the SL interpreter to make sure of the new hits *)
  (* Then copy the interesting test program to the output directory
     and update the running coverage *)
  let program_result, cover =
    Runner.run_program_with_dangling config.specenv.simulator
      config.specenv.spec config.specenv.relname config.specenv.includes_p4
      path_gen_p4
  in
  match program_result with
  | Pass _ when IIdSet.for_all (DCov_single.is_hit cover) iids_hit_new ->
      let path_hit_p4 =
        Util.Filesys.cp path_gen_p4 config.storage.dirname_welltyped_p4
      in
      update_hit_new' fuel iid idx_seed strategy idx_method idx_mutation config
        log path_hit_p4 kind true iids_hit_new
  | Fail (`Runtime _)
    when IIdSet.for_all (DCov_single.is_hit cover) iids_hit_new ->
      let path_hit_p4 =
        Util.Filesys.cp path_gen_p4 config.storage.dirname_illtyped_p4
      in
      update_hit_new' fuel iid idx_seed strategy idx_method idx_mutation config
        log path_hit_p4 kind false iids_hit_new
  | _ -> ()

let update_close_miss_new' (fuel : int) (iid : iid) (idx_seed : int)
    (strategy : string) (idx_method : int) (idx_mutation : int)
    (config : Config.t) (log : Logger.t) (path_close_miss_p4 : string)
    (iids_close_miss_new : IIdSet.t) : unit =
  F.asprintf "[F %d] [P %d] [S %d] [%s %d] [M %d] %s close-misses %s" fuel iid
    idx_seed strategy idx_method idx_mutation path_close_miss_p4
    (IIdSet.to_string iids_close_miss_new)
  |> Logger.log config.modes.logmode log;
  let oc = open_out_gen [ Open_append; Open_text ] 0o666 path_close_miss_p4 in
  F.asprintf "\n// Close-missed iids %s\n"
    (IIdSet.to_string iids_close_miss_new)
  |> output_string oc;
  close_out oc;
  (* Update the set of covered danglings *)
  Config.update_close_miss_seed config path_close_miss_p4 iids_close_miss_new

let update_close_miss_new (fuel : int) (iid : iid) (idx_seed : int)
    (strategy : string) (idx_method : int) (idx_mutation : int)
    (config : Config.t) (log : Logger.t) (path_gen_p4 : string)
    (iids_close_miss_new : IIdSet.t) : unit =
  (* Re-run the SL interpreter to make sure of the new close-misses *)
  (* Then copy the interesting test program to the output directory,
     and update the running coverage *)
  (* Then copy the interesting test program to the output directory
     and update the running coverage *)
  let program_result, cover =
    Runner.run_program_with_dangling config.specenv.simulator
      config.specenv.spec config.specenv.relname config.specenv.includes_p4
      path_gen_p4
  in
  match program_result with
  | Pass _
    when IIdSet.for_all (DCov_single.is_close_miss cover) iids_close_miss_new ->
      let path_close_miss_p4 =
        Util.Filesys.cp path_gen_p4 config.storage.dirname_close_miss_p4
      in
      update_close_miss_new' fuel iid idx_seed strategy idx_method idx_mutation
        config log path_close_miss_p4 iids_close_miss_new
  | _ -> ()

let update_interesting (fuel : int) (iid : iid) (idx_seed : int)
    (strategy : string) (idx_method : int) (idx_mutation : int)
    (trials : int ref) (config : Config.t) (log : Logger.t)
    (path_gen_p4 : string) (kind : Mutate.kind) (value_program : value) : unit =
  (* Evaluate the generated program to see if it is interesting *)
  let time_start = Unix.gettimeofday () in
  F.asprintf "[F %d] [P %d] [S %d] [%s %d] [M %d] [%d/%d] Evaluating %s" fuel
    iid idx_seed strategy idx_method idx_mutation !trials Config.trials_seed
    path_gen_p4
  |> Logger.log config.modes.logmode log;
  let welltyped, cover =
    let rel_result, cover =
      Runner.run_program_internal_with_dangling config.specenv.simulator
        config.specenv.spec config.specenv.relname value_program
    in
    match rel_result with Pass _ -> (true, cover) | Fail _ -> (false, cover)
  in
  let time_end = Unix.gettimeofday () in
  F.asprintf
    "[F %d] [P %d] [S %d] [%s %d] [M %d] [%d/%d] Evaluated %s (took %.2f)" fuel
    iid idx_seed strategy idx_method idx_mutation !trials Config.trials_seed
    path_gen_p4 (time_end -. time_start)
  |> Logger.log config.modes.logmode log;
  (* Find newly hit or newly close-missing nodes *)
  let iids_hit_new, iids_close_miss_new = find_interesting config cover in
  (* Collect the file if it covers a new dangling, and update the running coverage
     If in strict mode, we only collect the file if it covers the intended dangling *)
  (match config.modes.covermode with
  | Relaxed ->
      if not (IIdSet.is_empty iids_hit_new) then
        update_hit_new fuel iid idx_seed strategy idx_method idx_mutation config
          log path_gen_p4 kind iids_hit_new
  | Strict ->
      if IIdSet.mem iid iids_hit_new then
        update_hit_new fuel iid idx_seed strategy idx_method idx_mutation config
          log path_gen_p4 kind (IIdSet.singleton iid));
  (* Collect the file if it is well-typed and covers a new close-miss dangling,
     then update the running coverage *)
  if welltyped && not (IIdSet.is_empty iids_close_miss_new) then
    update_close_miss_new fuel iid idx_seed strategy idx_method idx_mutation
      config log path_gen_p4 iids_close_miss_new

(* Mutate an AST and generate a new program *)

let classify_mutation' (fuel : int) (iid : iid) (idx_seed : int)
    (strategy : string) (idx_method : int) (idx_mutation : int)
    (trials : int ref) (config : Config.t) (log : Logger.t)
    (dirname_gen_tmp : string) (path_p4 : string) (comment_gen_p4 : string)
    (kind : Mutate.kind) (value_source : value) (value_mutated : value)
    (value_program : value) : unit =
  let path_gen_p4 =
    F.asprintf "%s/%s_F%dP%dS%d%s%dM%dT%d.p4" dirname_gen_tmp
      (Util.Filesys.base ~suffix:".p4" path_p4)
      fuel iid idx_seed
      (if strategy = "Derive" then "D"
       else if strategy = "Random" then "R"
       else "")
      idx_method idx_mutation !trials
  in
  let comment_gen_p4 =
    F.asprintf "%s\n/*\nFrom %s\nTo %s\n*/\n" comment_gen_p4
      (Sl.Print.string_of_value value_source)
      (Sl.Print.string_of_value value_mutated)
  in
  (* Write the mutated program to a file *)
  let oc = open_out path_gen_p4 in
  F.asprintf "%s\n%s\n" comment_gen_p4 (config.specenv.printer value_program)
  |> output_string oc;
  close_out oc;
  (* Check if the mutated program is interesting, and if so, update *)
  update_interesting fuel iid idx_seed strategy idx_method idx_mutation trials
    config log path_gen_p4 kind value_program

let classify_mutation (fuel : int) (iid : iid) (idx_seed : int)
    (strategy : string) (idx_method : int) (idx_mutation : int)
    (trials : int ref) (config : Config.t) (log : Logger.t)
    (dirname_gen_tmp : string) (path_p4 : string) (comment_gen_p4 : string)
    (vdg : Dep.Graph.t) (kind : Mutate.kind) (value_source : value)
    (value_mutated : value) : unit =
  (* Reassemble the program with the mutated AST *)
  let renamer = VIdMap.singleton value_source.note.vid value_mutated in
  let value_program = Dep.Graph.reassemble_graph_from_root vdg renamer in
  (* Mutation may yield a syntactically ill-formed AST, so have a try block *)
  try
    classify_mutation' fuel iid idx_seed strategy idx_method idx_mutation trials
      config log dirname_gen_tmp path_p4 comment_gen_p4 kind value_source
      value_mutated value_program
  with Util.Error.UnparseError msg ->
    Logger.warn config.modes.logmode log
      (Format.asprintf "error while printing the mutated program: %s" msg)

let fuzz_mutation (fuel : int) (iid : iid) (idx_seed : int) (strategy : string)
    (idx_method : int) (trials : int ref) (config : Config.t) (log : Logger.t)
    (query : Query.t) (dirname_gen_tmp : string) (path_p4 : string)
    (comment_gen_p4 : string) (vdg : Dep.Graph.t) (vid_source : vid) : unit =
  F.asprintf "[F %d] [P %d] [S %d] [%s %d]\n[File] %s\n" fuel iid idx_seed
    strategy idx_method path_p4
  |> Query.query query;
  (* Mutate the AST *)
  let mutations =
    Mutate.mutates Config.trials_mutation config.specenv.tdenv
      config.specenv.mixopenv vdg vid_source
  in
  (* Generate the mutated program *)
  List.iteri
    (fun idx_mutation (kind, value_source, value_mutated) ->
      if
        !trials < Config.trials_seed && DCov_multi.is_miss config.seed.cover iid
      then (
        trials := !trials + 1;
        F.asprintf "[Source] %s\n" (Sl.Print.string_of_value value_source)
        |> Query.query query;
        F.asprintf "[Mutated] [%s] %s\n"
          (Mutate.string_of_kind kind)
          (Sl.Print.string_of_value value_mutated)
        |> Query.answer query;
        let comment_gen_p4 =
          F.asprintf "%s\n// Mutation %s\n" comment_gen_p4
            (Mutate.string_of_kind kind)
        in
        classify_mutation fuel iid idx_seed strategy idx_method idx_mutation
          trials config log dirname_gen_tmp path_p4 comment_gen_p4 vdg kind
          value_source value_mutated))
    mutations

(* Fuzzing from derivations *)

let fuzz_derivations (fuel : int) (iid : iid) (idx_seed : int)
    (trials : int ref) (config : Config.t) (log : Logger.t) (query : Query.t)
    (dirname_gen_tmp : string) (path_p4 : string) (vdg : Dep.Graph.t)
    (derivations_source : (vid * int) list) : unit =
  List.iteri
    (fun idx_derivation (vid_source, depth) ->
      if
        !trials < Config.trials_seed && DCov_multi.is_miss config.seed.cover iid
      then
        let comment_gen_p4 =
          F.asprintf "// Intended iid %d\n// Source vid %d\n// Depth %d\n" iid
            vid_source depth
        in
        let strategy = "Derive" in
        fuzz_mutation fuel iid idx_seed strategy idx_derivation trials config
          log query dirname_gen_tmp path_p4 comment_gen_p4 vdg vid_source)
    derivations_source

let fuzz_derivations_bounded (fuel : int) (iid : iid) (idx_seed : int)
    (config : Config.t) (log : Logger.t) (query : Query.t)
    (dirname_gen_tmp : string) (path_p4 : string) (vdg : Dep.Graph.t)
    (derivations_source : (vid * int) list) : unit =
  if derivations_source = [] then
    F.asprintf "[F %d] [P %d] [S %d] Skipping, no derivation found" fuel iid
      idx_seed
    |> Logger.log config.modes.logmode log
  else
    let derivations_total = List.length derivations_source in
    F.asprintf
      "[F %d] [P %d] [S %d] Fuzzing from %d derivations, until %d trials" fuel
      iid idx_seed derivations_total Config.trials_seed
    |> Logger.log config.modes.logmode log;
    let trials = ref 0 in
    while
      !trials < Config.trials_seed && DCov_multi.is_miss config.seed.cover iid
    do
      fuzz_derivations fuel iid idx_seed trials config log query dirname_gen_tmp
        path_p4 vdg derivations_source
    done

(* Fuzzing from a random value id *)

let fuzz_randoms (fuel : int) (iid : iid) (idx_seed : int) (trials : int ref)
    (config : Config.t) (log : Logger.t) (query : Query.t)
    (dirname_gen_tmp : string) (path_p4 : string) (vdg : Dep.Graph.t)
    (vids_source : vid list) : unit =
  List.iteri
    (fun idx_random vid_source ->
      if
        !trials < Config.trials_seed && DCov_multi.is_miss config.seed.cover iid
      then
        let comment_gen_p4 =
          F.asprintf "// Intended iid %d\n// Source vid %d\n" iid vid_source
        in
        let strategy = "Random" in
        fuzz_mutation fuel iid idx_seed strategy idx_random trials config log
          query dirname_gen_tmp path_p4 comment_gen_p4 vdg vid_source)
    vids_source

let fuzz_randoms_bounded (fuel : int) (iid : iid) (idx_seed : int)
    (config : Config.t) (log : Logger.t) (query : Query.t)
    (dirname_gen_tmp : string) (path_p4 : string) (vdg : Dep.Graph.t)
    (vids_source : vid list) : unit =
  F.asprintf
    "[F %d] [P %d] [S %d] Fuzzing from %d random values, until %d trials" fuel
    iid idx_seed (List.length vids_source) Config.trials_seed
  |> Logger.log config.modes.logmode log;
  let trials = ref 0 in
  while
    !trials < Config.trials_seed && DCov_multi.is_miss config.seed.cover iid
  do
    fuzz_randoms fuel iid idx_seed trials config log query dirname_gen_tmp
      path_p4 vdg vids_source
  done

(* Fuzzing from a seed program *)

let fuzz_seed_random (fuel : int) (iid : iid) (idx_seed : int)
    (config : Config.t) (log : Logger.t) (query : Query.t)
    (dirname_gen_tmp : string) (path_p4 : string) (vdg : Dep.Graph.t) : unit =
  (* Randomly sample N vids from the program *)
  let vids_source =
    List.init vdg.root Fun.id
    |> List.filter (fun vid -> Dep.Graph.G.mem vdg.nodes vid)
    |> Rand.random_sample Config.samples_related_vid
  in
  (* Mutate the ASTs and dump to file *)
  fuzz_randoms_bounded fuel iid idx_seed config log query dirname_gen_tmp
    path_p4 vdg vids_source

let fuzz_seed_deriving (fuel : int) (iid : iid) (idx_seed : int)
    (config : Config.t) (log : Logger.t) (query : Query.t)
    (dirname_gen_tmp : string) (path_p4 : string) (vdg : Dep.Graph.t)
    (cover : DCov_single.t) : unit =
  (* Derive closes-ASTs from the dangling *)
  F.asprintf "[F %d] [P %d] [S %d] Finding derivations from %s" fuel iid
    idx_seed path_p4
  |> Logger.log config.modes.logmode log;
  let time_start = Unix.gettimeofday () in
  let derivations_source = Derive.derive_dangling iid vdg cover in
  let time_end = Unix.gettimeofday () in
  (* Take top ranked derivations, i.e., the ones with the smallest depth *)
  F.asprintf
    "[F %d] [P %d] [S %d] Found total %d derivations, sampling top %d (took \
     %.2f)"
    fuel iid idx_seed
    (List.length derivations_source)
    Config.samples_derivation_source (time_end -. time_start)
  |> Logger.log config.modes.logmode log;
  let derivations_source =
    if List.length derivations_source < Config.samples_derivation_source then
      derivations_source
    else
      List.init Config.samples_derivation_source (List.nth derivations_source)
  in
  (* Mutate the close-ASTs and dump to file *)
  fuzz_derivations_bounded fuel iid idx_seed config log query dirname_gen_tmp
    path_p4 vdg derivations_source

let fuzz_seed_hybrid (fuel : int) (iid : iid) (idx_seed : int)
    (config : Config.t) (log : Logger.t) (query : Query.t)
    (dirname_gen_tmp : string) (path_p4 : string) (vdg : Dep.Graph.t)
    (cover : DCov_single.t) : unit =
  (* Derive closes-ASTs from the dangling *)
  F.asprintf "[F %d] [P %d] [S %d] Finding derivations from %s" fuel iid
    idx_seed path_p4
  |> Logger.log config.modes.logmode log;
  let time_start = Unix.gettimeofday () in
  let derivations_source = Derive.derive_dangling iid vdg cover in
  let time_end = Unix.gettimeofday () in
  (* Take top ranked derivations, i.e., the ones with the smallest depth *)
  F.asprintf
    "[F %d] [P %d] [S %d] Found total %d derivations, sampling top %d (took \
     %.2f)"
    fuel iid idx_seed
    (List.length derivations_source)
    Config.samples_derivation_source (time_end -. time_start)
  |> Logger.log config.modes.logmode log;
  let derivations_source =
    if List.length derivations_source < Config.samples_derivation_source then
      derivations_source
    else
      List.init Config.samples_derivation_source (List.nth derivations_source)
  in
  (* If there are no derivations, fallback to random *)
  match derivations_source with
  | [] ->
      fuzz_seed_random fuel iid idx_seed config log query dirname_gen_tmp
        path_p4 vdg
  | _ ->
      fuzz_derivations_bounded fuel iid idx_seed config log query
        dirname_gen_tmp path_p4 vdg derivations_source

let fuzz_seed (fuel : int) (iid : iid) (idx_seed : int) (config : Config.t)
    (log : Logger.t) (query : Query.t) (dirname_gen_tmp : string)
    (path_p4 : string) : unit =
  let time_start = Unix.gettimeofday () in
  F.asprintf "[F %d] [P %d] [S %d] Running SL interpreter on %s" fuel iid
    idx_seed path_p4
  |> Logger.log config.modes.logmode log;
  (* Construct the value dependency graph for deriving and hybrid modes *)
  let derive =
    match config.modes.mutationmode with
    | Random -> false
    | Derive | Hybrid -> true
  in
  (* Run SL interpreter on the program,
     and if it is well-typed, start generating tests from it *)
  let program_result, cover, vdg =
    Runner.run_program_with_dangling_and_vdg ~derive config.specenv.simulator
      config.specenv.spec config.specenv.relname config.specenv.includes_p4
      path_p4
  in
  (match program_result with
  | Pass _ ->
      let time_end = Unix.gettimeofday () in
      F.asprintf
        "[F %d] [P %d] [S %d] SL interpreter succeeded on %s (took %.2f)" fuel
        iid idx_seed path_p4 (time_end -. time_start)
      |> Logger.log config.modes.logmode log;
      (match config.modes.mutationmode with
      | Random ->
          fuzz_seed_random fuel iid idx_seed config log query dirname_gen_tmp
            path_p4 vdg
      | Derive ->
          fuzz_seed_deriving fuel iid idx_seed config log query dirname_gen_tmp
            path_p4 vdg cover
      | Hybrid ->
          fuzz_seed_hybrid fuel iid idx_seed config log query dirname_gen_tmp
            path_p4 vdg cover);
      Dep.Graph.G.reset vdg.nodes;
      Dep.Graph.G.reset vdg.edges
  | Fail _ ->
      F.asprintf "[F %d] [P %d] [S %d] SL interpreter failed on %s" fuel iid
        idx_seed path_p4
      |> Logger.log config.modes.logmode log);
  let total, hits, coverage = DCov_multi.measure_coverage config.seed.cover in
  F.asprintf "[F %d] [P %d] [S %d] Coverage %d/%d (%.2f%%)" fuel iid idx_seed
    hits total coverage
  |> Logger.log config.modes.logmode log

let fuzz_seeds (fuel : int) (iid : iid) (config : Config.t) (log : Logger.t)
    (query : Query.t) (dirname_gen_tmp : string) (paths_p4 : string list) : unit
    =
  (* Fuzz from seed programs until the target dangling node is covered *)
  List.iteri
    (fun idx_seed path_p4 ->
      if DCov_multi.is_miss config.seed.cover iid then (
        let _ =
          Sys.set_signal Sys.sigalrm
            (Sys.Signal_handle (fun _ -> raise Timeout))
        in
        Unix.alarm Config.timeout_seed |> ignore;
        (try
           fuzz_seed fuel iid idx_seed config log query dirname_gen_tmp path_p4
         with Timeout ->
           F.asprintf "[F %d] [S %d] [P %d] Timeout on %s" fuel iid idx_seed
             path_p4
           |> Logger.warn config.modes.logmode log);
        Unix.alarm 0 |> ignore))
    paths_p4

(* Fuzzing from a target dangling node *)

let fuzz_dangling (fuel : int) (iid : iid) (config : Config.t) (log : Logger.t)
    (query : Query.t) (paths_p4 : string list) : unit =
  F.asprintf "[F %d] [P %d] Targeting dangling %d" fuel iid iid
  |> Logger.log config.modes.logmode log;
  (* Create a directory for the generated programs *)
  let dirname_gen_tmp =
    config.storage.dirname_gen ^ "/fuel" ^ string_of_int fuel ^ "dangling"
    ^ string_of_int iid
  in
  Util.Filesys.mkdir dirname_gen_tmp;
  (* Randomly sample N close-miss paths *)
  let paths_p4 = Rand.random_sample Config.samples_close_miss paths_p4 in
  (* Generate tests from the files *)
  (try fuzz_seeds fuel iid config log query dirname_gen_tmp paths_p4
   with _ as err ->
     F.asprintf "[F %d] [P %d] Unexpected error occurred : %s" fuel iid
       (Printexc.to_string err)
     |> Logger.warn config.modes.logmode log);
  (* Remove the directory for the generated programs *)
  Util.Filesys.rmdir dirname_gen_tmp

let fuzz_danglings (fuel : int) (config : Config.t) (log : Logger.t)
    (query : Query.t) : unit =
  let iids = DCov_multi.Cover.dom config.seed.cover in
  IIdSet.iter
    (fun iid ->
      let branch = DCov_multi.Cover.find iid config.seed.cover in
      match branch.status with
      | Hit _ -> ()
      | Miss [] -> ()
      | Miss paths_p4 -> fuzz_dangling fuel iid config log query paths_p4)
    iids

(* Fuzzing in a loop with fuel *)

let rec fuzz_loop (fuel : int) (config : Config.t) : Config.t =
  if fuel = 0 then config
  else
    (* Create a log for the current fuel *)
    let logname = F.asprintf "%s/fuel%d.log" config.storage.dirname_log fuel in
    let log = Logger.init logname in
    (* Create q query for the current fuel *)
    let queryname =
      F.asprintf "%s/fuel%d.query" config.storage.dirname_query fuel
    in
    let query = Query.init queryname in
    (* Fuzz single iteration *)
    F.asprintf "[F %d] Start fuzzing loop" fuel
    |> Logger.log config.modes.logmode log;
    fuzz_danglings fuel config log query;
    let total, hits, coverage = DCov_multi.measure_coverage config.seed.cover in
    F.asprintf "[F %d] End fuzzing loop with coverage %d/%d (%.2f%%)" fuel hits
      total coverage
    |> Logger.log config.modes.logmode log;
    (* Close the logger *)
    Logger.close log;
    (* Close the query *)
    Query.close query;
    (* Proceed to the next fuel level *)
    fuzz_loop (fuel - 1) config

(* Entry point to main fuzzing loop *)

let fuzzer_init (spec : spec) (relname : string) (includes_p4 : string list)
    (dirname_gen : string) (name_campaign : string option)
    (randseed : int option) (logmode : Modes.logmode)
    (bootmode : Modes.bootmode) (mutationmode : Modes.mutationmode)
    (covermode : Modes.covermode) : Config.t =
  (* Name the campaign *)
  let name_campaign =
    match name_campaign with
    | Some name_campaign -> name_campaign
    | None ->
        let timestamp =
          let tm = Unix.gettimeofday () |> Unix.localtime in
          F.asprintf "%04d-%02d-%02d-%02d-%02d-%02d" (tm.Unix.tm_year + 1900)
            (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
            tm.Unix.tm_sec
        in
        "fuzz-" ^ timestamp
  in
  (* Create directories for storage *)
  let dirname_gen = dirname_gen ^ "/" ^ name_campaign in
  let storage = Config.init_storage dirname_gen in
  (* Create a mode *)
  let modes = Modes.{ bootmode; logmode; mutationmode; covermode } in
  (* Create a initializer log *)
  let logname_init = storage.dirname_log ^ "/init.log" in
  let log_init = Logger.init logname_init in
  (* Log the command line arguments *)
  F.asprintf "[COMMAND] testgen -gen %s%s%s%s" dirname_gen
    (match modes.bootmode with
    | Cold (excludes_p4, dirname_seed_p4) ->
        "-e" ^ String.concat " " excludes_p4 ^ "-cold " ^ dirname_seed_p4
    | Warm path_boot -> " -warm " ^ path_boot)
    (match modes.mutationmode with
    | Random -> " -random"
    | Derive -> ""
    | Hybrid -> " -hybrid")
    (match modes.covermode with Strict -> " -strict" | Relaxed -> "")
  |> Logger.log modes.logmode log_init;
  (* Create a spec environment *)
  "Loading type definitions from the spec file"
  |> Logger.log modes.logmode log_init;
  let specenv = Config.init_specenv spec relname includes_p4 in
  (* Create a seed *)
  "Booting initial coverage" |> Logger.log modes.logmode log_init;
  let cover_seed =
    match modes.bootmode with
    | Cold (excludes_p4, dirname_seed_p4) ->
        let cover_seed =
          Boot.boot_cold specenv.simulator specenv.spec relname includes_p4
            excludes_p4 dirname_seed_p4
        in
        (* Log the initial coverage for later use in warm boot *)
        let path_cov = dirname_gen ^ "/boot.coverage" in
        DCov_multi.log ~path_cov_opt:(Some path_cov) cover_seed;
        cover_seed
    | Warm path_boot -> Boot.boot_warm path_boot
  in
  let seed = Config.init_seed cover_seed in
  (* Close the initial log *)
  let total, hits, coverage = DCov_multi.measure_coverage cover_seed in
  F.asprintf "Finished booting with initial coverage %d/%d (%.2f%%)" hits total
    coverage
  |> Logger.log modes.logmode log_init;
  F.asprintf
    "[SAMPLES_CLOSE_MISS] %d [SAMPLES_RELATED_VID] %d \
     [SAMPLES_DERIVATION_SOURCE] %d [TRIALS_MUTATION] %d [TRIALS_SEED] %d \
     [TIMEOUT_SEED] %d"
    Config.samples_close_miss Config.samples_related_vid
    Config.samples_derivation_source Config.trials_mutation Config.trials_seed
    Config.timeout_seed
  |> Logger.log modes.logmode log_init;
  Logger.close log_init;
  (* Create a configuration *)
  let config = Config.init randseed modes specenv storage seed in
  config

let fuzzer (fuel : int) (spec : spec) (relname : string)
    (includes_p4 : string list) (dirname_gen : string)
    (name_campaign : string option) (randseed : int option)
    (logmode : Modes.logmode) (bootmode : Modes.bootmode)
    (mutationmode : Modes.mutationmode) (covermode : Modes.covermode) : unit =
  (* Initialize the fuzzing configuration *)
  let config =
    fuzzer_init spec relname includes_p4 dirname_gen name_campaign randseed
      logmode bootmode mutationmode covermode
  in
  (* Call the main fuzzing loop *)
  let config = fuzz_loop fuel config in
  (* Log the final coverage *)
  let path_cov = config.storage.dirname_gen ^ "/final.coverage" in
  DCov_multi.log ~path_cov_opt:(Some path_cov) config.seed.cover
