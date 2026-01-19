open Domain
open Lib
open Lang
open Sl
module DCov_single = Coverage.Dangling.Single
module DCov_multi = Coverage.Dangling.Multi
open Runtime.Testgen_neg
open Envs
module Sim = Runtime.Sim.Simulator

(* Hyperparameters for the fuzzing loop *)

(* Max number of seeds per phantom *)
let samples_close_miss = 3

(* Max number of related vids to derive from per seed *)
let samples_related_vid = 10

(* Max number of close-ASTs per seed *)
let samples_derivation_source = 10

(* Max number of mutation trials per close-AST *)
let trials_mutation = 5

(* Trials per seed *)
let trials_seed = 50

(* Timeout per seed *)
let timeout_seed = 30

(* Environment for the spec *)
type specenv = {
  driver : (module Sim.DRIVER);
  printer : Sl.value -> string;
  spec : Sim.spec;
  relname : string;
  tdenv : TDEnv.t;
  mixopenv : MixopEnv.t;
  includes_p4 : string list;
}

(* Storage for generated files *)

type storage = {
  dirname_gen : string;
  dirname_log : string;
  dirname_query : string;
  dirname_close_miss_p4 : string;
  dirname_welltyped_p4 : string;
  dirname_illtyped_p4 : string;
}

(* Seed for the fuzz campaign *)

type seed = { mutable cover : DCov_multi.Cover.t }

(* Configuration for the fuzz campaign *)

type t = {
  rand : int;
  modes : Modes.t;
  specenv : specenv;
  storage : storage;
  seed : seed;
}

(* Load mixop groups into the environment *)

let load_mixops (mixopenv : MixopEnv.t) (def : def) : MixopEnv.t =
  match def.it with
  | TypD (id, _, deftyp, _) -> (
      match deftyp.it with
      | VariantT typcases ->
          let nottyps = List.map fst typcases in
          let insert_into_groups (typed_groups : (typ list * MixIdSet.t) list)
              (nottyp : nottyp) : (typ list * MixIdSet.t) list =
            let mixop, typs = nottyp.it in
            let rec insert_into_groups' typed_group = function
              | [] -> (typs, MixIdSet.singleton mixop) :: typed_group
              | (typs_found, group) :: rest ->
                  if List.equal Sl.Eq.eq_typ typs typs_found then
                    (typs, MixIdSet.add mixop group)
                    :: (List.rev typed_group @ rest)
                  else insert_into_groups' ((typs, group) :: typed_group) rest
            in
            insert_into_groups' [] typed_groups
          in
          let typed_groups_new =
            List.fold_left insert_into_groups [] nottyps
            |> List.filter (fun (_, mixop_group) ->
                   MixIdSet.cardinal mixop_group > 1)
          in
          if List.length typed_groups_new = 0 then mixopenv
          else
            let mixop_family_orig =
              MixopEnv.find_opt id mixopenv
              |> Option.value ~default:Mixops.Family.empty
            in
            let mixop_family =
              List.fold_left
                (fun mixop_family (_, mixop_group) ->
                  Mixops.Family.add mixop_group mixop_family)
                mixop_family_orig typed_groups_new
            in
            MixopEnv.add id mixop_family mixopenv
      | PlainT { it = VarT (id_alias, _); _ } ->
          let mixop_family =
            MixopEnv.find_opt id_alias mixopenv
            |> Option.value ~default:Mixops.Family.empty
          in
          MixopEnv.add id mixop_family mixopenv
      | _ -> mixopenv)
  | _ -> mixopenv

(* Load type definitions into the environment *)

let load_def (tdenv : TDEnv.t) (def : def) : TDEnv.t =
  match def.it with
  | ExternTypD (id, _) ->
      let td = Typdef.Extern in
      TDEnv.add id td tdenv
  | TypD (id, tparams, deftyp, _) ->
      let td = Typdef.Defined (tparams, deftyp) in
      TDEnv.add id td tdenv
  | _ -> tdenv

(* Loader *)

let load_spec (tdenv : TDEnv.t) (mixopenv : MixopEnv.t) (spec : spec) :
    TDEnv.t * MixopEnv.t =
  let tdenv = List.fold_left load_def tdenv spec in
  let mixopenv = List.fold_left load_mixops mixopenv spec in
  (tdenv, mixopenv)

(* Constructor *)

let init_specenv (spec : spec) (relname : string) (includes_p4 : string list) :
    specenv =
  let (module Driver : Sim.DRIVER) = Backend_sim.Gen.gen_placeholder () in
  Driver.init (Sim.SL spec);
  let driver = (module Driver : Sim.DRIVER) in
  let printer value_program =
    Format.asprintf "%a\n" (Interface.Unparse.pp_program_sl spec) value_program
  in
  let tdenv, mixopenv = load_spec TDEnv.empty MixopEnv.empty spec in
  let spec = Sim.SL spec in
  { driver; printer; spec; relname; tdenv; mixopenv; includes_p4 }

let init_storage (dirname_gen : string) : storage =
  Util.Filesys.mkdir dirname_gen;
  let dirname_log = dirname_gen ^ "/log" in
  Util.Filesys.mkdir dirname_log;
  let dirname_query = dirname_gen ^ "/query" in
  Util.Filesys.mkdir dirname_query;
  let dirname_close_miss_p4 = dirname_gen ^ "/closemiss" in
  Util.Filesys.mkdir dirname_close_miss_p4;
  let dirname_welltyped_p4 = dirname_gen ^ "/welltyped" in
  Util.Filesys.mkdir dirname_welltyped_p4;
  let dirname_illtyped_p4 = dirname_gen ^ "/illtyped" in
  Util.Filesys.mkdir dirname_illtyped_p4;
  {
    dirname_gen;
    dirname_log;
    dirname_query;
    dirname_close_miss_p4;
    dirname_welltyped_p4;
    dirname_illtyped_p4;
  }

let init_seed (cover : DCov_multi.t) : seed = { cover }

let init (randseed : int option) (modes : Modes.t) (specenv : specenv)
    (storage : storage) (seed : seed) =
  let rand = Option.value ~default:2025 randseed in
  Random.init rand;
  { rand; modes; specenv; storage; seed }

(* Seed updater *)

let update_hit_seed (config : t) (filename_p4 : string) (welltyped : bool)
    (pids_hit : PIdSet.t) : unit =
  let cover_seed = config.seed.cover in
  let cover_seed =
    PIdSet.fold
      (fun pid_hit cover_seed ->
        let branch : DCov_multi.Branch.t =
          DCov_multi.Cover.find pid_hit cover_seed
        in
        let branch =
          match branch.status with
          | Hit (likely, filenames_p4) ->
              let likely = likely && not welltyped in
              let filenames_p4 = filename_p4 :: filenames_p4 in
              DCov_multi.Branch.
                { branch with status = Hit (likely, filenames_p4) }
          | _ ->
              let likely = not welltyped in
              let filenames_p4 = [ filename_p4 ] in
              DCov_multi.Branch.
                { branch with status = Hit (likely, filenames_p4) }
        in
        DCov_multi.Cover.add pid_hit branch cover_seed)
      pids_hit cover_seed
  in
  config.seed.cover <- cover_seed

let update_close_miss_seed (config : t) (filename_p4 : string)
    (pids_close_miss : PIdSet.t) : unit =
  let cover_seed = config.seed.cover in
  let cover_seed =
    PIdSet.fold
      (fun pid_close_miss cover_seed ->
        let branch = DCov_multi.Cover.find pid_close_miss cover_seed in
        let branch =
          DCov_multi.Branch.{ branch with status = Miss [ filename_p4 ] }
        in
        DCov_multi.Cover.add pid_close_miss branch cover_seed)
      pids_close_miss cover_seed
  in
  config.seed.cover <- cover_seed
