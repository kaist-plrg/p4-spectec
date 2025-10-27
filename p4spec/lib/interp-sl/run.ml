open Domain.Lib
open Sl.Ast
module Value = Runtime_dynamic.Value
module Cache = Runtime_dynamic.Cache
module Dep = Runtime_testgen.Dep
module Ignore = Runtime_testgen.Cov.Ignore
module SCov = Runtime_testgen.Cov.Single
module MCov = Runtime_testgen.Cov.Multiple
open Error
module F = Format
open Util.Source

type res =
  | Pass of value list * Dep.Graph.t * vid * SCov.Cover.t
  | Fail of region * string * SCov.Cover.t
  | IllFormed of region * string * SCov.Cover.t

let do_run (ctx : Ctx.t) (spec : spec) (relname : string) (values : value list)
    : value list =
  let ctx = Interp.load_spec ctx spec in
  match Interp.invoke_rel ctx (relname $ no_region) values with
  | Some values -> values
  | None -> error no_region "relation was not matched"

(* Entry point: Run the specification on given values *)

let run (spec : spec) (relname : string) (filename_p4 : string)
    (values : value list) : res =
  Builtin.init ();
  Value.refresh ();
  Cache.Cache.clear !Interp.func_cache;
  Cache.Cache.clear !Interp.rule_cache;
  let cover = ref (SCov.init IdSet.empty spec) in
  try
    let graph = Dep.Graph.empty () in
    let vid = -1 in
    let cover = ref (SCov.init IdSet.empty spec) in
    let ctx = Ctx.empty ~derive:false filename_p4 graph vid cover in
    let values = do_run ctx spec relname values in
    Pass
      (values, ctx.testing.graph, ctx.testing.vid_program, !(ctx.testing.cover))
  with
  | Util.Error.ParseError (at, msg) -> IllFormed (at, msg, !cover)
  | Util.Error.InterpError (at, msg) -> Fail (at, msg, !cover)

(* Entry point: Run the specification on a given P4 file *)

let run_program' ?(derive : bool = false) (spec : spec) (relname : string)
    (includes_p4 : string list) (filename_p4 : string) (ignores : IdSet.t) : res
    =
  Builtin.init ();
  Value.refresh ();
  Cache.Cache.clear !Interp.func_cache;
  Cache.Cache.clear !Interp.rule_cache;
  let cover = ref (SCov.init ignores spec) in
  try
    let value_program = Interface.Parse.parse_file includes_p4 filename_p4 in
    let graph = Dep.Graph.assemble_graph value_program in
    let ctx =
      Ctx.empty ~derive filename_p4 graph value_program.note.vid cover
    in
    let values = do_run ctx spec relname [ value_program ] in
    Pass
      (values, ctx.testing.graph, ctx.testing.vid_program, !(ctx.testing.cover))
  with
  | Util.Error.ParseError (at, msg) -> IllFormed (at, msg, !cover)
  | Util.Error.InterpError (at, msg) -> Fail (at, msg, !cover)

let run_program ?(derive : bool = false) (spec : spec) (relname : string)
    (includes_p4 : string list) (filename_p4 : string)
    (filenames_ignore : string list) : res =
  let ignores = Ignore.init filenames_ignore in
  run_program' ~derive spec relname includes_p4 filename_p4 ignores

(* Entry point: Run the specification on a given program value *)

let run_program_testgen (spec : spec) (relname : string) (filename_p4 : string)
    (value_program : value) (ignores : IdSet.t) : res =
  Builtin.init ();
  Value.refresh ();
  Cache.Cache.clear !Interp.func_cache;
  Cache.Cache.clear !Interp.rule_cache;
  let cover = ref (SCov.init ignores spec) in
  try
    let graph = Dep.Graph.empty () in
    let ctx =
      Ctx.empty ~derive:false filename_p4 graph value_program.note.vid cover
    in
    let values = do_run ctx spec relname [ value_program ] in
    Pass
      (values, ctx.testing.graph, ctx.testing.vid_program, !(ctx.testing.cover))
  with Util.Error.InterpError (at, msg) -> Fail (at, msg, !cover)

(* Entry point : Measure spec coverage of phantom nodes on a given P4 file *)

let cover_program (spec : spec) (relname : string) (includes_p4 : string list)
    (filenames_p4 : string list) (filenames_ignore : string list) : MCov.Cover.t
    =
  let ignores = Ignore.init filenames_ignore in
  let cover_multi = MCov.init ignores spec in
  List.fold_left
    (fun cover_multi filename_p4 ->
      let wellformed, welltyped, cover_single =
        match run_program' spec relname includes_p4 filename_p4 ignores with
        | Pass (_, _, _, cover_single) -> (true, true, cover_single)
        | Fail (_, _, cover_single) -> (true, false, cover_single)
        | IllFormed (_, _, cover_single) -> (false, false, cover_single)
      in
      MCov.extend cover_multi filename_p4 wellformed welltyped cover_single)
    cover_multi filenames_p4
