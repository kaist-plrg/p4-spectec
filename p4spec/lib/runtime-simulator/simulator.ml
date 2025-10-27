open Domain.Lib
open Sl.Ast
module Dep = Runtime_testgen.Dep
module SCov = Runtime_testgen.Cov.Single
module MCov = Runtime_testgen.Cov.Multiple
module IO = Io
open Util.Source

(* Module signatures for interpreter-architecture interaction *)

type program_result =
  | Pass of Sl.Ast.value list * Dep.Graph.t * vid * SCov.Cover.t
  | Fail of region * string * SCov.Cover.t
  | IllFormed of region * string * SCov.Cover.t

type rel_result =
  | Pass of Sl.Ast.value list * SCov.Cover.t
  | Fail of region * string * SCov.Cover.t

type func_result =
  | Pass of Sl.Ast.value * SCov.Cover.t
  | Fail of region * string * SCov.Cover.t

module type ARCH = sig
  (* Extern evaluation *)

  val eval_extern_func_call : Sl.Ast.value list -> Sl.Ast.value list
  val eval_extern_method_call : Sl.Ast.value list -> Sl.Ast.value list

  (* Pipeline evaluation *)

  val init_pipe :
    Sl.Ast.spec ->
    string list ->
    string ->
    string list ->
    Sl.Ast.value * Sl.Ast.value

  val drive_pipe :
    Sl.Ast.value ->
    Sl.Ast.value ->
    IO.rx ->
    Sl.Ast.value * Sl.Ast.value * IO.tx option
end

module type INTERP = sig
  (* Relation and meta-function valuation *)

  val eval_program :
    derive:bool ->
    Sl.Ast.spec ->
    string ->
    string list ->
    string ->
    string list ->
    program_result

  val eval_program_with_ignores :
    derive:bool ->
    Sl.Ast.spec ->
    string ->
    string list ->
    string ->
    IdSet.t ->
    program_result

  val eval_rel :
    ?ignores:IdSet.t -> Sl.Ast.spec -> string -> Sl.Ast.value list -> rel_result

  val eval_func :
    Sl.Ast.spec -> string -> Sl.Ast.typ list -> Sl.Ast.value list -> func_result

  (* Coverage *)

  val cover_programs :
    Sl.Ast.spec ->
    string ->
    string list ->
    string list ->
    string list ->
    MCov.Cover.t
end

module type DRIVER = sig
  (* Run a P4 program against the spec *)

  val run_program :
    derive:bool ->
    Sl.Ast.spec ->
    string ->
    string list ->
    string ->
    string list ->
    program_result

  val run_program_with_ignores :
    derive:bool ->
    Sl.Ast.spec ->
    string ->
    string list ->
    string ->
    IdSet.t ->
    program_result

  val run_program_with_ignores_internal :
    derive:bool ->
    Sl.Ast.spec ->
    string ->
    Sl.Ast.value ->
    IdSet.t ->
    rel_result

  (* Run a P4 program against the spec and a STF test *)

  val run_stf_test :
    Sl.Ast.spec -> string list -> string -> string -> string list -> unit

  (* Coverage *)

  val cover_programs :
    Sl.Ast.spec ->
    string ->
    string list ->
    string list ->
    string list ->
    MCov.Cover.t
end
