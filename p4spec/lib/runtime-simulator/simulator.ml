open Domain.Lib
module Value = Runtime_dynamic.Value
module Dep = Runtime_testgen.Dep
module SCov = Runtime_testgen.Cov.Single
module MCov = Runtime_testgen.Cov.Multiple
module IO = Io
open Util.Source

(* Module signatures for interpreter-architecture interaction *)

type spec = IL of Il.Ast.spec | SL of Sl.Ast.spec | Empty

type program_result =
  | Pass of Value.t list * Dep.Graph.t * Value.id * SCov.Cover.t
  | Fail of region * string * SCov.Cover.t
  | IllFormed of region * string * SCov.Cover.t

type rel_result =
  | Pass of Value.t list * SCov.Cover.t
  | Fail of region * string * SCov.Cover.t

type func_result =
  | Pass of Value.t * SCov.Cover.t
  | Fail of region * string * SCov.Cover.t

module type ARCH = sig
  (* Extern evaluation *)

  val eval_extern_func_call : Value.t list -> Value.t list
  val eval_extern_method_call : Value.t list -> Value.t list

  (* Pipeline evaluation *)

  val init_pipe : spec -> string list -> string -> Value.t * Value.t

  val drive_pipe :
    Value.t -> Value.t -> IO.rx -> Value.t * Value.t * IO.tx option
end

module type INTERP_IL = sig
  (* Relation and meta-function valuation *)

  val eval_program :
    Il.Ast.spec -> string -> string list -> string -> program_result

  val eval_rel : Il.Ast.spec -> string -> Value.t list -> rel_result

  val eval_func :
    Il.Ast.spec -> string -> Il.Ast.typ list -> Value.t list -> func_result
end

module type INTERP_SL = sig
  (* Relation and meta-function valuation *)

  val eval_program :
    derive:bool ->
    Sl.Ast.spec ->
    string ->
    string list ->
    string ->
    program_result

  val eval_rel : Sl.Ast.spec -> string -> Value.t list -> rel_result

  val eval_func :
    Sl.Ast.spec -> string -> Sl.Ast.typ list -> Value.t list -> func_result

  (* Coverage *)

  val cover_programs :
    Sl.Ast.spec -> string -> string list -> string list -> MCov.Cover.t
end

module type DRIVER = sig
  (* Run a P4 program against the spec *)

  val run_program :
    derive:bool -> spec -> string -> string list -> string -> program_result

  val run_program_internal :
    derive:bool -> Sl.Ast.spec -> string -> Value.t -> rel_result

  (* Run a P4 program against the spec and a STF test *)

  val run_stf_test : spec -> string list -> string -> string -> unit

  (* Coverage *)

  val cover_programs :
    Sl.Ast.spec -> string -> string list -> string list -> MCov.Cover.t
end
