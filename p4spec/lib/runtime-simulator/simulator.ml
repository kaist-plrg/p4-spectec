open Sl.Ast
module Dep = Runtime_testgen.Dep
module Cover = Runtime_testgen.Cov.Single.Cover
module IO = Io
open Util.Source

(* Module signatures for interpreter-architecture interaction *)

type runresult =
  | Pass of Sl.Ast.value list * Cover.t
  | Fail of region * string * Cover.t
  | IllFormed of region * string

module type ARCH = sig
  val eval_extern_func_call : Sl.Ast.value list -> Sl.Ast.value list
  val eval_extern_method_call : Sl.Ast.value list -> Sl.Ast.value list

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
  val eval_rel_call :
    Sl.Ast.spec -> string -> Sl.Ast.value list -> Sl.Ast.value list

  val eval_rel_call_program :
    Sl.Ast.spec -> string -> string list -> string -> string list -> runresult

  val eval_func_call :
    Sl.Ast.spec ->
    string ->
    Sl.Ast.typ list ->
    Sl.Ast.value list ->
    Sl.Ast.value
end

module type DRIVER = sig
  val run_program :
    Sl.Ast.spec -> string -> string list -> string -> string list -> runresult

  val run_stf_test :
    Sl.Ast.spec -> string list -> string -> string -> string list -> unit
end
