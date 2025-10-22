open Io

(* Module signatures for interpreter-architecture interaction *)

module type ARCH = sig
  val init : Sl.Ast.spec -> string list -> string -> Sl.Ast.value * Sl.Ast.value
  val eval_extern_func_call : Sl.Ast.value list -> Sl.Ast.value list
  val eval_extern_method_call : Sl.Ast.value list -> Sl.Ast.value list

  val drive_pipe :
    Sl.Ast.value ->
    Sl.Ast.value ->
    port ->
    packet ->
    Sl.Ast.value * Sl.Ast.value * result option
end

module type INTERP = sig
  val eval_rel_call :
    Sl.Ast.spec -> string -> Sl.Ast.value list -> Sl.Ast.value list

  val eval_rel_call_program :
    Sl.Ast.spec -> string -> string list -> string -> Sl.Ast.value list

  val eval_func_call :
    Sl.Ast.spec ->
    string ->
    Sl.Ast.typ list ->
    Sl.Ast.value list ->
    Sl.Ast.value
end

module type DRIVER = sig
  val run : Sl.Ast.spec -> string list -> string -> string -> unit
end
