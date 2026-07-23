module IO = Io
open Util.Source

(* Module signatures for interpreter-architecture simulation *)

include Dynamic_runner.Signature

type stf_result =
  | Pass
  | Fail of [ `Syntax of region * string | `Runtime of region * string ]

module type ARCH = sig
  (* STF AST transformation *)

  val transform_stf_stmt : Stf.Ast.stmt -> Stf.Ast.stmt

  (* Extern evaluation *)

  val eval_extern_init : Value.t list -> Value.t
  val eval_extern_func_lctk_call : Value.t list -> Value.t list
  val eval_extern_func_call : Value.t list -> Value.t list
  val eval_extern_method_call : Value.t list -> Value.t list

  (* Architecture-specific external state *)

  val init_arch_state : Value.t

  (* Mirror session interface *)

  val add_mirror_session : Value.t -> int -> int -> Value.t
  val add_mirror_session_mc : Value.t -> int -> int -> Value.t

  (* Multicast interface *)

  val mc_mgrp_create : Value.t -> int -> Value.t
  val mc_node_create : Value.t -> int -> int list -> Value.t
  val mc_node_associate : Value.t -> int -> int -> Value.t

  (* Register interface *)

  val register_read : Value.t -> string -> int -> Value.t
  val register_write : Value.t -> string -> int -> int -> Value.t
  val register_reset : Value.t -> string -> Value.t

  (* Pipeline evaluation *)

  val init_pipe : string list -> string -> Value.t * Value.t
  val drive_pipe : Value.t -> Value.t -> IO.rx -> Value.t * Value.t * IO.tx list

  (* Extern relation and meta-function evaluation *)

  val eval_extern_rel : string -> Value.t list -> rel_result
  val eval_extern_func : string -> Typ.t list -> Value.t list -> func_result
end

module type SIM = sig
  include RUNNER

  (* Run a program against the spec and a STF test (For P4 only) *)

  val run_stf_test : string list -> string -> string -> stf_result
end
