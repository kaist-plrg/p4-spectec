module IO = Io
open Util.Source

(* Module signatures for interpreter-architecture simulation *)

include Dynamic_runner.Signature

type stf_result =
  | Pass
  | Fail of [ `Syntax of region * string | `Runtime of region * string ]

module type ARCH = sig
  (* Abstract value representation crossing the compiled-spec <-> extern
     boundary. [V_value] pins [vt = Value.t]; [V_typed] pins [vt = Obj.t]. *)

  type vt

  (* STF AST transformation *)

  val transform_stf_stmt : Stf.Ast.stmt -> Stf.Ast.stmt

  (* Extern evaluation *)

  val eval_extern_init : vt list -> vt
  val eval_extern_func_lctk_call : vt list -> vt list
  val eval_extern_func_call : vt list -> vt list
  val eval_extern_method_call : vt list -> vt list

  (* Architecture-specific external state *)

  val init_arch_state : vt

  (* Mirror session interface *)

  val add_mirror_session : vt -> int -> int -> vt
  val add_mirror_session_mc : vt -> int -> int -> vt

  (* Multicast interface *)

  val mc_mgrp_create : vt -> int -> vt
  val mc_node_create : vt -> int -> int list -> vt
  val mc_node_associate : vt -> int -> int -> vt

  (* Register interface *)

  val register_read : vt -> string -> int -> vt
  val register_write : vt -> string -> int -> int -> vt
  val register_reset : vt -> string -> vt

  (* Pipeline evaluation *)

  val init_pipe : string list -> string -> vt * vt
  val drive_pipe : vt -> vt -> IO.rx -> vt * vt * IO.tx list

  (* Extern relation and meta-function evaluation (bridge to the dynamic
     runner, which is [Value.t]-typed). *)

  val eval_extern_rel : string -> Value.t list -> rel_result
  val eval_extern_func : string -> Typ.t list -> Value.t list -> func_result
end

module type SIM = sig
  include RUNNER

  (* Run a program against the spec and a STF test (For P4 only) *)

  val run_stf_test : string list -> string -> string -> stf_result
end
