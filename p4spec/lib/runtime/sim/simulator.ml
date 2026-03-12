open Lang
module Value = Dynamic.Value
module Dep = Testgen_neg.Dep
module ICov_single = Coverage.Instr.Single
module ICov_multi = Coverage.Instr.Multi
module DCov_single = Coverage.Dangling.Single
module DCov_multi = Coverage.Dangling.Multi
module IO = Io
open Util.Source

(* Module signatures for interpreter-architecture interaction *)

type mode = IL_mode | SL_mode | Empty_mode
type spec = IL of Il.spec | SL of Sl.spec | Empty
type rel_result = Pass of Value.t list | Fail of region * string
type func_result = Pass of Value.t | Fail of region * string

type program_result =
  | Pass of Value.t list
  | Fail of [ `Syntax of region * string | `Runtime of region * string ]

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

  (* Match-action table interface *)

  val table_add_entry :
    Value.t ->
    (* context *)
    Value.t ->
    (* store *)
    Value.t ->
    (* table name *)
    Value.t ->
    (* table entry priority *)
    Value.t ->
    (* table entry keysets *)
    Value.t ->
    (* table entry action *)
    Value.t (* store *)

  val table_add_default_action :
    Value.t ->
    (* context *)
    Value.t ->
    (* store *)
    Value.t ->
    (* table name *)
    Value.t ->
    (* table entry action *)
    Value.t (* store *)

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

  (* Initialization *)

  val init : mode -> unit
end

module type INTERP_IL = sig
  (* Relation and meta-function evaluation *)

  val eval_program : string -> string list -> string -> program_result
  val eval_rel : string -> Value.t list -> rel_result
  val eval_func : string -> Il.typ list -> Value.t list -> func_result

  (* Initialization *)

  val init : cache:bool -> det:bool -> Il.spec -> unit
end

module type INTERP_SL = sig
  (* Relation and meta-function evaluation *)

  val eval_program : string -> string list -> string -> program_result
  val eval_rel : string -> Value.t list -> rel_result
  val eval_func : string -> Sl.typ list -> Value.t list -> func_result

  (* Initialization *)

  val init : cache:bool -> det:bool -> Sl.spec -> unit
end

module type DRIVER = sig
  (* Run a P4 program against the spec *)

  val run_program : string -> string list -> string -> program_result
  val run_program_internal : string -> Value.t -> rel_result

  (* Run a P4 program against the spec and a STF test *)

  val run_stf_test : string list -> string -> string -> stf_result

  (* Initialization *)

  val init : ?cache:bool -> ?det:bool -> spec -> unit
end
