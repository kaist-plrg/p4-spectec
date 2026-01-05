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

type spec = IL of Il.spec | SL of Sl.spec | Empty
type cover_single = { instr : ICov_single.t; dangling : DCov_single.t }
type coverage = Cover of cover_single | Empty

(* IL results *)

type rel_result_il = Pass of Value.t list | Fail of region * string
type func_result_il = Pass of Value.t | Fail of region * string

type program_result_il =
  | Pass of Value.t list
  | Fail of region * string
  | IllFormed of region * string

type stf_result_il =
  | Pass
  | Fail of region * string
  | IllFormed of region * string

(* SL results *)

type rel_result_sl =
  | Pass of Value.t list * cover_single
  | Fail of region * string * cover_single

type func_result_sl =
  | Pass of Value.t * cover_single
  | Fail of region * string * cover_single

type program_result_sl =
  | Pass of Value.t list * Dep.Graph.t * cover_single
  | Fail of region * string * cover_single
  | IllFormed of region * string * cover_single

type stf_result_sl =
  | Pass of cover_single
  | Fail of region * string * cover_single
  | IllFormed of region * string * cover_single

(* Merged results *)

type program_result = program_result_il
type stf_result = stf_result_il

let promote_program_result_sl (program_result_sl : program_result_sl) :
    program_result =
  match program_result_sl with
  | Pass (values_output, _, _) -> Pass values_output
  | Fail (at, msg, _) -> Fail (at, msg)
  | IllFormed (at, msg, _) -> IllFormed (at, msg)

let promote_stf_result_sl (stf_result_sl : stf_result_sl) : stf_result =
  match stf_result_sl with
  | Pass _ -> Pass
  | Fail (at, msg, _) -> Fail (at, msg)
  | IllFormed (at, msg, _) -> IllFormed (at, msg)

module type ARCH = sig
  (* Coverage *)

  val spec : spec ref
  val coverage : coverage ref

  (* Extern evaluation *)

  val eval_extern_init : Value.t list -> Value.t
  val eval_extern_func_lctk_call : Value.t list -> Value.t list
  val eval_extern_func_call : Value.t list -> Value.t list
  val eval_extern_method_call : Value.t list -> Value.t list

  (* Match-action table interface *)

  val table_add_entry :
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

  (* Pipeline evaluation *)

  val init : spec -> unit
  val init_pipe : spec -> string list -> string -> Value.t * Value.t

  val drive_pipe :
    Value.t -> Value.t -> IO.rx -> Value.t * Value.t * IO.tx option
end

module type INTERP_IL = sig
  (* Relation and meta-function evaluation *)

  val eval_program :
    Il.spec -> string -> string list -> string -> program_result_il

  val eval_rel : Il.spec -> string -> Value.t list -> rel_result_il

  val eval_func :
    Il.spec -> string -> Il.typ list -> Value.t list -> func_result_il
end

module type INTERP_SL = sig
  (* Relation and meta-function evaluation *)

  val eval_program :
    derive:bool ->
    Sl.spec ->
    string ->
    string list ->
    string ->
    program_result_sl

  val eval_rel : Sl.spec -> string -> Value.t list -> rel_result_sl

  val eval_func :
    Sl.spec -> string -> Sl.typ list -> Value.t list -> func_result_sl

  (* Coverage *)

  val cover_instr_programs :
    Sl.spec -> string -> string list -> string list -> ICov_multi.t

  val cover_dangling_programs :
    Sl.spec -> string -> string list -> string list -> DCov_multi.t
end

module type DRIVER = sig
  (* Run a P4 program against the spec *)

  val run_program_il :
    derive:bool ->
    Il.spec ->
    string ->
    string list ->
    string ->
    program_result_il

  val run_program_sl :
    derive:bool ->
    Sl.spec ->
    string ->
    string list ->
    string ->
    program_result_sl

  val run_program :
    derive:bool -> spec -> string -> string list -> string -> program_result

  val run_program_internal :
    derive:bool -> Sl.spec -> string -> Value.t -> rel_result_sl

  (* Run a P4 program against the spec and a STF test *)

  val run_stf_test_il :
    Il.spec -> string list -> string -> string -> stf_result_il

  val run_stf_test_sl :
    Sl.spec -> string list -> string -> string -> stf_result_sl

  val run_stf_test : spec -> string list -> string -> string -> stf_result

  (* Coverage *)

  val cover_instr_programs :
    Sl.spec -> string -> string list -> string list -> ICov_multi.t

  val cover_instr_stfs :
    Sl.spec -> string list -> string list -> string list -> ICov_multi.t

  val cover_dangling_programs :
    Sl.spec -> string -> string list -> string list -> DCov_multi.t
end
