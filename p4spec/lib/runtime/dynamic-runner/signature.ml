open Domain.Lib
open Lang
module Typ = Type.Typ
open Util.Source

(* Module signatures for interpreter-extern interaction *)

type mode = IL_mode | SL_mode | Empty_mode
type spec = IL of Il.spec | SL of Sl.spec | Empty

(* Result types *)

type rel_result = Pass of Value.t list | Fail of region * string
type func_result = Pass of Value.t | Fail of region * string
type parse_result = Pass of Value.t | Fail of [ `Syntax of region * string ]

type program_result =
  | Pass of Value.t list
  | Fail of [ `Syntax of region * string | `Runtime of region * string ]

type stf_result =
  | Pass
  | Fail of [ `Syntax of region * string | `Runtime of region * string ]

module type INTERFACE = sig
  (* Program parsing, into IL value *)

  val parse_program : string list -> string list -> parse_result
  val parse_string : string -> string -> parse_result

  (* Program unparsing *)

  val unparse_program : Value.t -> string

  (* Builtins *)

  val call_builtin :
    (Value.t -> unit) -> Id.t -> Typ.t list -> Value.t list -> Value.t

  (* State management *)

  val checkpoint : unit -> int
  val seff : int -> int -> bool

  (* Initialization *)

  val init : spec -> unit
end

module type EXTERN = sig
  (* Extern relation and meta-function evaluation *)

  val eval_extern_rel : string -> Value.t list -> rel_result
  val eval_extern_func : string -> Typ.t list -> Value.t list -> func_result

  (* Mode initialization *)

  val init_mode : mode -> unit

  (* Clear the cache *)

  val clear : unit -> unit
end

module type INTERP_IL = sig
  (* Relation and meta-function evaluation *)

  val eval_program : string -> string list -> string -> program_result
  val eval_rel : string -> Value.t list -> rel_result
  val eval_func : string -> Il.typ list -> Value.t list -> func_result

  (* Initialization *)

  val init : cache:bool -> det:bool -> Il.spec -> unit

  (* Clear the cache *)

  val clear : unit -> unit
end

module type INTERP_SL = sig
  (* An entry point for running a closed program against the spec *)

  val eval_program : string -> string list -> string -> program_result

  (* Relation and meta-function evaluation *)

  val eval_rel : string -> Value.t list -> rel_result
  val eval_func : string -> Sl.typ list -> Value.t list -> func_result

  (* Initialization *)

  val init : cache:bool -> det:bool -> Sl.spec -> unit

  (* Clear the cache *)

  val clear : unit -> unit
end

module type RUNNER = sig
  (* Run a program against the spec *)

  val run_program : string -> string list -> string -> program_result
  val run_program_internal : string -> Value.t -> rel_result

  (* Relation and meta-function evaluation *)

  val run_rel : string -> Value.t list -> rel_result
  val run_func : string -> Sl.typ list -> Value.t list -> func_result

  (* Parsing *)

  val parse_file : string list -> string list -> parse_result
  val parse_string : string -> string -> parse_result

  (* Unparsing *)

  val unparse_program : Value.t -> string

  (* State management *)

  val checkpoint : unit -> int
  val seff : int -> int -> bool

  (* Initialization *)

  val init : ?cache:bool -> ?det:bool -> spec -> unit

  (* Clear the cache *)

  val clear : unit -> unit
end
