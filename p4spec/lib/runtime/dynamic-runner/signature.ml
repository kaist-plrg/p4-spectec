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

  (* State management *)

  val checkpoint : unit -> int
  val seff : int -> int -> bool

  (* Clear the cache *)

  val clear : unit -> unit
end

module type INTERP = sig
  (* Relation and meta-function evaluation *)

  val eval_program : string -> string list -> string -> program_result
  val eval_rel : string -> Value.t list -> rel_result
  val eval_func : string -> Typ.t list -> Value.t list -> func_result

  (* Clear the cache *)

  val clear : unit -> unit
end

module type INTERP_IL = sig
  include INTERP

  (* Initialization *)

  val init : cache:bool -> det:bool -> guard:bool -> Il.spec -> unit
end

module type INTERP_SL = sig
  include INTERP

  (* Initialization *)

  val init : cache:bool -> det:bool -> guard:bool -> Sl.spec -> unit
end

module type RUNNER = sig
  module Interface : INTERFACE
  module Interp : INTERP

  (* Initialization *)

  val init : ?cache:bool -> ?det:bool -> ?guard:bool -> spec -> unit

  (* Clear the cache *)

  val clear : unit -> unit
end
