open Domain.Lib
open Lang
module Typ = Type.Typ

(* Module signatures for interpreter-extern interaction *)

type mode = AL_mode | SL_mode | PL_mode | Empty_mode
type spec = AL of Al.spec | SL of Sl.spec | PL of Pl.spec | Empty

(* Error *)

type error = Diagnostic.t

(* Failure reported by the entry points below: an abort that propagates as
   is, or an unmatch that re-enters the caller's backtracking *)

type failure = Abort of error | Unmatch of Util.Attempt.failtrace list

(* Constructor for failure *)

let abort ~source at msg = Abort (Diagnostic.error ~source at msg)

(* Unmatch failtraces are rendered only by [diagnostic_of_failure]. *)

let diagnostic_of_failure = function
  | Abort diagnostic -> diagnostic
  | Unmatch failtraces ->
      Diagnostic.of_failtraces ~source:"interp" ~fallback:"evaluation failed"
        failtraces

(* Raised by an extern implementation, caught by its caller *)

exception ExternError of failure

(* Result types *)

type ('value, 'failure) outcome = Pass of 'value | Fail of 'failure
type rel_result = (Value.t list, failure) outcome
type func_result = (Value.t, failure) outcome
type parse_result = (Value.t, error) outcome
type program_failure = [ `Syntax of error | `Runtime of failure ]
type program_result = (Value.t list, program_failure) outcome

(* Cache management *)

module type CACHE = sig
  val cache_on : unit -> unit
  val cache_off : unit -> unit
end

(* Interface for the interaction between SpecTec and the defined language *)

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

  val init : spec -> (unit, error) result
end

(* Interface for the interaction between SpecTec and external code *)

module type EXTERN = sig
  module Cache : CACHE

  (* Extern relation and meta-function evaluation *)

  val eval_extern_rel : string -> Value.t list -> rel_result
  val eval_extern_func : string -> Typ.t list -> Value.t list -> func_result

  (* State management *)

  val checkpoint : unit -> int
  val seff : int -> int -> bool
  val clear : unit -> unit

  (* Mode initialization for interp-extern knot *)

  val init_mode : mode -> (unit, error) result
end

(* SpecTec interperter(s) *)

module type INTERP = sig
  module Cache : CACHE

  (* Relation and meta-function evaluation *)

  val eval_program : string -> string list -> string -> program_result
  val eval_rel : string -> Value.t list -> rel_result
  val eval_func : string -> Typ.t list -> Value.t list -> func_result

  (* Clear the state *)

  val clear : unit -> unit
end

module type INTERP_AL = sig
  include INTERP

  (* Initialization *)

  val init :
    cache:bool -> det:bool -> guard:bool -> Al.spec -> (unit, error) result
end

module type INTERP_SL = sig
  include INTERP

  (* Initialization *)

  val init :
    cache:bool -> det:bool -> guard:bool -> Sl.spec -> (unit, error) result
end

module type INTERP_PL = sig
  include INTERP

  (* Initialization *)

  val init :
    cache:bool -> det:bool -> guard:bool -> Pl.spec -> (unit, error) result
end

(* Runner for SpecTec, which glues together the interface, the extern, and the interpreter *)

module type RUNNER = sig
  module Cache : CACHE
  module Interface : INTERFACE
  module Interp : INTERP

  (* Initialization *)

  val init :
    ?cache:bool -> ?det:bool -> ?guard:bool -> spec -> (unit, error) result

  (* Clear the state *)

  val clear : unit -> unit
end
