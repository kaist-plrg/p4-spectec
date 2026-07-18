open Domain.Lib
open Lang
module Typ = Type.Typ
open Util.Source

(* Module signatures for interpreter-extern interaction *)

type mode = IL_mode | SL_mode | ML_mode | Empty_mode
type spec = IL of Il.spec | SL of Sl.spec | ML | Empty

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

(* Cache management *)

module type CACHE = sig
  val cache_on : unit -> unit
  val cache_off : unit -> unit
end

(* Builtins, at an arbitrary value representation [V] *)

module type BUILTINS = sig
  type vt

  val invoke : (vt -> unit) -> Id.t -> Typ.t list -> vt list -> vt
  val init : unit -> unit
  val checkpoint : unit -> int
  val seff : int -> int -> bool
end

(* Interface for the interaction between SpecTec and the defined language *)

module type INTERFACE = sig
  (* The interface's builtins at any value rep; [call_builtin] is
     [Builtins(Valrep.V_value).invoke]. *)

  module Builtins (V : Valrep.SAFE) : BUILTINS with type vt = V.t

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

  val init_mode : mode -> unit
end

(* SpecTec interperter(s) *)

module type INTERP = sig
  module Cache : CACHE

  (* Relation and meta-function evaluation *)

  val eval_program : string -> string list -> string -> program_result
  val eval_rel : string -> Value.t list -> rel_result
  val eval_func : string -> Typ.t list -> Value.t list -> func_result

  val unmarshal_program : Value.t -> Value.t

  (* Clear the state *)

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

module type INTERP_ML = sig
  include INTERP

  (* Native-side dispatch: same shape as [eval_func]/[eval_rel], but for
     callers that already hold typed [Obj.t] (compiler-val's own
     [backend-sim] glue, calling back into the spec by name) rather than a
     real [Value.t] — IL/SL have no such caller, hence ML-only. *)

  val eval_func_native : string -> Typ.t list -> Value.t list -> func_result
  val eval_rel_native : string -> Value.t list -> rel_result

  (* Initialization *)

  val init : cache:bool -> det:bool -> guard:bool -> unit -> unit
end

(* Runner for SpecTec, which glues together the interface, the extern, and the interpreter *)

module type RUNNER = sig
  module Cache : CACHE
  module Interface : INTERFACE
  module Interp : INTERP

  (* Initialization *)

  val init : ?cache:bool -> ?det:bool -> ?guard:bool -> spec -> unit

  (* Clear the state *)

  val clear : unit -> unit
end
