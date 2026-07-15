(* The value-representation contracts at the compiled-spec <-> extern boundary.

   Three signatures, layered by capability:
     - [SAFE]: the structural contract (to_string, marshal, Get, Make). The
       [builtin] and [interface] layers and the structural [backend-sim]
       functors are generic over [SAFE], so the same code runs against two
       value representations and none of them can reinterpret a value.
     - [UNSAFE]: the unchecked reinterpret across the runner edge (see below).
     - [VAL]: [SAFE] + [UNSAFE] — what a concrete representation actually provides.

   The two representations:
     - [V_value] (here in [valrep]): t = Value.t, for the IL/SL interpreter.
     - [V_native] (in backend-ocaml-p4): t = Obj.t holding native compiled
       values, so a boundary crossing is an O(1) box/unbox instead of a deep
       conversion.

   These signatures sit at the bottom of the boundary stack so [builtin],
   [interface], and [backend-sim] can all be functors over them. [V_native]
   stays in [backend-ocaml-p4] because its body needs the generated symbols. *)

module Value = Value
module Typ = Type.Typ
module Mixfix = Domain.Mixfix
module Mixop = Domain.Mixop
module Il = Lang.Il
module Num = Lang.Xl.Num
open Util.Source

module type SAFE = sig
  type t

  (* The value's source region, when the representation carries one.
     [V_value] returns the real region; [V_native] has none (compiled
     values carry no per-node location), so it returns [no_region]. *)
  val at : t -> region

  (* [marshal]/[unmarshal]: convert a value of (statically known) spec type
     [typ] to/from a real [Value.t] when it must be stored in a serialized field
     (scheduler [Packet.value_ctx], register payloads). Identity under
     [V_value]; a real per-type conversion under [V_native].

     [marshal] is also the only way to read a typed value's structure, so the
     set/map builtins derive element comparison from it, not from [Raw.to_value]
     (a type-erased identity cast under [V_native]). *)
  val marshal : Typ.t -> t -> Value.t
  val unmarshal : Typ.t -> Value.t -> t

  module Get : sig
    val text : t -> string
    val num : t -> Num.t
    val bool : t -> bool
    val list : t -> t list
    val opt : t -> t option
    val tuple : t -> t list

    (* [case]/[( |>>? )] take the value's spec type as an [Il.typ] and the mixop
       as an [Il.mixop]. [V_value] ignores the typ (each [Value.t] case carries
       its mixop tag); [V_native] needs it to pick the OCaml variant projection.
       [( |>>? )] takes [(mixop, typ)] as a pair so it stays usable infix. *)
    val case : t -> Il.typ -> t Mixfix.t
    val extern : t -> Yojson.Safe.t

    (* extractors / case-nav operators *)
    val nth : int -> t list -> t
    val one : t list -> t
    val two : t list -> t * t
    val three : t list -> t * t * t
    val ( |>> ) : t -> string -> t list
    val ( |>>? ) : t -> Il.mixop * Il.typ -> t list option
  end

  module Make : sig
    val text : ?at:region -> string -> t
    val int : ?at:region -> Bigint.t -> t
    val nat : ?at:region -> Bigint.t -> t
    val bool : ?at:region -> bool -> t
    val opt : ?at:region -> Typ.t -> t option -> t
    val list : ?at:region -> Typ.t -> t list -> t
    val tuple : ?at:region -> Typ.t -> t list -> t
    val extern : ?at:region -> Typ.t -> Yojson.Safe.t -> t

    (* case construction: "mixop" <| args <<| typ. [( <<| )]'s [~at] is
       optional since [V_native] has no per-node region to carry it in. *)
    val ( <| ) : string -> t list -> Il.mixop * t list
    val ( <<| ) : ?at:region -> Il.mixop * t list -> Il.typ -> t
  end
end

(* [UNSAFE]: the unchecked reinterpret across the [Value.t]-typed dynamic-runner
   edge. [to_value]/[of_value] pass a value straight back to compiled code,
   where it is never decoded — identity under [V_value], a bare [Obj.obj]/
   [Obj.repr] under [V_native]. This is the dangerous escape hatch, so it is
   deliberately NOT part of [SAFE]: the generic [builtin] layer and the
   structural [backend-sim] functors only ever see [SAFE] and so cannot cast.
   Only the per-arch boundary that owns the runner edge ([extern], and the pipe
   initializers via [Spec.V]) is handed a [UNSAFE]/[VAL]. *)
module type UNSAFE = sig
  type t

  val to_value : t -> Value.t
  val of_value : Value.t -> t
end

(* [VAL]: what a concrete value representation actually provides — the [SAFE]
   contract plus the [UNSAFE] escape hatch. [V_value]/[V_native] satisfy it, and
   [Spec.V] is typed at it so the arch/pipe layer keeps cast access. *)
module type VAL = sig
  include SAFE
  include UNSAFE with type t := t
end
