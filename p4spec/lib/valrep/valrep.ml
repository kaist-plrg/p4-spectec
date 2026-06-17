(* Abstract value representation at the compiled-spec <-> extern boundary.

   Externs (and the Spec_Func/Spec_Rel trampolines) are functorized over [VAL]
   so the same extern code runs against two representations:
     - [V_value]: t = Value.t, for the IL/SL interpreter (native Value.t).
     - V_typed (later): t = Obj.t boxed typed values, for the compiled spec,
       so boundary crossings are O(1) box/unbox instead of deep marshal/unmarshal.

   The surface is exactly what backend-sim uses (see survey): ~8 destructors and
   ~8 constructors plus the case-construction operators.

   This signature + the [V_value] interpreter instance live in their own [valrep]
   lib (below [interface] and [backend-sim]) so that both the [builtin] lib (which
   functorizes its builtins over [VAL]) and [backend-sim] (which defines [V_typed]
   over the generated [Spec_parts]) can reference the same currency. [V_typed]
   stays in [backend-sim] — it needs the generated symbols, which sit above this
   lib. See API.md §10 (D1). *)

module Value = Runtime.Value
module Typ = Runtime.Type.Typ
module Mixfix = Domain.Mixfix
module Mixop = Domain.Mixop
module Il = Lang.Il
module Num = Lang.Xl.Num
open Util.Source

module type VAL = sig
  type t

  val to_string : t -> string

  (* Total order / equality over values of a (statically known) spec type, for
     the set/map builtins. [V_value] = [Value.compare]/[Value.eq] (ignores the
     type). [V_typed] marshals both operands to [Value.t] via the type name then
     compares (cold path: only the set/map builtins, not packet arithmetic),
     keeping the serialized element order identical to [V_value] (API.md D2). The
     type name is required because a typed [Obj.t] is type-erased — it picks the
     right [marshal_<typ>] (B5); the set/map builtins thread it from their
     element-type targ. *)
  val compare : string -> t -> t -> int
  val equal : string -> t -> t -> bool

  (* [to_value]/[of_value]: the TRANSIENT smuggle across a [Value.t]-typed
     interface where the value is handed straight back to compiled code and never
     decoded (the runner bridge, [init_pipe]). Identity under both reps. *)
  val to_value : t -> Value.t
  val of_value : Value.t -> t

  (* [marshal]/[unmarshal]: the PERSIST bridge for a value of (statically known)
     spec type [typ] that is STORED into a concrete [Value.t]-typed, yojson-
     serialized field (scheduler [Packet.value_ctx], register payloads) and
     decoded later. Identity under [V_value]; a REAL per-type
     marshal/unmarshal under [V_typed], because the stored [Obj.t] must become an
     honest [Value.t] before something serializes it (see Make.extern/to_yojson).
     [typ] is the marshal interface name, e.g. "eval_context" / "value". *)
  val marshal : string -> t -> Value.t
  val unmarshal : string -> Value.t -> t

  module Get : sig
    val text : t -> string
    val num : t -> Num.t
    val bool : t -> bool
    val list : t -> t list
    val opt : t -> t option
    val tuple : t -> t list

    (* [case]/[( |>>? )] take the value's spec type as a structured [Il.typ] (and
       the mixop as a structured [Il.mixop]). [V_value] ignores the typ (every
       [Value.t] case carries its mixop tag at runtime); [V_typed] needs it to
       pick the OCaml variant projection, since a bare [Obj.t] is type-erased.
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

    (* case construction DSL: "mixop" <| args <<| typ. [( <| )] parses the
       readable mixop string into a structured [Il.mixop]; [( <<| )] takes the
       value's spec type as a structured [Il.typ]. *)
    val ( <| ) : string -> t list -> Il.mixop * t list
    val ( <<| ) : Il.mixop * t list -> Il.typ -> t
  end
end

(* Interpreter representation: t = Value.t, ops alias the existing Value API. *)
module V_value : VAL with type t = Value.t = struct
  type t = Value.t

  let to_string = Value.to_string
  let compare _typ = Value.compare
  let equal _typ = Value.eq
  let to_value = Fun.id
  let of_value = Fun.id

  (* [V_value]'s values are already [Value.t], so the persist bridge is identity;
     the spec type name is irrelevant. *)
  let marshal (_typ : string) (x : t) : Value.t = x
  let unmarshal (_typ : string) (v : Value.t) : t = v

  module Get = struct
    let text = Value.Get.text
    let num = Value.Get.num
    let bool = Value.Get.bool
    let list = Value.Get.list
    let opt = Value.Get.opt
    let tuple = Value.Get.tuple

    (* The [Value.t] case carries its own mixop tag, so the spec type is
       redundant here and ignored. *)
    let case (x : t) (_typ : Il.typ) : t Mixfix.t = Value.Get.case x
    let extern = Value.Get.extern
    let nth = Value.Get.nth
    let one = Value.Get.one
    let two = Value.Get.two
    let three = Value.Get.three
    let ( |>> ) = Value.Get.( |>> )

    let ( |>>? ) (x : t) ((mixop, _typ) : Il.mixop * Il.typ) : t list option =
      Value.Get.( |>>?! ) x mixop
  end

  module Make = struct
    let text = Value.Make.text
    let int = Value.Make.int
    let nat = Value.Make.nat
    let bool = Value.Make.bool
    let opt = Value.Make.opt
    let list = Value.Make.list
    let tuple = Value.Make.tuple
    let extern = Value.Make.extern

    (* Parse the readable mixop string into a structured [Il.mixop]. *)
    let ( <| ) (s_mixop : string) (args : t list) : Il.mixop * t list =
      (Value.Mixops.of_string s_mixop, args)

    (* Build the [Value.t] case from the structured mixop + spec type, deriving
       the region from the args exactly as [Value.Make.( <<| )] did. *)
    let ( <<| ) ((mixop, args) : Il.mixop * t list) (typ : Il.typ) : t =
      let valuecase = Mixfix.fill mixop args in
      let at =
        args |> List.map at
        |> List.filter (fun region -> region <> no_region)
        |> over_region
      in
      Value.Make.case ~at typ valuecase
  end
end
