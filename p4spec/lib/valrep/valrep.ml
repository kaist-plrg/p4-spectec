(* Abstract value representation at the compiled-spec <-> extern boundary.

   The extern and builtin layers are generic over [VAL] so the same code runs
   against two value representations:
     - [V_value]: t = Value.t, for the IL/SL interpreter.
     - V_typed (in backend-sim): t = Obj.t holding native compiled values, so a
       boundary crossing is an O(1) box/unbox instead of a deep conversion.

   This lib sits below [interface] and [backend-sim] so both can share it.
   [V_typed] stays in [backend-sim] because it needs the generated symbols. *)

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

  (* [to_value]/[of_value]: pass a value across a [Value.t]-typed interface
     straight back to compiled code, where it is never decoded. Identity under
     both representations. *)
  val to_value : t -> Value.t
  val of_value : Value.t -> t

  (* [marshal]/[unmarshal]: convert a value of (statically known) spec type
     [typ] to/from a real [Value.t] when it must be stored in a serialized field
     (scheduler [Packet.value_ctx], register payloads). Identity under
     [V_value]; a real per-type conversion under [V_typed].

     [marshal] is also the only way to read a typed value's structure, so the
     set/map builtins derive element comparison from it, not from [to_value]
     (a type-erased identity cast under [V_typed]). *)
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
       its mixop tag); [V_typed] needs it to pick the OCaml variant projection.
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

    (* case construction: "mixop" <| args <<| typ. [( <| )] parses the mixop
       string into an [Il.mixop]; [( <<| )] takes the spec type as an [Il.typ]. *)
    val ( <| ) : string -> t list -> Il.mixop * t list
    val ( <<| ) : Il.mixop * t list -> Il.typ -> t
  end
end

(* Interpreter representation: t = Value.t, ops alias the existing Value API. *)
module V_value : VAL with type t = Value.t = struct
  type t = Value.t

  let to_string = Value.to_string
  let to_value = Fun.id
  let of_value = Fun.id

  (* [V_value] is already [Value.t], so this is the identity and the spec type
     is irrelevant. *)
  let marshal (_typ : Typ.t) (x : t) : Value.t = x
  let unmarshal (_typ : Typ.t) (v : Value.t) : t = v

  module Get = struct
    let text = Value.Get.text
    let num = Value.Get.num
    let bool = Value.Get.bool
    let list = Value.Get.list
    let opt = Value.Get.opt
    let tuple = Value.Get.tuple

    (* The [Value.t] case carries its own mixop tag, so the spec type is
       ignored here. *)
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

    (* Build the [Value.t] case from the mixop + spec type, taking the region
       from the args. *)
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
