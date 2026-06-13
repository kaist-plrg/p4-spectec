(* Abstract value representation at the compiled-spec <-> extern boundary.

   Externs (and the Spec_Func/Spec_Rel trampolines) are functorized over [VAL]
   so the same extern code runs against two representations:
     - [V_value]: t = Value.t, for the IL/SL interpreter (native Value.t).
     - V_typed (later): t = Obj.t boxed typed values, for the compiled spec,
       so boundary crossings are O(1) box/unbox instead of deep marshal/unmarshal.

   The surface is exactly what backend-sim uses (see survey): ~8 destructors and
   ~8 constructors plus the case-construction operators. *)

module Value = Runtime.Value
module Typ = Runtime.Type.Typ
module Mixfix = Domain.Mixfix
module Num = Lang.Xl.Num
open Util.Source

module type VAL = sig
  type t

  module Get : sig
    val text : t -> string
    val num : t -> Num.t
    val bool : t -> bool
    val list : t -> t list
    val opt : t -> t option
    val tuple : t -> t list
    val case : t -> t Mixfix.t
    val extern : t -> Yojson.Safe.t
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

    (* case construction DSL: "mixop" <| args <<| "typename" *)
    val ( <| ) : string -> t list -> string * t list
    val ( <<| ) : string * t list -> string -> t
  end
end

(* Interpreter representation: t = Value.t, ops alias the existing Value API. *)
module V_value : VAL with type t = Value.t = struct
  type t = Value.t

  module Get = struct
    let text = Value.Get.text
    let num = Value.Get.num
    let bool = Value.Get.bool
    let list = Value.Get.list
    let opt = Value.Get.opt
    let tuple = Value.Get.tuple
    let case = Value.Get.case
    let extern = Value.Get.extern
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
    let ( <| ) = Value.Make.( <| )
    let ( <<| ) = Value.Make.( <<| )
  end
end
