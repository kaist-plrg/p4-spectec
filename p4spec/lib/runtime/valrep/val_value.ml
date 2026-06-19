(* Interpreter representation: t = Value.t, ops alias the existing Value API. *)

module Value = Runtime.Value
module Typ = Runtime.Type.Typ
module Mixfix = Domain.Mixfix
module Il = Lang.Il
module Num = Lang.Xl.Num
open Util.Source

module V_value : Sig.VAL with type t = Value.t = struct
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
