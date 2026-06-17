module Num = Lang.Xl.Num
module Mixfix = Domain.Mixfix
module Mixop = Domain.Mixop
module Value = Runtime.Value

(* Unpacks an IL value representing a P4 value into an OCaml type *)

module Make (V : Valrep.VAL) = struct
  type vt = V.t

  let first fs x = List.find_map (fun f -> f x) fs

  (* boolValue = `B bool *)

  let unpack_p4_bool (value : vt) : bool =
    V.Get.(value |>> "`B bool" |> one |> bool)

  (* errorValue = ERROR `. id *)
  (* matchKindValue = MATCH_KIND `. id *)
  (* stringValue = stringLiteral *)

  let unpack_p4_string (value : vt) : string =
    V.Get.(value |>> "`\" text `\"" |> one |> text)

  (* D int *)

  (* nat W int *)

  let unpack_p4_fixedBit (value : vt) : Bigint.t * Bigint.t =
    V.Get.(
      value |>> "nat W int" |> two |> fun (value_width, value_int) ->
      let width = value_width |> num |> Num.to_int in
      let int = value_int |> num |> Num.to_int in
      (width, int))

  (* nat S int *)

  let unpack_p4_fixedInt (value : vt) : Bigint.t * Bigint.t =
    V.Get.(
      value |>> "nat S int" |> two |> fun (value_width, value_int) ->
      let width = value_width |> num |> Num.to_int in
      let int = value_int |> num |> Num.to_int in
      (width, int))

  (* nat `. nat V int *)

  let unpack_p4_variableBit (value : vt) : Bigint.t * Bigint.t * Bigint.t =
    V.Get.(
      value |>> "nat `. nat V int" |> three
      |> fun (value_width_max, value_width, value_int) ->
      let width_max = value_width_max |> num |> Num.to_int in
      let width = value_width |> num |> Num.to_int in
      let int = value_int |> num |> Num.to_int in
      (width_max, width, int))

  let unpack_p4_precision_numberValue (value : vt) : Bigint.t * Bigint.t =
    (* Dispatch on the actual [value] constructor. [V_typed]'s [( |>> )] projects
       by arity WITHOUT validating the constructor, so the previous
       [try unpack_p4_fixedBit with _ -> ...] fallback is dead under [V_typed] and
       silently mis-reads e.g. a variableBit ("nat `. nat V int", 3 args) as a
       fixedBit ("nat W int", 2 args), yielding the wrong (width, value). *)
    let mixop, _ = Mixfix.split (V.Get.case value Typs.value) in
    let canon s = Mixop.string_of_mixop (Value.Mixops.of_string s) in
    let m = Mixop.string_of_mixop mixop in
    if m = canon "nat W int" then unpack_p4_fixedBit value
    else if m = canon "nat S int" then unpack_p4_fixedInt value
    else
      let _, width, int = unpack_p4_variableBit value in
      (width, int)

  (* listValue = LIST `[ value* ] *)

  (* tupleValue = TUPLE `( value* ) *)

  let unpack_p4_tuple (value : vt) : vt list =
    V.Get.(value |>> "TUPLE `( value* )" |> one |> list)

  (* headerStackValue = HEADER_STACK `[ value* `( nat; nat ) ] *)
  (* structValue = STRUCT tid `{ fieldValue* } *)
  (* headerValue = HEADER tid `{ bool `; fieldValue* } *)
  (* headerUnionValue = HEADER_UNION tid `{ fieldValue* } *)

  (* tid `. id *)

  let unpack_p4_enum (value : vt) : string * string =
    V.Get.(
      value |>> "tid `. id" |> two |> fun (value_tid, value_id) ->
      let tid = value_tid |> text in
      let id = value_id |> text in
      (tid, id))

  (* tid `. id `. value *)

  (* objectReferenceValue = `! oid *)
  (* defaultValue = DEFAULT *)

  (* SEQ `( value* ) *)

  let unpack_p4_sequence (value : vt) : vt list =
    V.Get.(value |>> "SEQ `( value* )" |> one |> list)

  (* SEQ `( value* `, `... ) *)
  (* RECORD `{ fieldValue* } *)
  (* RECORD `{ fieldValue* `, `... } *)
  (* SET `{ value } *)
  (* SET `{ value `&&& value } *)
  (* SET `{ value `.. value } *)
  (* SET `{ `... } *)
  (* TABLE_ENUM tid `. id *)
  (* TABLE_STRUCT tid `{ fieldValue* } *)
end
