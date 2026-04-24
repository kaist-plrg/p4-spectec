module Num = Lang.Xl.Num
module Value = Runtime.Value

(* Unpacks an IL value representing a P4 value into an OCaml type *)

let first fs x = List.find_map (fun f -> f x) fs

(* boolValue = `B bool *)

let unpack_p4_bool (value : Value.t) : bool =
  Value.Get.(value |>> "`B bool" |> one |> bool)

(* errorValue = ERROR `. id *)
(* matchKindValue = MATCH_KIND `. id *)
(* stringValue = stringLiteral *)

let unpack_p4_string (value : Value.t) : string =
  Value.Get.(value |>> "`\" text `\"" |> one |> text)

(* D int *)

(* nat W int *)

let unpack_p4_fixedBit (value : Value.t) : Bigint.t * Bigint.t =
  Value.Get.(
    value |>> "nat W int" |> two |> fun (value_width, value_int) ->
    let width = value_width |> num |> Num.to_int in
    let int = value_int |> num |> Num.to_int in
    (width, int))

(* nat S int *)

let unpack_p4_fixedInt (value : Value.t) : Bigint.t * Bigint.t =
  Value.Get.(
    value |>> "nat S int" |> two |> fun (value_width, value_int) ->
    let width = value_width |> num |> Num.to_int in
    let int = value_int |> num |> Num.to_int in
    (width, int))

(* nat `. nat V int *)

let unpack_p4_variableBit (value : Value.t) : Bigint.t * Bigint.t * Bigint.t =
  Value.Get.(
    value |>> "nat `. nat V int" |> three
    |> fun (value_width_max, value_width, value_int) ->
    let width_max = value_width_max |> num |> Num.to_int in
    let width = value_width |> num |> Num.to_int in
    let int = value_int |> num |> Num.to_int in
    (width_max, width, int))

let unpack_p4_precision_numberValue (value : Value.t) : Bigint.t * Bigint.t =
  try unpack_p4_fixedBit value
  with _ -> (
    try unpack_p4_fixedInt value
    with _ ->
      let _, width, int = unpack_p4_variableBit value in
      (width, int))

(* listValue = LIST `[ value* ] *)

(* tupleValue = TUPLE `( value* ) *)

let unpack_p4_tuple (value : Value.t) : Value.t list =
  Value.Get.(value |>> "TUPLE `( value* )" |> one |> list)

(* headerStackValue = HEADER_STACK `[ value* `( nat; nat ) ] *)
(* structValue = STRUCT tid `{ fieldValue* } *)
(* headerValue = HEADER tid `{ bool `; fieldValue* } *)
(* headerUnionValue = HEADER_UNION tid `{ fieldValue* } *)

(* tid `. id *)

let unpack_p4_enum (value : Value.t) : string * string =
  Value.Get.(
    value |>> "tid `. id" |> two |> fun (value_tid, value_id) ->
    let tid = value_tid |> text in
    let id = value_id |> text in
    (tid, id))

(* tid `. id `. value *)

(* objectReferenceValue = `! oid *)
(* defaultValue = DEFAULT *)

(* SEQ `( value* ) *)

let unpack_p4_sequence (value : Value.t) : Value.t list =
  Value.Get.(value |>> "SEQ `( value* )" |> one |> list)

(* SEQ `( value* `, `... ) *)
(* RECORD `{ fieldValue* } *)
(* RECORD `{ fieldValue* `, `... } *)
(* SET `{ value } *)
(* SET `{ value `&&& value } *)
(* SET `{ value `.. value } *)
(* SET `{ `... } *)
(* TABLE_ENUM tid `. id *)
(* TABLE_STRUCT tid `{ fieldValue* } *)
