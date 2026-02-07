module Value = Runtime.Dynamic_Il.Value
open Wrap

(* Packs an IL value representing a P4 value from an OCaml type *)

(* boolValue = B bool *)
(* errorValue = ERROR `. id *)
(* matchKindValue = MATCH_KIND `. id *)
(* stringValue = stringLiteral *)
(* D int *)

let pack_p4_arbitraryInt (i : Bigint.t) : Value.t =
  let value_i = i |> wrap_num_v_int in
  [ Term "D"; NT value_i ] #@ "value"

(* nat W int *)

let pack_p4_fixedBit (width : Bigint.t) (i : Bigint.t) : Value.t =
  let value_width = width |> wrap_num_v_nat in
  let value_i = i |> wrap_num_v_int in
  [ NT value_width; Term "W"; NT value_i ] #@ "value"

let pack_p4_enum (type_id : string) (name : string) : Value.t =
  let value_typeId = wrap_text_v type_id in
  let value_nameIR = wrap_text_v name in
  [ NT value_typeId; Term "."; NT value_nameIR ] #@ "enumValue"

(* nat S int *)
(* nat `. nat V int *)
(* listValue = LIST `[ value* ] *)
(* tupleValue = TUPLE `( value* ) *)
(* headerStackValue = HEADER_STACK `[ value* `( nat; nat ) ] *)
(* structValue = STRUCT tid `{ fieldValue* } *)
(* headerValue = HEADER tid `{ bool `; fieldValue* } *)
(* headerUnionValue = HEADER_UNION tid `{ fieldValue* } *)
(* tid `. id *)
(* tid `. id `. value *)
(* objectReferenceValue = `! oid *)
(* defaultValue = DEFAULT *)
(* SEQ `( value* ) *)
(* SEQ `( value* `, `... ) *)
(* RECORD `{ fieldValue* } *)
(* RECORD `{ fieldValue* `, `... } *)
(* SET `{ value } *)
(* SET `{ value `&&& value } *)
(* SET `{ value `.. value } *)
(* SET `{ `... } *)
(* TABLE_ENUM tid `. id *)
(* TABLE_STRUCT tid `{ fieldValue* } *)
