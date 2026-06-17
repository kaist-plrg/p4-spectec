(* Packs an IL value representing a P4 value from an OCaml type *)

module Make (V : Valrep.VAL) = struct
  type vt = V.t

  (* boolValue = B bool *)
  (* errorValue = ERROR `. id *)
  (* matchKindValue = MATCH_KIND `. id *)
  (* stringValue = stringLiteral *)
  (* D int *)

  let pack_p4_arbitraryInt (i : Bigint.t) : vt =
    let value_int = V.Make.int i in
    V.Make.("D int" <| [ value_int ] <<| Typs.value)

  (* nat W int *)

  let pack_p4_fixedBit (width : Bigint.t) (i : Bigint.t) : vt =
    let value_nat = V.Make.nat width in
    let value_int = V.Make.int i in
    V.Make.("nat W int" <| [ value_nat; value_int ] <<| Typs.value)

  (* nat S int *)
  (* nat `. nat V int *)
  (* listValue = LIST `[ value* ] *)
  (* tupleValue = TUPLE `( value* ) *)
  (* headerStackValue = HEADER_STACK `[ value* `( nat; nat ) ] *)
  (* structValue = STRUCT tid `{ fieldValue* } *)
  (* headerValue = HEADER tid `{ bool `; fieldValue* } *)
  (* headerUnionValue = HEADER_UNION tid `{ fieldValue* } *)
  (* tid `. id *)

  let pack_p4_enum (type_id : string) (name : string) : vt =
    let value_tid = V.Make.text type_id in
    let value_id = V.Make.text name in
    V.Make.("tid `. id" <| [ value_tid; value_id ] <<| Typs.value)

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
end
