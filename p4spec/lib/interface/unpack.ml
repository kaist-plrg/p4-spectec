module Value = Runtime.Dynamic_Il.Value
open Flatten
open Unwrap

(* Unpacks an IL value representing a P4 value into an OCaml type *)

let first fs x = List.find_map (fun f -> f x) fs

(* boolValue = `B bool *)

let unpack_p4_bool (value : Value.t) : bool =
  match flatten_case_v_opt value with
  | Some (_, [ [ "`B" ]; [] ], [ value_bool ]) -> unwrap_bool_v value_bool
  | _ -> assert false

(* errorValue = ERROR `. id *)
(* matchKindValue = MATCH_KIND `. id *)
(* stringValue = stringLiteral *)

let unpack_p4_string (value : Value.t) : string =
  match flatten_case_v_opt value with
  | Some (_, [ [ "\"" ]; [ "\"" ] ], [ value_string ]) ->
      unwrap_text_v value_string
  | _ -> assert false

(* D int *)

(* nat W int *)

let unpack_p4_fixedBit_opt (value : Value.t) : (Bigint.t * Bigint.t) option =
  match flatten_case_v_opt value with
  | Some (_, [ []; [ "W" ]; [] ], [ value_width; value_int ]) ->
      Some (unwrap_num_v value_width, unwrap_num_v value_int)
  | _ -> None

let unpack_p4_fixedBit (value : Value.t) : Bigint.t * Bigint.t =
  value |> unpack_p4_fixedBit_opt |> Option.get

(* nat S int *)

let unpack_p4_fixedInt_opt (value : Value.t) : (Bigint.t * Bigint.t) option =
  match flatten_case_v_opt value with
  | Some (_, [ []; [ "S" ]; [] ], [ value_width; value_int ]) ->
      Some (unwrap_num_v value_width, unwrap_num_v value_int)
  | _ -> None

let unpack_p4_fixedInt (value : Value.t) : Bigint.t * Bigint.t =
  value |> unpack_p4_fixedInt_opt |> Option.get

(* nat `. nat V int *)

let unpack_p4_variableBit_opt (value : Value.t) :
    (Bigint.t * Bigint.t * Bigint.t) option =
  match flatten_case_v_opt value with
  | Some
      ( _,
        [ []; [ "." ]; [ "V" ]; [] ],
        [ value_width_max; value_width; value_int ] ) ->
      Some
        ( unwrap_num_v value_width_max,
          unwrap_num_v value_width,
          unwrap_num_v value_int )
  | _ -> None

let unpack_p4_variableBit (value : Value.t) : Bigint.t * Bigint.t * Bigint.t =
  value |> unpack_p4_variableBit_opt |> Option.get

let unpack_p4_precision_numberValue_opt (value : Value.t) :
    (Bigint.t * Bigint.t) option =
  let unpack_p4_variableBit_opt value =
    match unpack_p4_variableBit_opt value with
    | Some (_, width, i) -> Some (width, i)
    | _ -> None
  in
  first
    [
      unpack_p4_fixedBit_opt; unpack_p4_fixedInt_opt; unpack_p4_variableBit_opt;
    ]
    value

let unpack_p4_precision_numberValue (value : Value.t) : Bigint.t * Bigint.t =
  value |> unpack_p4_precision_numberValue_opt |> Option.get

(* listValue = LIST `[ value* ] *)

(* tupleValue = TUPLE `( value* ) *)

let unpack_p4_tuple (value : Value.t) : Value.t list =
  match flatten_case_v_opt value with
  | Some (_, [ [ "TUPLE"; "(" ]; [ ")" ] ], [ values ]) -> unwrap_list_v values
  | _ -> assert false

(* headerStackValue = HEADER_STACK `[ value* `( nat; nat ) ] *)
(* structValue = STRUCT tid `{ fieldValue* } *)
(* headerValue = HEADER tid `{ bool `; fieldValue* } *)
(* headerUnionValue = HEADER_UNION tid `{ fieldValue* } *)

(* tid `. id *)

let unpack_p4_enum (value : Value.t) : string * string =
  match flatten_case_v_opt value with
  | Some (_, [ []; [ "." ]; [] ], [ value_tid; value_id ]) ->
      (unwrap_text_v value_tid, unwrap_text_v value_id)
  | _ -> assert false

(* tid `. id `. value *)

(* objectReferenceValue = `! oid *)
(* defaultValue = DEFAULT *)

(* SEQ `( value* ) *)

let unpack_p4_sequence (value : Value.t) : Value.t list =
  match flatten_case_v_opt value with
  | Some (_, [ [ "SEQ"; "(" ]; [ ")" ] ], [ values ]) -> unwrap_list_v values
  | _ -> assert false

(* SEQ `( value* `, `... ) *)
(* RECORD `{ fieldValue* } *)
(* RECORD `{ fieldValue* `, `... } *)
(* SET `{ value } *)
(* SET `{ value `&&& value } *)
(* SET `{ value `.. value } *)
(* SET `{ `... } *)
(* TABLE_ENUM tid `. id *)
(* TABLE_STRUCT tid `{ fieldValue* } *)
