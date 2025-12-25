open Lang
open Il
open Util.Source

let unwrap_bool_v (value : value) : bool =
  match value.it with BoolV b -> b | _ -> failwith "expected BoolV value"

let unwrap_num_v (value : value) : Bigint.t =
  match value.it with
  | NumV (`Nat n) -> n
  | NumV (`Int i) -> i
  | _ -> failwith "expected NumV value"

let unwrap_text_v (value : value) : string =
  match value.it with TextV s -> s | _ -> failwith "expected TextV value"

let unwrap_case_v (value : value) : valuecase =
  match value.it with
  | CaseV valuecase -> valuecase
  | _ -> failwith "expected CaseV value"

let unwrap_tuple_v (value : value) : value list =
  match value.it with
  | TupleV values -> values
  | _ -> failwith "expected TupleV value"

let unwrap_tuple_v_two (value : value) : value * value =
  match value.it with
  | TupleV [ v_a; v_b ] -> (v_a, v_b)
  | _ -> failwith "expected TupleV of length 2"

let unwrap_opt_v (value : value) : value option =
  match value.it with
  | OptV value_opt -> value_opt
  | _ -> failwith "expected OptV value"

let unwrap_list_v (value : value) : value list =
  match value.it with
  | ListV values -> values
  | _ -> failwith "expected ListV value"

let unwrap_extern_v (value : value) : Yojson.Safe.t =
  match value.it with
  | ExternV json -> json
  | _ -> failwith "expected ExternV value"
