open Util.Source

(* Cache entry for relation and function invocations *)

module Entry = struct
  type t = string * Value.t list

  let rec equal_values (values_a : Value.t list) (values_b : Value.t list) :
      bool =
    match (values_a, values_b) with
    | [], [] -> true
    | v_a :: rest_a, v_b :: rest_b ->
        Value.eq v_a v_b && equal_values rest_a rest_b
    | _ -> false

  let equal ((id_a, values_a) : t) ((id_b, values_b) : t) : bool =
    if id_a <> id_b then false else equal_values values_a values_b

  let hash ((id, values) : t) : int =
    let h = ref ((Hashtbl.hash id * 31) + 17) in
    let rec hash_value (value : Value.t) =
      match value.it with
      | BoolV b -> h := (!h * 31) + if b then 1231 else 1237
      | NumV (`Nat n) -> h := (!h * 31) + (1 + Bigint.hash n)
      | NumV (`Int i) -> h := (!h * 31) + (2 + Bigint.hash i)
      | TextV s -> h := (!h * 31) + Hashtbl.hash s
      | StructV valuefields ->
          List.iter
            (fun (atom, value_field) ->
              h := (!h * 31) + Hashtbl.hash atom.it;
              hash_value value_field)
            valuefields
      | CaseV (mixop, values) ->
          List.iter
            (fun atoms ->
              List.iter
                (fun atom -> h := (!h * 31) + Hashtbl.hash atom.it)
                atoms)
            mixop;
          List.iter hash_value values
      | TupleV values | ListV values -> List.iter hash_value values
      | OptV None -> h := (!h * 31) + 997
      | OptV (Some value) ->
          h := (!h * 31) + 1009;
          hash_value value
      | FuncV id -> h := (!h * 31) + Hashtbl.hash id.it
      | ExternV json -> h := (!h * 31) + Hashtbl.hash json
    in
    List.iter hash_value values;
    !h land 0x7FFFFFFF
end

(* Cache *)

module Cache = struct
  module Table = Hashtbl.Make (Entry)

  let create ~size = Table.create size
  let clear cache = Table.clear cache
  let reset cache = Table.reset cache
  let find cache key = Table.find_opt cache key
  let add cache key value = Table.add cache key value
  let size cache = Table.length cache
end

(* Cache targets *)

let is_cached_func = function
  | "specialize_typeDefIR" | "unroll_typeIR" | "free_typeIR" | "bound" -> true
  | _ -> false

let is_cached_rule = function
  | "Cast_expl" | "Cast_impl" | "Type_wf" | "Type_alpha" -> true
  | _ -> false
