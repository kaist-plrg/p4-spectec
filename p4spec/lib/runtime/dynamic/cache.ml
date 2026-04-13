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
    List.iter (fun (v : Value.t) -> h := (!h * 31) + v.note.vhash) values;
    !h land 0x7FFFFFFF
end

(* Cache *)

module Cache = struct
  module Table = Hashtbl.Make (Entry)

  let capacity = ref 1024

  let create ~size =
    capacity := size;
    Table.create size

  let size cache = Table.length cache
  let clear cache = Table.clear cache
  let reset cache = Table.reset cache
  let find cache key = Table.find_opt cache key
  let add cache key value = Table.add cache key value
end
