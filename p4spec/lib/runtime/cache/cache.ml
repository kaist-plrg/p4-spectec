open Util.Source

(* Cache entry using strings *)

module StringEntry = struct
  type t = string

  let default : t = ""
  let equal (s_a : t) (s_b : t) : bool = s_a = s_b
  let hash (s : t) : int = Hashtbl.hash s land 0x7FFFFFFF
end

module StringCache = Make.Make (StringEntry)

(* Cache entry using values *)

module ValueEntry = struct
  type t = Value.t

  let default : t = Value.Make.bool true
  let equal = Value.eq
  let hash (value : Value.t) = value.note.vhash
end

module ValueCache = Make.Make (ValueEntry)

(* Cache entry using id and values *)

module CallEntry = struct
  type t = string * Value.t list

  let default : t = ("", [])

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

module CallCache = Make.Make (CallEntry)
