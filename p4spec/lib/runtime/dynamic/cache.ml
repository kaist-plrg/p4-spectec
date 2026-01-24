open Domain
open Lang
open Xl
open Util.Source

(* Cache entry for relation and function invocations *)

module Entry = struct
  type t = string * Value.t list

  let equal (id_a, values_a) (id_b, values_b) =
    id_a = id_b
    && List.compare (fun v_a v_b -> Value.compare v_a v_b) values_a values_b = 0

  let ( +! ) h1 h2 = (h1 * 65599) + h2
  let hash_atom (atom : Atom.t) : int = Hashtbl.hash atom

  let hash_num (num : Num.t) : int =
    match num with `Nat n -> 0 +! Bigint.hash n | `Int i -> 1 +! Bigint.hash i

  let hash_mixop (mixop : Mixop.t) : int =
    List.fold_left
      (fun hash atoms ->
        List.fold_left (fun hash atom -> hash +! hash_atom atom.it) hash atoms)
      2 mixop

  let rec hash_value (v : Value.t) : int =
    match v.it with
    | BoolV b -> 0 +! Hashtbl.hash b
    | NumV n -> 1 +! hash_num n
    | TextV s -> 2 +! Hashtbl.hash s
    | StructV fields ->
        List.fold_left
          (fun hash (atom, v) -> hash +! (hash_atom atom.it +! hash_value v))
          3 fields
    | CaseV (mixop, values) ->
        let base_hash = 4 +! hash_mixop mixop in
        List.fold_left (fun hash v -> hash +! hash_value v) base_hash values
    | TupleV values ->
        List.fold_left (fun hash v -> hash +! hash_value v) 5 values
    | OptV None -> 6
    | OptV (Some v) -> 7 +! hash_value v
    | ListV values ->
        List.fold_left (fun hash v -> hash +! hash_value v) 8 values
    | FuncV id -> 9 +! Hashtbl.hash id.it
    | ExternV json -> 10 +! Hashtbl.hash json

  let hash (id, values) =
    let base_hash = Hashtbl.hash id in
    List.fold_left (fun hash v -> hash +! hash_value v) base_hash values
end

(* Cache *)

module Cache = struct
  module Table = Hashtbl.Make (Entry)

  let create ~size = Table.create size
  let clear cache = Table.clear cache
  let find cache key = Table.find_opt cache key
  let add cache key value = Table.add cache key value
end

(* Cache targets *)

let is_cached_func = function
  | "subst_typeIR" | "specialize_typeDefIR" | "unroll_typeIR" | "free_typeIR"
  | "bound" | "gen_constraint_typeIR" | "merge_constraint" | "merge_constraint'"
  | "nestable_struct" | "nestable_struct_in_header" ->
      true
  | _ -> false

let is_cached_rule = function
  | "Cast_expl" | "Cast_impl" | "Type_wf" | "Type_alpha" ->
      true
  | _ -> false
