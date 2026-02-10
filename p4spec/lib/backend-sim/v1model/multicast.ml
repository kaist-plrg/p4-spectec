type handle = int [@@deriving yojson]
type port = int [@@deriving yojson]
type mgid = int [@@deriving yojson]
type rid = int [@@deriving yojson]

module Make (V : sig
  type t [@@deriving yojson]
end) =
struct
  module M = Map.Make (Int)
  include M

  type t = V.t M.t

  let to_yojson (t : t) : Yojson.Safe.t =
    let kvs =
      bindings t |> List.map (fun (k, v) -> (string_of_int k, V.to_yojson v))
    in
    `Assoc kvs

  let of_yojson (j : Yojson.Safe.t) : (t, string) result =
    match j with
    | `Assoc kvs ->
        let rec aux acc = function
          | [] -> Ok acc
          | (ks, vj) :: tl -> (
              match int_of_string_opt ks with
              | None -> Error ("Key is not an int: " ^ ks)
              | Some k -> (
                  match V.of_yojson vj with
                  | Error e -> Error ("Value error at key " ^ ks ^ ": " ^ e)
                  | Ok v -> aux (M.add k v acc) tl))
        in
        aux M.empty kvs
    | _ -> Error "Expected JSON object"
end

(* Multicast Group *)

type group = { id : mgid; node_handles : handle list } [@@deriving yojson]

module GroupMap = Make (struct
  type t = group [@@deriving yojson]
end)

(* Multicast Node *)

type node = { port : port; rid : rid } [@@deriving yojson]

module NodeMap = Make (struct
  type t = node [@@deriving yojson]
end)

module State = struct
  type t = {
    next_handle : handle;
    (* handle -> node *)
    nodes : NodeMap.t;
    (* mgid -> group *)
    groups : GroupMap.t;
  }
  [@@deriving yojson]

  let empty =
    { next_handle = 0; groups = GroupMap.empty; nodes = NodeMap.empty }

  let group_create (mgid : mgid) ({ next_handle; groups; nodes } : t) : t =
    let group = { id = mgid; node_handles = [] } in
    let groups = GroupMap.add mgid group groups in
    { next_handle; groups; nodes }

  let node_create (rid : rid) (port : port) ({ next_handle; groups; nodes } : t)
      : t =
    let handle = next_handle in
    let node = { port; rid } in

    let next_handle = handle + 1 in
    let nodes = NodeMap.add handle node nodes in
    { next_handle; groups; nodes }

  let node_associate (mgid : mgid) (handle : handle)
      ({ next_handle; groups; nodes } : t) : t =
    let node = NodeMap.find handle nodes in
    let groups =
      GroupMap.update mgid
        (Option.map (fun group ->
             { id = group.id; node_handles = handle :: group.node_handles }))
        groups
    in
    { next_handle; groups; nodes }
end
