module Json = Util.Json

(* Multicast types *)

type handle = int [@@deriving yojson]
type port = int [@@deriving yojson]
type mgid = int [@@deriving yojson]
type instance = int [@@deriving yojson]

(* Multicast group *)

type group = handle list [@@deriving yojson]

module GroupMap = Json.Map.Make (struct
  type t = group [@@deriving yojson]
end)

(* Multicast node *)

type node = { port : port; instance : instance } [@@deriving yojson]

module NodeMap = Json.Map.Make (struct
  type t = node list [@@deriving yojson]
end)

(* Multicast state *)

module State = struct
  type t = {
    next_handle : handle;
    (* mgid -> group *)
    groups : GroupMap.t;
    (* handle -> node *)
    nodes : NodeMap.t;
  }
  [@@deriving yojson]

  let empty =
    { next_handle = 0; groups = GroupMap.empty; nodes = NodeMap.empty }

  let group_create (mgid : mgid) ({ next_handle; groups; nodes } : t) : t =
    let groups = GroupMap.add mgid [] groups in
    { next_handle; groups; nodes }

  let node_create (instance : instance) (ports : port list)
      ({ next_handle; groups; nodes } : t) : t =
    let handle = next_handle in
    let nodes_created = List.map (fun port -> { port; instance }) ports in
    let next_handle = handle + 1 in
    let nodes = NodeMap.add handle nodes_created nodes in
    { next_handle; groups; nodes }

  let node_associate (mgid : mgid) (handle : handle)
      ({ next_handle; groups; nodes } : t) : t =
    let groups =
      GroupMap.update mgid (Option.map (fun group -> group @ [ handle ])) groups
    in
    { next_handle; groups; nodes }
end
