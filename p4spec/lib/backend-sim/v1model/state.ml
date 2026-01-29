open Interface.Wrap
open Interface.Unwrap
module Value = Runtime.Sim.Value

module MirrorTable = struct
  module M = Map.Make (Int)
  include M

  type t = int M.t

  let to_yojson (tbl : t) : Yojson.Safe.t =
    let kvs =
      bindings tbl |> List.map (fun (k, v) -> (string_of_int k, `Int v))
    in
    `Assoc kvs

  let of_yojson : Yojson.Safe.t -> (t, string) result = function
    | `Assoc kvs ->
        let rec aux acc = function
        | [] -> Result.map (fun s -> s |> List.to_seq |> of_seq) acc 
        | (k, v) :: tl -> Result.bind acc (fun acc' ->
            match v with
            | `Int iv -> aux (Ok ((int_of_string k, iv) :: acc')) tl
            | json -> Error
                       (Format.sprintf "Invalid value for MirrorTable: %s"
                          (Yojson.Safe.to_string json))
          )
        in
        aux (Ok []) kvs
    | json ->
        Error
          (Format.sprintf "Invalid MirrorTable: %s"
             (Yojson.Safe.to_string json))
end

module ArchState = struct
  type t = {
    queue : Scheduler.t;
    mirror_tbl : MirrorTable.t;
    clone_opt : Packet.CloneInfo.t option;
    resubmit_opt : Packet.ResubmitInfo.t option;
  }
  [@@deriving yojson]

  let empty =
    {
      queue = Scheduler.empty;
      mirror_tbl = MirrorTable.empty;
      resubmit_opt = None;
      clone_opt = None;
    }

  let with_clone_opt (clone_opt : Packet.CloneInfo.t option) (t : t) : t =
    { t with clone_opt }

  let with_clone (clone : Packet.CloneInfo.t) = with_clone_opt (Some clone)

  let with_resubmit_opt (resubmit_opt : Packet.ResubmitInfo.t option) (t : t) : t
      =
    { t with resubmit_opt }

  let with_resubmit (resubmit : Packet.ResubmitInfo.t) =
    with_resubmit_opt (Some resubmit)

  let with_queue (queue : Scheduler.t) (t : t) = { t with queue }
  let with_mirror_tbl (mirror_tbl : MirrorTable.t) (t : t) = { t with mirror_tbl }

  let reset (t : t) = { t with resubmit_opt = None; clone_opt = None }
  let to_value (t : t) = t |> to_yojson |> wrap_extern_v "archState"
  let of_value (v : Value.t) = v |> unwrap_extern_v |> of_yojson |> Result.get_ok
end
