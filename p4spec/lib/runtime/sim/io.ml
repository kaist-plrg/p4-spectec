(* Port and packets are input/outputs *)

type port = int
type packet = string
type rx = port * packet
type tx = port * packet

let string_of_rx ((port, packet) : rx) : string =
  Printf.sprintf "(%d) %s" port packet

let string_of_tx ((port, packet) : tx) : string =
  Printf.sprintf "(%d)%s" port (if packet = "" then "" else " " ^ packet)

let compare_packet packet_out packet_expect : bool =
  let to_list s = List.init (String.length s) (String.get s) in
  let packet_out = to_list packet_out in
  let packet_expect = to_list packet_expect in
  List.length packet_out = List.length packet_expect
  && List.fold_left2
       (fun same o e -> same && (e = '*' || o = e))
       true packet_out packet_expect

let compare_tx ((port_out, packet_out) : tx) ((port_expect, packet_expect) : tx)
    : bool =
  port_out = port_expect && compare_packet packet_out packet_expect
