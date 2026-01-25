(* Port and packets are input/outputs *)

type port = int
type packet = string
type rx = port * packet
type tx = port * packet
type expect = tx * bool

let string_of_rx ((port, packet) : rx) : string =
  Printf.sprintf "(%d) %s" port packet

let string_of_tx ((port, packet) : tx) : string =
  Printf.sprintf "(%d)%s" port (if packet = "" then "" else " " ^ packet)

let compare_packet ~(exact : bool) (packet_out : packet)
    (packet_expect : packet) : bool =
  let to_list s = List.init (String.length s) (String.get s) in
  let take n l =
    List.fold_left
      (fun (k, acc) x -> if k = 0 then (0, acc) else (k - 1, x :: acc))
      (n, []) l
    |> snd |> List.rev
  in
  let packet_out = to_list packet_out in
  let packet_expect = to_list packet_expect in
  let len_packet_out = List.length packet_out in
  let len_packet_expect = List.length packet_expect in
  if len_packet_out < len_packet_expect then false
  else
    let packet_out =
      if exact then packet_out else take len_packet_expect packet_out
    in
    List.length packet_out = List.length packet_expect
    && List.fold_left2
         (fun same o e -> same && (e = '*' || o = e))
         true packet_out packet_expect

let compare_tx ~(exact : bool) ((port_out, packet_out) : tx)
    ((port_expect, packet_expect) : tx) : bool =
  port_out = port_expect && compare_packet ~exact packet_out packet_expect
