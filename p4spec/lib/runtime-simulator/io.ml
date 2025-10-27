(* Port and packets are input/outputs *)

type port = int
type packet = string
type rx = port * packet
type tx = port * packet

let compare_packet packet_out packet_expect : bool =
  let to_list s = List.init (String.length s) (String.get s) in
  let packet_out = to_list packet_out in
  let packet_expect = to_list packet_expect in
  List.length packet_out = List.length packet_expect
  && List.fold_left2
       (fun same o e -> same && (e = '*' || o = e))
       true packet_out packet_expect

let compare_result (port_out, packet_out) (port_expect, packet_expect) : bool =
  let pass =
    port_out = port_expect && compare_packet packet_out packet_expect
  in
  if pass then
    Format.printf "[PASS] Expected: %d %s / Got: %d %s\n" port_expect
      packet_expect port_out packet_out
  else
    Format.printf "[FAIL] Expected: %d %s / Got: %d %s\n" port_expect
      packet_expect port_out packet_out;
  pass
