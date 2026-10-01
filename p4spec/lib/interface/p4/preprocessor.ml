open Core

(* [cc]'s own stderr passes through, so a failure only reports its exit. *)
let preprocess includes path : (string, string) result =
  let cmd =
    String.concat ~sep:" "
      ([ "cc" ]
      @ List.map ~f:(Printf.sprintf "-I%s") includes
      @ [ "-undef"; "-nostdinc"; "-E"; "-x"; "c"; path ])
  in
  let in_chan = Core_unix.open_process_in cmd in
  let program = In_channel.input_all in_chan in
  match Core_unix.close_process_in in_chan with
  | Ok () -> Ok program
  | Error _ as status ->
      Error ("cc " ^ Core_unix.Exit_or_signal.to_string_hum status)
