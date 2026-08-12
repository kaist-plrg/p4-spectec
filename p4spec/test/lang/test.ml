open Lang
open Test_common

(* Spec elaboration test *)

let elab_test path_spec =
  match Pass.elab path_spec with
  | Ok spec_il -> Il.Print.string_of_spec spec_il |> print_endline
  | Error e ->
      Format.printf "Error on elaboration: %s\n" (Pass.string_of_error e)

let elab_command =
  Core.Command.basic ~summary:"run elaboration test"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map path_spec = flag "-s" (required string) ~doc:"p4 spec directory" in
     fun () -> elab_test [ path_spec ])

(* Algo test *)

let algo_test path_spec =
  match Pass.algo path_spec with
  | Ok spec_al -> Al.Print.string_of_spec spec_al |> print_endline
  | Error e -> Format.printf "%s\n" (Pass.string_of_error e)

let algo_command =
  Core.Command.basic ~summary:"run algo test"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map path_spec = flag "-s" (required string) ~doc:"p4 spec directory" in
     fun () -> algo_test [ path_spec ])

(* Structuring test *)

let structure_test path_spec =
  match Pass.structure ~final:true path_spec with
  | Ok spec_sl -> Sl.Print.string_of_spec spec_sl |> print_endline
  | Error e -> Format.printf "%s\n" (Pass.string_of_error e)

let structure_command =
  Core.Command.basic ~summary:"run structuring test"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map path_spec = flag "-s" (required string) ~doc:"p4 spec directory" in
     fun () -> structure_test [ path_spec ])

(* Annotate (prose) test *)

let annotate_test path_spec =
  match Pass.annotate path_spec with
  | Ok spec_pl -> Pl.Render.render_spec spec_pl |> print_endline
  | Error e -> Format.printf "%s\n" (Pass.string_of_error e)

let annotate_command =
  Core.Command.basic ~summary:"run annotate test"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map path_spec = flag "-s" (required string) ~doc:"p4 spec directory" in
     fun () -> annotate_test [ path_spec ])

let command =
  Core.Command.group ~summary:"p4spec-test-lang"
    [
      ("elab", elab_command);
      ("algo", algo_command);
      ("struct", structure_command);
      ("annotate", annotate_command);
    ]

let () = Command_unix.run ~version command
