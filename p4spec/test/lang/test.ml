open Lang
open Test_common
open Util.Error

(* Spec elaboration test *)

let elab_test specdir =
  let spec_il = elab specdir in
  Il.Print.string_of_spec spec_il |> print_endline

let elab_command =
  Core.Command.basic ~summary:"run elaboration test"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map specdir = flag "-s" (required string) ~doc:"p4 spec directory" in
     fun () ->
       try elab_test specdir
       with ParseError (at, msg) | ElabError (at, msg) ->
         Format.printf "Error on elaboration: %s\n" (string_of_error at msg))

(* Structuring test *)

let structure_test specdir =
  let spec_sl = structure specdir in
  Sl.Print.string_of_spec spec_sl |> print_endline

let structure_command =
  Core.Command.basic ~summary:"run structuring test"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map specdir = flag "-s" (required string) ~doc:"p4 spec directory" in
     fun () ->
       try structure_test specdir
       with ParseError (at, msg) | ElabError (at, msg) ->
         Format.printf "%s\n" (string_of_error at msg))

(* Prose test *)

let prose_test specdir =
  let spec_pl = prosify specdir in
  Pl.Render.render_spec spec_pl |> print_endline

let prose_command =
  Core.Command.basic ~summary:"run prose test"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map specdir = flag "-s" (required string) ~doc:"p4 spec directory" in
     fun () ->
       try prose_test specdir
       with ParseError (at, msg) | ElabError (at, msg) ->
         Format.printf "%s\n" (string_of_error at msg))

let command =
  Core.Command.group ~summary:"p4spec-test-lang"
    [
      ("elab", elab_command);
      ("struct", structure_command);
      ("prose", prose_command);
    ]

let () = Command_unix.run ~version command
