open Lang
open Test_common
open Util.Error

(* Spec elaboration test *)

let elab_test path_spec =
  let spec_il = Pass.elab path_spec in
  Il.Print.string_of_spec spec_il |> print_endline

let elab_command =
  Core.Command.basic ~summary:"run elaboration test"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map path_spec = flag "-s" (required string) ~doc:"p4 spec directory" in
     fun () ->
       try elab_test [ path_spec ]
       with ParseError (at, msg) | ElabError (at, msg) ->
         Format.printf "Error on elaboration: %s\n" (string_of_error at msg))

(* Algo test *)

let algo_test path_spec =
  let spec_al = Pass.algo path_spec in
  Al.Print.string_of_spec spec_al |> print_endline

let algo_command =
  Core.Command.basic ~summary:"run algo test"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map path_spec = flag "-s" (required string) ~doc:"p4 spec directory" in
     fun () ->
       try algo_test [ path_spec ]
       with
       | ParseError (at, msg) | ElabError (at, msg) | AlgoError (at, msg) ->
         Format.printf "%s\n" (string_of_error at msg))

(* Structuring test *)

let structure_test path_spec =
  let spec_sl = Pass.structure ~final:true path_spec in
  Sl.Print.string_of_spec spec_sl |> print_endline

let structure_command =
  Core.Command.basic ~summary:"run structuring test"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map path_spec = flag "-s" (required string) ~doc:"p4 spec directory" in
     fun () ->
       try structure_test [ path_spec ]
       with
       | ParseError (at, msg) | ElabError (at, msg) | AlgoError (at, msg) ->
         Format.printf "%s\n" (string_of_error at msg))

(* Annotate (prose) test *)

let annotate_test path_spec =
  let spec_pl = Pass.annotate path_spec in
  Pl.Render.render_spec spec_pl |> print_endline

let annotate_command =
  Core.Command.basic ~summary:"run annotate test"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map path_spec = flag "-s" (required string) ~doc:"p4 spec directory" in
     fun () ->
       try annotate_test [ path_spec ]
       with
       | ParseError (at, msg) | ElabError (at, msg) | AlgoError (at, msg) ->
         Format.printf "%s\n" (string_of_error at msg))

let command =
  Core.Command.group ~summary:"p4spec-test-lang"
    [
      ("elab", elab_command);
      ("algo", algo_command);
      ("struct", structure_command);
      ("annotate", annotate_command);
    ]

let () = Command_unix.run ~version command
