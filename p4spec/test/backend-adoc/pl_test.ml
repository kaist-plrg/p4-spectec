open Lang
open Test_common

let () =
  match P4spectec.annotate [ Sys.argv.(1) ] with
  | Ok spec_pl -> Backend_adoc.Pl.render_spec spec_pl |> print_endline
  | Error error -> Format.printf "%s\n" (Error.to_string error)
