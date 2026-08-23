let run (pass : string) (file : string) : unit P4spectec.result =
  match pass with
  | "parse" -> P4spectec.parse [ file ] |> Result.map ignore
  | "elab" -> P4spectec.elab [ file ] |> Result.map ignore
  | "algo" -> P4spectec.algo [ file ] |> Result.map ignore
  | _ -> failwith ("unknown pass: " ^ pass)

let render_case name f =
  Printf.printf "=== %s ===\n" name;
  (try
     let _result, report = P4spectec.with_diagnostics f in
     P4spectec.Diagnostic.Report.to_sorted_list report
     |> List.iter (fun (diagnostic : P4spectec.Diagnostic.t) ->
            match diagnostic.code with
            | None -> ()
            | Some code ->
                let prefix = diagnostic.source ^ "/" in
                let prefix_length = String.length prefix in
                if
                  String.length code < prefix_length
                  || String.sub code 0 prefix_length <> prefix
                then
                  failwith
                    (Printf.sprintf
                       "diagnostic code %S does not match source %S" code
                       diagnostic.source));
     P4spectec.Diagnostic.Render.render_report
       ~ansi:P4spectec.Diagnostic.Ansi.plain report
     |> print_string
   with exn ->
     Printf.printf "uncaught exception: %s\n" (Printexc.to_string exn));
  print_newline ()

let as_pass_result result =
  result |> Result.map ignore
  |> Result.map_error (fun diagnostic -> P4spectec.Error.PassError diagnostic)

let capture_parse_error f =
  try
    f ();
    Ok ()
  with Frontend.Error.ParseError diagnostic ->
    Error (P4spectec.Error.PassError diagnostic)

let render_parse_boundary_cases () =
  render_case "parse-illegal-control-in-text-literal" (fun () ->
      Frontend.Parse.parse_string "\"\011" |> as_pass_result);
  render_case "parse-malformed-mixop" (fun () ->
      capture_parse_error (fun () -> Frontend.Parse.parse_mixop "" |> ignore));
  render_case "parse-malformed-utf8" (fun () ->
      Frontend.Parse.parse_string "\128" |> as_pass_result);
  render_case "parse-malformed-utf8-in-comment" (fun () ->
      Frontend.Parse.parse_string "(;\128;)" |> as_pass_result);
  render_case "parse-misplaced-control-char" (fun () ->
      Frontend.Parse.parse_string "\011" |> as_pass_result);
  render_case "parse-directory-io-error" (fun () ->
      let path = "unreadable-dir" in
      Unix.mkdir path 0o700;
      Fun.protect
        ~finally:(fun () ->
          Unix.chmod path 0o700;
          Unix.rmdir path)
        (fun () ->
          Unix.chmod path 0o000;
          Frontend.Parse.parse_files [ path ] |> as_pass_result))

let () =
  let pass = Sys.argv.(1) in
  let dir = Sys.argv.(2) in
  Sys.chdir dir;
  let cases =
    Sys.readdir "." |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".watsup")
    |> List.sort String.compare
  in
  List.iter (fun file -> render_case file (fun () -> run pass file)) cases;
  if pass = "parse" then (
    render_case "parse-io-error" (fun () ->
        P4spectec.parse [ "missing-input.watsup" ] |> Result.map ignore);
    render_parse_boundary_cases ())
