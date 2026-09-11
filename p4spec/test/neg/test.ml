module Run = Runtime.Dynamic_Runner.Signature
open Util.Source

let ( let* ) = Result.bind

module ExternImpl = struct
  let failure = ref (Run.Unmatch [])
  let raise_failure () = raise (Run.ExternError !failure)
  let eval_extern_init _ = raise_failure ()
  let eval_extern_func_lctk_call _ = raise_failure ()
  let eval_extern_func_call _ = raise_failure ()
  let eval_extern_method_call _ = raise_failure ()
  let init_arch_state = Runtime.Value.Make.nat (Bigint.of_int 0)
end

module Extern = Backend_sim.Extern.Make (ExternImpl)

let syntax_diagnostic =
  let related : P4spectec.Diagnostic.related =
    { region = no_region; message = "syntax related item" }
  in
  let trace : P4spectec.Diagnostic.trace_node =
    { region = no_region; message = "syntax trace"; children = [] }
  in
  P4spectec.Diagnostic.error ~code:"parse/test" ~detail:"Syntax detail."
    ~related:[ related ] ~trace:[ trace ] ~source:"parse" no_region
    "syntax diagnostic"

module ParseFailureInterface = struct
  include Interface.P4

  let parse_program _ _ : Run.parse_result = Run.Fail syntax_diagnostic
  let parse_string _ _ : Run.parse_result = Run.Fail syntax_diagnostic
end

let run (pass : string) (file : string) : unit P4spectec.result =
  match pass with
  | "parse" -> P4spectec.parse [ file ] |> Result.map ignore
  | "elab" -> P4spectec.elab [ file ] |> Result.map ignore
  | "algo" -> P4spectec.algo [ file ] |> Result.map ignore
  | "annotate" -> P4spectec.annotate [ file ] |> Result.map ignore
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

let as_pass_result result = Result.map ignore result

let capture_parse_error f =
  try
    f ();
    Ok ()
  with Frontend.Error.ParseError diagnostic -> Error diagnostic

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

let duplicate_al_spec () : Run.spec =
  let open Util.Source in
  let id = "Dup" $ no_region in
  let def = Lang.Al.ExternTypD (id, []) $ no_region in
  Run.AL [ def; def ]

let duplicate_sl_spec () : Run.spec =
  let open Util.Source in
  let id = "Dup" $ no_region in
  let def = Lang.Sl.ExternTypD (id, []) $ no_region in
  Run.SL [ def; def ]

let duplicate_pl_spec () : Run.spec =
  let open Util.Source in
  let id = "Dup" $ no_region in
  let node = Lang.Pl.ExternTypD id $ no_region in
  let def = Lang.Pl.Annot.no_hints node in
  Run.PL [ def; def ]

let render_boundary_cases () =
  render_case "command-error" (fun () ->
      Error (P4spectec.Error.error "command failed"));
  render_case "command-boot-source-conflict" (fun () ->
      Error P4spectec.Error.error_boot_source_conflict);
  render_case "command-boot-source-required" (fun () ->
      Error P4spectec.Error.error_boot_source_required);
  render_case "command-splice-file-count-mismatch" (fun () ->
      Error (P4spectec.Error.error_splice_file_count_mismatch 2 1));
  render_case "boot-unsupported-interface" (fun () ->
      P4spectec.build_null Backend_boot.Config.P4_interface Run.Empty
      |> Result.map ignore);
  render_case "interp-al-duplicate-type" (fun () ->
      P4spectec.build_null Backend_boot.Config.AL_interface
        (duplicate_al_spec ())
      |> Result.map ignore);
  render_case "interp-sl-duplicate-type" (fun () ->
      P4spectec.build_null Backend_boot.Config.AL_interface
        (duplicate_sl_spec ())
      |> Result.map ignore);
  render_case "interp-pl-duplicate-type" (fun () ->
      P4spectec.build_null Backend_boot.Config.AL_interface
        (duplicate_pl_spec ())
      |> Result.map ignore);
  let source = "syntax foo = | nat hint(blah)" in
  let preserve_spec_parse_diagnostic mode
      (parse : string -> string -> Run.parse_result) =
    match
      ( Spectec.Parse.parse_string mode "input.watsup" source,
        parse "input.watsup" source )
    with
    | Error expected, Run.Fail actual ->
        if actual <> expected then
          failwith "SpecTec parse diagnostic fields were not preserved";
        Ok ()
    | Error _, Run.Pass _ -> failwith "invalid SpecTec input was accepted"
    | Ok _, _ -> failwith "expected invalid SpecTec input"
  in
  render_case "parse-al-diagnostic" (fun () ->
      preserve_spec_parse_diagnostic Run.AL_mode
        Interface.SpecTec_AL.parse_string);
  render_case "parse-sl-diagnostic" (fun () ->
      preserve_spec_parse_diagnostic Run.SL_mode
        Interface.SpecTec_SL.parse_string);
  render_case "sim-unsupported-architecture" (fun () ->
      P4spectec.build_sim ~arch:"unsupported" Run.Empty |> Result.map ignore)

let eval_failure ?extern_failure (module Interpreter : Run.INTERP) relation :
    unit P4spectec.result =
  Option.iter (fun failure -> ExternImpl.failure := failure) extern_failure;
  let input = Runtime.Value.Make.nat (Bigint.of_int 1) in
  match Interpreter.eval_rel relation [ input ] with
  | Run.Pass _ -> failwith "relation unexpectedly matched"
  | Run.Fail (Run.Abort actual) -> (
      match extern_failure with
      | Some (Run.Abort expected) ->
          if actual <> expected then
            failwith "extern diagnostic fields were not preserved";
          Ok ()
      | _ -> Error actual)
  | Run.Fail (Run.Unmatch traces) -> (
      match extern_failure with
      | Some (Run.Abort _) -> failwith "extern diagnostic became failtraces"
      | _ -> Error (Run.diagnostic_of_failure (Run.Unmatch traces)))

let eval_syntax_failure (module Interpreter : Run.INTERP) :
    unit P4spectec.result =
  match Interpreter.eval_program "R" [] "input.p4" with
  | Run.Fail (`Syntax actual) ->
      if actual <> syntax_diagnostic then
        failwith "interpreter syntax diagnostic fields were not preserved";
      Ok ()
  | Run.Fail (`Runtime _) -> failwith "syntax error became a runtime error"
  | Run.Pass _ -> failwith "invalid input was accepted"

let extern_failtraces =
  let open Util.Attempt in
  Run.Unmatch
    [
      Failtrace
        ( no_region,
          (fun () -> "external relation failed"),
          [ Failtrace (no_region, (fun () -> "external leaf"), []) ] );
    ]

let extern_diagnostic =
  let related : P4spectec.Diagnostic.related =
    { region = no_region; message = "external related item" }
  in
  let trace : P4spectec.Diagnostic.trace_node =
    {
      region = no_region;
      message = "external trace root";
      children =
        [
          { region = no_region; message = "external trace leaf"; children = [] };
        ];
    }
  in
  Run.Abort
    (P4spectec.Diagnostic.error ~code:"sim/test" ~detail:"external detail"
       ~related:[ related ] ~trace:[ trace ] ~source:"sim" no_region
       "external diagnostic")

let render_al_case ?extern_failure title path relation =
  render_case title (fun () ->
      let module Interpreter = Interp_al.Interp.Make (Interface.P4) (Extern) ()
      in
      let* spec = P4spectec.algo [ path ] in
      let* () = Interpreter.init ~cache:false ~det:false ~guard:false spec in
      eval_failure ?extern_failure (module Interpreter) relation)

let render_sl_case ?extern_failure title path relation =
  render_case title (fun () ->
      let module Interpreter = Interp_sl.Interp.Make (Interface.P4) (Extern) ()
      in
      let* spec = P4spectec.structure ~final:true [ path ] in
      let* () = Interpreter.init ~cache:false ~det:false ~guard:false spec in
      eval_failure ?extern_failure (module Interpreter) relation)

let render_pl_case ?extern_failure title path relation =
  render_case title (fun () ->
      let module Interpreter = Interp_pl.Interp.Make (Interface.P4) (Extern) ()
      in
      let* spec = P4spectec.annotate [ path ] in
      let* () = Interpreter.init ~cache:false ~det:false ~guard:false spec in
      eval_failure ?extern_failure (module Interpreter) relation)

let render_interp_cases dir =
  let local = Filename.concat dir "backtrack.watsup" in
  let external_path = Filename.concat dir "external.watsup" in
  let module Interp_AL =
    Interp_al.Interp.Make (ParseFailureInterface) (Extern) ()
  in
  let module Interp_SL =
    Interp_sl.Interp.Make (ParseFailureInterface) (Extern) ()
  in
  let module Interp_PL =
    Interp_pl.Interp.Make (ParseFailureInterface) (Extern) ()
  in
  render_case "interp-al-syntax-diagnostic" (fun () ->
      eval_syntax_failure (module Interp_AL));
  render_case "interp-sl-syntax-diagnostic" (fun () ->
      eval_syntax_failure (module Interp_SL));
  render_case "interp-pl-syntax-diagnostic" (fun () ->
      eval_syntax_failure (module Interp_PL));
  render_al_case ~extern_failure:extern_diagnostic "interp-al-extern-diagnostic"
    external_path "R";
  render_sl_case ~extern_failure:extern_diagnostic "interp-sl-extern-diagnostic"
    external_path "R";
  render_pl_case ~extern_failure:extern_diagnostic "interp-pl-extern-diagnostic"
    external_path "R";
  render_al_case "interp-al-backtrack" local "R";
  render_sl_case "interp-sl-backtrack" local "R";
  render_pl_case "interp-pl-backtrack" local "R";
  render_al_case ~extern_failure:extern_failtraces "interp-al-extern-failtraces"
    external_path "R";
  render_sl_case ~extern_failure:extern_failtraces "interp-sl-extern-failtraces"
    external_path "R";
  render_pl_case ~extern_failure:extern_failtraces "interp-pl-extern-failtraces"
    external_path "R"

let () =
  let pass = Sys.argv.(1) in
  if pass = "boundary" then render_boundary_cases ()
  else if pass = "interp" then render_interp_cases Sys.argv.(2)
  else
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
