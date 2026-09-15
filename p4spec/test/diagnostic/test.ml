open Util.Source
module Diagnostic = P4spectec.Diagnostic

let fail name expected actual =
  failwith (Printf.sprintf "%s: expected %S, but got %S" name expected actual)

let expect name expected actual =
  if not (String.equal expected actual) then fail name expected actual

let expect_true name condition =
  if not condition then failwith (name ^ ": condition is false")

let contains text substring =
  let text_length = String.length text in
  let substring_length = String.length substring in
  let rec search offset =
    offset + substring_length <= text_length
    && (String.sub text offset substring_length = substring
       || search (offset + 1))
  in
  substring_length = 0 || search 0

let expect_contains name substring text =
  if not (contains text substring) then fail name substring text

let position file line column : pos = { file; line; column }

let region file line column_left column_right : region =
  {
    left = position file line column_left;
    right = position file line column_right;
  }

let render file line column_left column_right =
  let diagnostic =
    Diagnostic.error ~code:"test/region" ~source:"test"
      (region file line column_left column_right)
      "failure"
  in
  Diagnostic.Report.singleton diagnostic
  |> Diagnostic.Render.render_report ~ansi:Diagnostic.Ansi.plain

let output_line index output =
  String.split_on_char '\n' output |> fun lines -> List.nth lines index

let check_region file ~name ~line ~left ~right ~location ~underline =
  let output = render file line left right in
  expect (name ^ " location")
    (Printf.sprintf "  --> %s:%d:%d" file line location)
    (output_line 1 output);
  expect (name ^ " underline") underline (output_line 4 output)

type case = {
  name : string;
  source : string;
  left : int;
  right : int;
  location : int;
  underline : string;
}

let cases =
  [
    {
      name = "preceding two-byte character";
      source = "\xce\xb1@";
      left = 2;
      right = 3;
      location = 2;
      underline = "  |  ^";
    };
    {
      name = "three-byte character";
      source = "\xe2\x82\xac";
      left = 0;
      right = 3;
      location = 1;
      underline = "  | ^";
    };
    {
      name = "four-byte wide character";
      source = "\xf0\x9f\x99\x82";
      left = 0;
      right = 4;
      location = 1;
      underline = "  | ^^";
    };
    {
      name = "malformed byte";
      source = "\x80@";
      left = 1;
      right = 2;
      location = 2;
      underline = "  |  ^";
    };
    {
      name = "combining character";
      source = "e\xcc\x81";
      left = 0;
      right = 3;
      location = 1;
      underline = "  | ^";
    };
    {
      name = "wide character";
      source = "\xe7\x95\x8c";
      left = 0;
      right = 3;
      location = 1;
      underline = "  | ^^";
    };
    {
      name = "tab before range";
      source = "\t@";
      left = 1;
      right = 2;
      location = 2;
      underline = "  |     ^";
    };
    {
      name = "tab within range";
      source = "\t@";
      left = 0;
      right = 2;
      location = 1;
      underline = "  | ^^^^^";
    };
    {
      name = "overlong UTF-8";
      source = "\xc0\x80@";
      left = 2;
      right = 3;
      location = 3;
      underline = "  |   ^";
    };
    {
      name = "emoji grapheme";
      source = "\xf0\x9f\x91\xa9\xe2\x80\x8d\xf0\x9f\x92\xbb";
      left = 0;
      right = 11;
      location = 1;
      underline = "   | ^^";
    };
    {
      name = "flag grapheme";
      source = "\xf0\x9f\x87\xba\xf0\x9f\x87\xb8";
      left = 0;
      right = 8;
      location = 1;
      underline = "   | ^^";
    };
    {
      name = "malformed byte before emoji";
      source = "\x80\xf0\x9f\x91\xa9\xe2\x80\x8d\xf0\x9f\x92\xbb";
      left = 0;
      right = 12;
      location = 1;
      underline = "   | ^^^";
    };
    {
      name = "ordinary multi-scalar grapheme";
      source = "\xe1\x84\x80\xe1\x85\xa1";
      left = 0;
      right = 6;
      location = 1;
      underline = "   | ^^^";
    };
  ]

let write_source file =
  let channel = open_out_bin file in
  output_string channel
    (String.concat "\n" (List.map (fun case -> case.source) cases));
  close_out channel

let check_trace_location file =
  let trace : Diagnostic.trace_node =
    { region = region file 1 2 3; message = "trace node"; children = [] }
  in
  let diagnostic =
    Diagnostic.error ~code:"test/trace" ~trace:[ trace ] ~source:"test"
      no_region "failure"
  in
  let output =
    Diagnostic.Report.singleton diagnostic
    |> Diagnostic.Render.render_report ~ansi:Diagnostic.Ansi.plain
  in
  expect_contains "trace location" (Printf.sprintf "%s:1.2-1.3" file) output

let failtrace region message children =
  Util.Attempt.Failtrace (region, (fun () -> message), children)

let check_empty_failtrace_fallback () =
  let diagnostic =
    Diagnostic.of_failtraces ~source:"test" ~fallback:"fallback" []
  in
  expect "empty failtrace message" "fallback" diagnostic.message;
  expect "empty failtrace source" "test" diagnostic.source;
  expect_true "empty failtrace region" (diagnostic.region = no_region);
  expect_true "empty failtrace trace" (diagnostic.trace = [])

let check_coded_diagnostic_source () =
  let diagnostic =
    Diagnostic.error ~code:"test/example" ~source:"test" no_region "failure"
  in
  expect "coded diagnostic source" "test" diagnostic.source;
  expect_true "coded diagnostic code" (diagnostic.code = Some "test/example")

let check_quoting () =
  expect "safe diagnostic quote" "`name`" (Diagnostic.quote "name");
  expect "escaped diagnostic quote" "\"a\\x60b\\n\"" (Diagnostic.quote "a`b\n")

let check_warning_fields file =
  let related : Diagnostic.related =
    { region = region file 1 0 1; message = "related" }
  in
  let _, report =
    Diagnostic.collect (fun () ->
        Diagnostic.warn ~code:"test/warning" ~detail:"Additional detail."
          ~related:[ related ] ~source:"test" no_region "warning")
  in
  match Diagnostic.Report.to_sorted_list report with
  | [ diagnostic ] ->
      expect_true "warning severity" (diagnostic.severity = Diagnostic.Warning);
      expect "warning source" "test" diagnostic.source;
      expect_true "warning code" (diagnostic.code = Some "test/warning");
      expect_true "warning detail"
        (diagnostic.detail = Some "Additional detail.");
      expect_true "warning related" (diagnostic.related = [ related ])
  | _ -> failwith "warning fields: expected one diagnostic"

let check_warning_action name source message action =
  let _, report = Diagnostic.collect action in
  match Diagnostic.Report.to_sorted_list report with
  | [ diagnostic ] ->
      expect (name ^ " warning message") message diagnostic.message;
      expect (name ^ " warning source") source diagnostic.source;
      expect_true (name ^ " warning code") (diagnostic.code = None);
      expect_true (name ^ " warning detail") (diagnostic.detail = None);
      expect_true (name ^ " warning related") (diagnostic.related = []);
      expect_true (name ^ " warning trace") (diagnostic.trace = [])
  | _ -> failwith (name ^ " warning: expected one diagnostic")

let check_warning_adapter name source emit =
  let message = name ^ " warning" in
  check_warning_action name source message (fun () -> emit message)

let check_prose_warning_adapter () =
  let empty_id = "" $ no_region in
  check_warning_action "prose" "prose" "link with empty target" (fun () ->
      Backend_adoc.Pl.render_func_title Lang.Pl.Annot.empty empty_id [] []
      |> ignore)

let check_single_failtrace_promotion file =
  let root_region = region file 1 0 1 in
  let child_region = region file 1 2 3 in
  let child = failtrace child_region "child failure" [] in
  let root = failtrace root_region "root failure" [ child ] in
  let diagnostic =
    Diagnostic.of_failtraces ~source:"test" ~fallback:"fallback" [ root ]
  in
  expect "single failtrace message" "root failure" diagnostic.message;
  expect_true "single failtrace region" (diagnostic.region = root_region);
  match diagnostic.trace with
  | [ trace ] ->
      expect "single failtrace child message" "child failure" trace.message;
      expect_true "single failtrace child region" (trace.region = child_region);
      expect_true "single failtrace child leaves" (trace.children = [])
  | _ -> failwith "single failtrace: expected one promoted child"

let check_regionless_single_failtrace file =
  let child_region = region file 1 2 3 in
  let child = failtrace child_region "child failure" [] in
  let root = failtrace no_region "root failure" [ child ] in
  let diagnostic =
    Diagnostic.of_failtraces ~source:"test" ~fallback:"fallback" [ root ]
  in
  expect_true "regionless single failtrace region"
    (diagnostic.region = no_region);
  match diagnostic.trace with
  | [ trace ] ->
      expect_true "regionless single failtrace child region"
        (trace.region = child_region)
  | _ -> failwith "regionless single failtrace: expected one promoted child"

let check_multiple_failtrace_fallback file =
  let second_region = region file 2 0 1 in
  let child_region = region file 3 0 1 in
  let child = failtrace child_region "child failure" [] in
  let first = failtrace no_region "first failure" [ child ] in
  let second = failtrace second_region "second failure" [] in
  let diagnostic =
    Diagnostic.of_failtraces ~source:"test" ~fallback:"fallback"
      [ first; second ]
  in
  expect "multiple failtrace message" "fallback" diagnostic.message;
  expect_true "multiple failtrace region" (diagnostic.region = second_region);
  match diagnostic.trace with
  | [ first_trace; second_trace ] ->
      expect "first failtrace message" "first failure" first_trace.message;
      expect_true "first failtrace region" (first_trace.region = no_region);
      (match first_trace.children with
      | [ child_trace ] ->
          expect "multiple failtrace child message" "child failure"
            child_trace.message;
          expect_true "multiple failtrace child region"
            (child_trace.region = child_region);
          expect_true "multiple failtrace child leaves"
            (child_trace.children = [])
      | _ -> failwith "multiple failtrace: expected one child");
      expect "second failtrace message" "second failure" second_trace.message;
      expect_true "second failtrace region" (second_trace.region = second_region);
      expect_true "second failtrace children" (second_trace.children = [])
  | _ -> failwith "multiple failtrace: expected two root traces"

let check_elaboration_attempt_boundary () =
  let file = Filename.temp_file "p4spectec-failtrace-" ".watsup" in
  Fun.protect
    ~finally:(fun () -> Sys.remove file)
    (fun () ->
      let channel = open_out file in
      output_string channel "dec $f : bool\ndef $f = 0\n";
      close_out channel;
      let result, report =
        P4spectec.with_diagnostics (fun () -> P4spectec.elab [ file ])
      in
      expect_true "elaboration attempt result"
        (match result with Error _ -> true | Ok _ -> false);
      let output =
        Diagnostic.Render.render_report ~ansi:Diagnostic.Ansi.plain report
      in
      expect_contains "elaboration attempt message"
        "error: elaboration of expression 0 as type bool failed" output;
      expect_contains "elaboration attempt source" "  | source: elab" output;
      expect_contains "elaboration attempt trace header" "  | trace:" output;
      expect_contains "elaboration attempt rendered child"
        "  | cannot cast nat to bool" output)

let run file =
  List.iteri
    (fun line case ->
      check_region file ~name:case.name ~line:(line + 1) ~left:case.left
        ~right:case.right ~location:case.location ~underline:case.underline)
    cases;
  check_trace_location file;
  check_empty_failtrace_fallback ();
  check_single_failtrace_promotion file;
  check_regionless_single_failtrace file;
  check_multiple_failtrace_fallback file;
  check_coded_diagnostic_source ();
  check_quoting ();
  check_warning_fields file;
  check_warning_adapter "splice" "splice" (Backend_splice.Error.warn no_region);
  check_warning_adapter "interp" "interp" (Interp_common.Error.warn no_region);
  check_warning_adapter "runtime" "runtime" (Error.warn no_region);
  check_prose_warning_adapter ();
  check_elaboration_attempt_boundary ()

let () =
  let file = Filename.temp_file "p4spectec-diagnostic-" ".txt" in
  Fun.protect
    ~finally:(fun () -> Sys.remove file)
    (fun () ->
      write_source file;
      run file)
