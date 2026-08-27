open Util.Source
module Diagnostic = P4spectec.Diagnostic

let fail name expected actual =
  failwith (Printf.sprintf "%s: expected %S, but got %S" name expected actual)

let expect name expected actual =
  if not (String.equal expected actual) then fail name expected actual

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
    Diagnostic.error ~code:"test" ~source:"test"
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
    Diagnostic.error ~code:"test" ~trace:[ trace ] ~source:"test" no_region
      "failure"
  in
  let output =
    Diagnostic.Report.singleton diagnostic
    |> Diagnostic.Render.render_report ~ansi:Diagnostic.Ansi.plain
  in
  expect_contains "trace location" (Printf.sprintf "%s:1.2-1.3" file) output

let run file =
  List.iteri
    (fun line case ->
      check_region file ~name:case.name ~line:(line + 1) ~left:case.left
        ~right:case.right ~location:case.location ~underline:case.underline)
    cases;
  check_trace_location file

let () =
  let file = Filename.temp_file "p4spectec-diagnostic-" ".txt" in
  Fun.protect
    ~finally:(fun () -> Sys.remove file)
    (fun () ->
      write_source file;
      run file)
