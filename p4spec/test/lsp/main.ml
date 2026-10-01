module Lsp = Linol_eio
module Check = P4spectec_lsp.Check

let write_file path contents =
  Out_channel.with_open_bin path (fun channel -> output_string channel contents)

let rec remove path =
  if (Unix.lstat path).st_kind = Unix.S_DIR then (
    Array.iter
      (fun entry -> remove (Filename.concat path entry))
      (Sys.readdir path);
    Unix.rmdir path)
  else Sys.remove path

let with_directory f =
  let path = Filename.temp_file "p4spectec-lsp-" "" in
  Sys.remove path;
  Unix.mkdir path 0o700;
  Fun.protect ~finally:(fun () -> remove path) (fun () -> f path)

let print_diagnostics ~root = function
  | [] -> print_endline "(no diagnostics)"
  | diagnostics ->
      List.iter
        (fun d ->
          let json = Yojson.Safe.to_string (Lsp.Diagnostic.yojson_of_t d) in
          print_endline
            (Str.global_replace (Str.regexp_string root) "<root>" json))
        diagnostics

let case ?position_encoding name text =
  Printf.printf "## %s\n" name;
  print_diagnostics ~root:(Sys.getcwd ())
    (Check.run ?position_encoding ~path:"standalone.watsup" text);
  print_newline ()

let check condition message = if not condition then failwith message

let single = function
  | [ diagnostic ] -> diagnostic
  | _ -> failwith "expected one diagnostic"

let discovery () =
  with_directory (fun root ->
      let file name = Filename.concat root name in
      write_file (file "project.spec") "";
      write_file (file "10-def.watsup") "syntax t = T\n";
      write_file (file "2-use.watsup") "syntax u = t\n";
      Unix.mkdir (file "include") 0o700;
      write_file (file "include/excluded.watsup") "@";
      check
        (List.map Filename.basename (P4spectec.collect_spec_files root)
        = [ "10-def.watsup"; "2-use.watsup" ])
        "spec file order or include exclusion differs from CLI";
      check
        (Check.run ~path:(file "2-use.watsup") "syntax u = t\n" = [])
        "cross-file type did not resolve";
      let diagnostic =
        single (Check.run ~path:(file "10-def.watsup") "syntax x = X\n")
      in
      check
        (diagnostic.relatedInformation <> None)
        "sibling error lost its original location";
      Printf.printf "## sibling error from edited definition\n";
      print_diagnostics ~root [ diagnostic ];
      print_newline ();
      check
        (Check.run ~path:(file "10-def.watsup") "syntax t = T\n" = [])
        "diagnostic was cached after the buffer was repaired";
      check
        (Check.run ~path:(file "new.watsup") "syntax v = t\n" = [])
        "new buffer inside spec root was omitted";
      Unix.mkdir (file "nested") 0o700;
      write_file (file "nested/nested.spec") "";
      check
        (P4spectec.spec_root_of_file (file "nested/new.watsup")
        = Some (file "nested"))
        "nearest spec root was not selected";
      check
        (Check.run ~path:(file "nested/new.watsup") "syntax v = t\n" <> [])
        "nested spec used definitions from its parent";
      print_endline "discovery, overlay, repair, new file, nested root: ok")

let new_file_order () =
  List.iter
    (fun (definition, use) ->
      with_directory (fun root ->
          let file name = Filename.concat root name in
          let definition = file definition in
          let use = file use in
          let directory = Filename.dirname definition in
          if directory <> root then Unix.mkdir directory 0o700;
          write_file (file "project.spec") "";
          write_file use "syntax u = t\n";
          let text = "syntax t = T\n" in
          let files =
            P4spectec.collect_spec_files ~include_file:definition root
          in
          check
            (Check.run ~path:definition text = [])
            "new declaration was checked after its use";
          write_file definition text;
          check
            (P4spectec.collect_spec_files root = files)
            "saving a new file changed spec file order";
          check
            (Check.run ~path:definition text = [])
            "saved declaration did not resolve"))
    [ ("0-def.watsup", "1-use.watsup"); ("a/def.watsup", "a.watsup") ];
  print_endline "new file order before and after save: ok"

let symlink_roots () =
  with_directory (fun root ->
      let file name = Filename.concat root name in
      List.iter
        (fun dir -> Unix.mkdir (file dir) 0o700)
        [ "common"; "al"; "sl" ];
      write_file (file "common/defs.watsup") "syntax t = T\n";
      List.iter
        (fun dir ->
          write_file (file (dir ^ "/meta.spec")) "";
          Unix.symlink "../common" (file (dir ^ "/0-common")))
        [ "al"; "sl" ];
      write_file (file "al/1-use.watsup") "syntax u = t\n";
      write_file (file "sl/1-use.watsup") "syntax u = x\n";
      let text = "syntax x = X\n" in
      check
        (Check.run ~path:(file "al/0-common/defs.watsup") text <> [])
        "AL symlink lost its root or ignored the buffer";
      check
        (Check.run ~path:(file "sl/0-common/defs.watsup") text = [])
        "SL symlink used the wrong spec root";
      print_endline "symlink roots: ok")

let failures () =
  with_directory (fun root ->
      let file name = Filename.concat root name in
      write_file (file "project.spec") "";
      let socket = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
      let cwd = Sys.getcwd () in
      Fun.protect
        ~finally:(fun () ->
          Sys.chdir cwd;
          Unix.close socket)
        (fun () ->
          Sys.chdir root;
          Unix.bind socket (Unix.ADDR_UNIX "unreadable.watsup");
          let diagnostic = single (Check.run ~path:(file "open.watsup") "") in
          check
            (diagnostic.source = Some "io")
            "unreadable sibling was not reported");
      Sys.remove (file "unreadable.watsup");
      Unix.symlink "broken.watsup" (file "broken.watsup");
      let diagnostic = single (Check.run ~path:(file "open.watsup") "") in
      check
        (diagnostic.source = Some "internal")
        "unexpected discovery exception escaped the checker";
      Sys.remove (file "broken.watsup");
      check
        (Check.run ~path:(file "open.watsup") "syntax t = T\n" = [])
        "checker did not recover after an exception";
      print_endline "I/O failure, exception containment, recovery: ok")

let named_sources () =
  let sources = P4spectec.[ { filename = "buffer.watsup"; contents = "@" } ] in
  match P4spectec.parse_sources sources with
  | Error diagnostic ->
      check
        (diagnostic.region.left.file = "buffer.watsup")
        "buffer filename was not preserved";
      print_endline "named source regions: ok"
  | Ok _ -> failwith "invalid buffer parsed successfully"

let () =
  case "valid" "syntax type =\n  | INT\n  | type -> type\n";
  case "parse error" "syntax type =\n  | INT\n  | type ->\n";
  case "unclosed text literal" "\"abc";
  case "elab error" "syntax t = foo\n";
  case "warning"
    "syntax foo = _FOO\nrelation Foo_ok: |- foo\nrule Foo_ok/base:\n  |- _FOO\n";
  case "related location" "dec $f(nat, nat): nat\ndec $g: nat\ndef $g = $f(0)\n";
  case "detail"
    "syntax foo = nat\n\
     relation R: foo |- foo\n\
    \  hint(input %0)\n\
     rule R/iter:\n\
    \  0 |- 0\n\
    \  -- (otherwise)*\n";
  case "failure trace" "dec $f: nat\ndef $f = true\n";
  case "UTF-16" "(; 😀é ;) @\n";
  case ~position_encoding:`UTF8 "UTF-8" "(; 😀é ;) @\n";
  let unicode = single (Check.run ~path:"unicode.watsup" "(; 😀é ;) @\n") in
  check (unicode.range.start.character = 10) "UTF-16 range uses byte columns";
  named_sources ();
  discovery ();
  new_file_order ();
  symlink_roots ();
  failures ()
