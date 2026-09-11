open Lang
open Splicer
open Splicers

(* Splicers *)

let splicers =
  [
    (module Syntax.Source.Splicer : SPLICER);
    (module Rel_title.Source.Splicer : SPLICER);
    (module Rel_title.Latex.Splicer : SPLICER);
    (module Rel_title.Prose.Splicer : SPLICER);
    (module Rulegroup_else.Prose.Splicer : SPLICER);
    (module Rulegroup.Source.Splicer : SPLICER);
    (module Rulegroup.Latex.Splicer : SPLICER);
    (module Rulegroup.Prose.Splicer : SPLICER);
    (module Rulegroup_dispatch.Prose.Splicer : SPLICER);
    (module Func_title.Source.Splicer : SPLICER);
    (module Func_title.Latex.Splicer : SPLICER);
    (module Func_title.Prose.Splicer : SPLICER);
    (module Func.Source.Splicer : SPLICER);
    (module Func.Latex.Splicer : SPLICER);
    (module Func.Prose.Splicer : SPLICER);
    (module Table.Source.Splicer : SPLICER);
    (module Table.Latex.Splicer : SPLICER);
    (module Table.Prose.Splicer : SPLICER);
  ]

let init ?(context : Ctx.t = Ctx.empty) (spec_el : El.spec) (spec_pl : Pl.spec)
    : unit =
  Ctx.reset_anchors context;
  List.iter
    (fun (module S : SPLICER) -> S.init ~context spec_el spec_pl)
    splicers

(* Splicing *)

let rec try_splice_anchor (module S : SPLICER) (source : Source.t)
    (result : string ref) : bool =
  let parsed_start = Parser.parse_splice_start source S.name in
  if parsed_start then try_splice_anchor' (module S : SPLICER) source result;
  parsed_start

and try_splice_anchor' (module S : SPLICER) (source : Source.t)
    (result : string ref) : unit =
  Parser.parse_space source;
  result := S.splice source

and try_splice_anchors (source : Source.t) (buffer : Buffer.t) : bool =
  let result = ref "" in
  let spliced =
    splicers
    |> List.fold_left
         (fun spliced (module S : SPLICER) ->
           if spliced then true
           else try_splice_anchor (module S : SPLICER) source result)
         false
  in
  if spliced then (
    Buffer.add_string buffer !result;
    true)
  else false

(* File system helper *)

let gen_directory (filename : string) : unit =
  let rec gen_directory' (dirname : string) =
    if not (Sys.file_exists dirname) then (
      let dirname_parent = Filename.dirname dirname in
      if dirname_parent <> dirname then gen_directory' dirname_parent;
      Unix.mkdir dirname 0o755)
  in
  let dirname = Filename.dirname filename in
  if dirname <> "" && not (Sys.file_exists dirname) then gen_directory' dirname

let error_io (filename : string) (msg : string) =
  Error.error ~code:File_io_error
    (Util.Source.region_of_file filename)
    ("I/O error: " ^ Diagnostic.quote msg)

let read_file (filename : string) : string =
  try In_channel.with_open_bin filename In_channel.input_all
  with Sys_error msg -> error_io filename msg

let write_file (filename : string) (content : string) : unit =
  try
    gen_directory filename;
    Out_channel.with_open_bin filename (fun oc ->
        Out_channel.output_string oc content)
  with
  | Sys_error msg -> error_io filename msg
  | Unix.Unix_error (err, _, _) -> error_io filename (Unix.error_message err)

(* Entry points *)

let rec splice (source : Source.t) (buffer : Buffer.t) : unit =
  if not (Source.eos source) then (
    if not (try_splice_anchors source buffer) then (
      Buffer.add_char buffer (Source.get source);
      Source.adv source);
    splice source buffer)

let splice_string (source : Source.t) (content : string) : string =
  let buffer = Buffer.create (String.length content) in
  splice source buffer;
  Buffer.contents buffer

let splice_file (filename_input : string) (filename_output : string) : unit =
  let content = read_file filename_input in
  let source = Source.{ file = filename_input; s = content; i = 0 } in
  write_file filename_output (splice_string source content)

let splice_files (spec_el : El.spec) (spec_pl : Pl.spec)
    (filenames : (string * string) list) : unit =
  let sources =
    List.map
      (fun (filename_input, _) -> (filename_input, read_file filename_input))
      filenames
  in
  let context = Anchor.collect spec_el sources in
  init ~context spec_el spec_pl;
  List.iter
    (fun (filename_input, filename_output) ->
      splice_file filename_input filename_output)
    filenames;
  List.iter (fun (module S : SPLICER) -> S.warn_unused ()) splicers
