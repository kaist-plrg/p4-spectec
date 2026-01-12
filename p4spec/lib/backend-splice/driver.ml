open Lang
open Splicer
open Splicers

(* Splicers *)

let splicers =
  [
    (module Syntax.Source.Splicer : SPLICER);
    (module Rel_title.Source.Splicer : SPLICER);
    (module Rel_title.Prose.Splicer : SPLICER);
    (module Rulegroup.Source.Splicer : SPLICER);
    (module Rulegroup.Prose.Splicer : SPLICER);
    (module Func_title.Source.Splicer : SPLICER);
    (module Func_title.Prose.Splicer : SPLICER);
    (module Func.Source.Splicer : SPLICER);
    (module Func.Prose.Splicer : SPLICER);
    (module Table.Source.Splicer : SPLICER);
    (module Table.Prose.Splicer : SPLICER);
  ]

let init (spec_el : El.spec) (spec_pl : Pl.spec) =
  List.iter (fun (module S : SPLICER) -> S.init spec_el spec_pl) splicers

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
  let ic = open_in filename_input in
  let content =
    Fun.protect
      (fun () -> In_channel.input_all ic)
      ~finally:(fun () -> In_channel.close ic)
  in
  let source = Source.{ file = filename_input; s = content; i = 0 } in
  let content_spliced = splice_string source content in
  gen_directory filename_output;
  let oc = open_out filename_output in
  Fun.protect
    (fun () -> Out_channel.output_string oc content_spliced)
    ~finally:(fun () -> Out_channel.close oc)

let splice_files (spec_el : El.spec) (spec_pl : Pl.spec)
    (filenames : (string * string) list) : unit =
  init spec_el spec_pl;
  List.iter
    (fun (filename_input, filename_output) ->
      splice_file filename_input filename_output)
    filenames;
  List.iter (fun (module S : SPLICER) -> S.warn_unused ()) splicers
