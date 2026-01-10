open Lang

(* Splicing an anchor *)

let do_splice_anchor (module S : Splice.SPLICE) (ctx : Ctx.t)
    (source : Source.t) : string =
  let keys = S.parse_keys source in
  let values = S.find_values ctx keys in
  let content = S.render keys values in
  S.prefix ^ content ^ S.suffix

let rec try_splice_anchor (module S : Splice.SPLICE) (ctx : Ctx.t)
    (source : Source.t) (result : string ref) : bool =
  let parsed_start = Parser.parse_splice_start source S.name in
  if parsed_start then
    try_splice_anchor' (module S : Splice.SPLICE) ctx source result;
  parsed_start

and try_splice_anchor' (module S : Splice.SPLICE) (ctx : Ctx.t)
    (source : Source.t) (result : string ref) : unit =
  Parser.parse_space source;
  result := do_splice_anchor (module S : Splice.SPLICE) ctx source

and try_splice_anchors (ctx : Ctx.t) (source : Source.t) (buffer : Buffer.t) =
  let result = ref "" in
  ignore
    (try_splice_anchor (module Splicer.Syntax) ctx source result
    || try_splice_anchor (module Splicer.RelTitleSource) ctx source result
    || try_splice_anchor (module Splicer.RelTitleProse) ctx source result
    || try_splice_anchor (module Splicer.RuleGroupSource) ctx source result
    || try_splice_anchor (module Splicer.RuleGroupProse) ctx source result
    || try_splice_anchor (module Splicer.FuncTitleSource) ctx source result
    || try_splice_anchor (module Splicer.FuncTitleProse) ctx source result
    || try_splice_anchor (module Splicer.FuncSource) ctx source result
    || try_splice_anchor (module Splicer.FuncProse) ctx source result
    || try_splice_anchor (module Splicer.TableSource) ctx source result
    || try_splice_anchor (module Splicer.TableProse) ctx source result);
  if !result <> "" then (
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

let rec splice (ctx : Ctx.t) (source : Source.t) (buffer : Buffer.t) : unit =
  if not (Source.eos source) then (
    if not (try_splice_anchors ctx source buffer) then (
      Buffer.add_char buffer (Source.get source);
      Source.adv source);
    splice ctx source buffer)

let splice_string (ctx : Ctx.t) (source : Source.t) (content : string) : string
    =
  let buffer = Buffer.create (String.length content) in
  splice ctx source buffer;
  Buffer.contents buffer

let splice_file (spec_el : El.spec) (spec_pl : Pl.spec)
    (filename_input : string) (filename_output : string) : unit =
  let ic = open_in filename_input in
  let content =
    Fun.protect
      (fun () -> In_channel.input_all ic)
      ~finally:(fun () -> In_channel.close ic)
  in
  let ctx = Ctx.init spec_el spec_pl filename_input in
  let source = Source.{ file = filename_input; s = content; i = 0 } in
  let content_spliced = splice_string ctx source content in
  gen_directory filename_output;
  let oc = open_out filename_output in
  Fun.protect
    (fun () -> Out_channel.output_string oc content_spliced)
    ~finally:(fun () -> Out_channel.close oc)

let splice_files (spec_el : El.spec) (spec_pl : Pl.spec)
    (filenames : (string * string) list) : unit =
  List.iter
    (fun (filename_input, filename_output) ->
      splice_file spec_el spec_pl filename_input filename_output)
    filenames
