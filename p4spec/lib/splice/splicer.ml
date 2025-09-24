open Error

(* File system helpers *)

let gen_directory (filename : string) : unit =
  let rec gen_directory' (dirname : string) =
    if not (Sys.file_exists dirname) then (
      let dirname_parent = Filename.dirname dirname in
      if dirname_parent <> dirname then gen_directory' dirname_parent;
      Unix.mkdir dirname 0o755)
  in
  let dirname = Filename.dirname filename in
  if dirname <> "" && not (Sys.file_exists dirname) then gen_directory' dirname

(* Parsing the skeleton document *)

let rec try_string' (s : string) (i : int) (s_expect : string) (j : int) : bool
    =
  j = String.length s_expect
  || (s.[i] = s_expect.[j] && try_string' s (i + 1) s_expect (j + 1))

let try_string (source : Source.t) (s : string) : bool =
  Source.left source >= String.length s
  && try_string' source.s source.i s 0
  &&
  (Source.advn source (String.length s);
   true)

let rec parse_space (source : Source.t) : unit =
  if
    (not (Source.eos source))
    && (Source.get source = ' '
       || Source.get source = '\t'
       || Source.get source = '\n')
  then (
    Source.adv source;
    parse_space source)

let parse_anchor_start (source : Source.t) (anchor : Anchor.t) : bool =
  let start =
    match anchor with Syntax { start; _ } -> start | _ -> failwith "TODO"
  in
  try_string source (start ^ "{")

let rec parse_id' (source : Source.t) : unit =
  if not (Source.eos source) then
    match Source.get source with
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' | '`' | '-' | '*' | '.'
      ->
        Source.adv source;
        parse_id' source
    | _ -> ()

let parse_id (source : Source.t) : string =
  let i_prev = source.i in
  parse_id' source;
  if i_prev = source.i then
    error Util.Source.no_region "cannot parse identifier";
  Source.str source i_prev

let parse_target (source : Source.t) : string * string =
  let id = parse_id source in
  let id_sub = if try_string source "/" then parse_id source else "" in
  (id, id_sub)

let rec parse_targets (source : Source.t) : (string * string) list =
  parse_space source;
  if try_string source "}" then []
  else
    let target = parse_target source in
    target :: parse_targets source

(* Splicing an anchor *)

let try_splice_syntax_anchor (ctx : Ctx.t) (source : Source.t) (prefix : string)
    (suffix : string) : string option =
  match try_string source "syntax:" with
  | true ->
      let targets = parse_targets source in
      let defs_el = Ctx.find_syntax_defs ctx targets in
      let content =
        defs_el |> List.map El.Render.render_def |> String.concat "\n\n"
      in
      let content = prefix ^ content ^ suffix in
      Some content
  | false -> None

let splice_anchor (ctx : Ctx.t) (source : Source.t) (buffer : Buffer.t)
    (i_start : int) (anchor : Anchor.t) : unit =
  parse_space source;
  let result =
    match anchor with
    | Syntax { prefix; suffix; _ } ->
        try_splice_syntax_anchor ctx source prefix suffix
    | _ -> failwith "TODO"
  in
  match result with Some s -> Buffer.add_string buffer s | None -> ()

(* Driver for the splicing process *)

let rec try_anchors (ctx : Ctx.t) (source : Source.t) (buffer : Buffer.t) =
  function
  | [] -> false
  | anchor :: anchors -> (
      let i = source.i in
      match parse_anchor_start source anchor with
      | true ->
          splice_anchor ctx source buffer i anchor;
          true
      | false -> try_anchors ctx source buffer anchors)

let rec splice (ctx : Ctx.t) (source : Source.t) (buffer : Buffer.t) : unit =
  if not (Source.eos source) then (
    if not (try_anchors ctx source buffer ctx.anchors) then (
      Buffer.add_char buffer (Source.get source);
      Source.adv source);
    splice ctx source buffer)

(* Entry points *)

let splice_string (ctx : Ctx.t) (source : Source.t) (content : string) : string
    =
  let buffer = Buffer.create (String.length content) in
  splice ctx source buffer;
  Buffer.contents buffer

let splice_file (spec_el : El.Ast.spec) (filename_input : string)
    (filename_output : string) : unit =
  let ic = open_in filename_input in
  let content =
    Fun.protect
      (fun () -> In_channel.input_all ic)
      ~finally:(fun () -> In_channel.close ic)
  in
  let ctx = Ctx.init spec_el in
  let source = Source.{ file = filename_input; s = content; i = 0 } in
  let content_spliced = splice_string ctx source content in
  gen_directory filename_output;
  let oc = open_out filename_output in
  Fun.protect
    (fun () -> Out_channel.output_string oc content_spliced)
    ~finally:(fun () -> Out_channel.close oc)

let splice_files (spec_el : El.Ast.spec) (filenames : (string * string) list) :
    unit =
  List.iter
    (fun (filename_input, filename_output) ->
      splice_file spec_el filename_input filename_output)
    filenames
