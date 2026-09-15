open Util.Source

let severity_label : Record.severity -> string = function
  | Error -> "error"
  | Warning -> "warning"

let severity_styles : Record.severity -> Ansi.style list = function
  | Error -> [ Bold; Red ]
  | Warning -> [ Bold; Yellow ]

(* Inline-code styling requires paired backticks. *)
let style_inline_code ~ansi ~outer (s : string) : string =
  let parts = String.split_on_char '`' s in
  if List.length parts mod 2 = 0 then Ansi.style ansi outer s
  else
    List.mapi
      (fun i p ->
        if i mod 2 = 0 then Ansi.style ansi outer p
        else Ansi.style ansi [ Bold; Cyan ] ("`" ^ p ^ "`"))
      parts
    |> String.concat ""

let render_header ~ansi (d : Record.t) : string =
  let label = severity_label d.severity in
  let code = match d.code with Some c -> "[" ^ c ^ "]" | None -> "" in
  let prefix =
    Ansi.style ansi (severity_styles d.severity) (label ^ code ^ ": ")
  in
  let message = style_inline_code ~ansi ~outer:[ Bold ] d.message in
  prefix ^ message

let fold_utf8_range text byte_start byte_end f init =
  let text_length = String.length text in
  let byte_start = min text_length (max 0 byte_start) in
  let byte_end = min text_length (max byte_start byte_end) in
  if byte_start = byte_end then init
  else
    Uutf.String.fold_utf_8 ~pos:byte_start ~len:(byte_end - byte_start) f init
      text

let virtual_width text byte_start byte_end =
  max 0 (byte_end - max (String.length text) byte_start)

let utf8_character_count text byte_start byte_end =
  let byte_start = max 0 byte_start in
  let byte_end = max byte_start byte_end in
  let count characters _ = function
    | `Uchar _ -> characters + 1
    | `Malformed bytes -> characters + String.length bytes
  in
  fold_utf8_range text byte_start byte_end count 0
  + virtual_width text byte_start byte_end

let tab_width column = 8 - (column mod 8)

let uchar_terminal_width uchar =
  let width = Uucp.Break.tty_width_hint uchar in
  if width < 0 then 1 else width

let grapheme_width grapheme =
  let add (width, emoji, emoji_presentation, regional_count) _ = function
    | `Malformed bytes ->
        (width + String.length bytes, emoji, emoji_presentation, regional_count)
    | `Uchar uchar ->
        ( width + uchar_terminal_width uchar,
          emoji || Uucp.Emoji.is_emoji uchar,
          emoji_presentation
          || Uucp.Emoji.is_emoji_presentation uchar
          || Uchar.to_int uchar = 0xfe0f,
          regional_count
          + if Uucp.Break.grapheme_cluster uchar = `RI then 1 else 0 )
  in
  let width, emoji, emoji_presentation, regional_count =
    Uutf.String.fold_utf_8 add (0, false, false, 0) grapheme
  in
  if (emoji && emoji_presentation) || regional_count = 2 then 2 else width

let column_after_valid_utf8 text ~column byte_start byte_end =
  let substring = String.sub text byte_start (byte_end - byte_start) in
  Uuseg_string.fold_utf_8 `Grapheme_cluster
    (fun column grapheme ->
      if grapheme = "\t" then column + tab_width column
      else column + grapheme_width grapheme)
    column substring

let terminal_width text ~column byte_start byte_end =
  let start_column = column in
  let byte_start = max 0 byte_start in
  let byte_end = max byte_start byte_end in
  let actual_start = min (String.length text) byte_start in
  let actual_end = min (String.length text) byte_end in
  let advance (column, valid_start) position = function
    | `Uchar _ -> (column, valid_start)
    | `Malformed bytes ->
        let column =
          column_after_valid_utf8 text ~column valid_start position
          + String.length bytes
        in
        (column, position + String.length bytes)
  in
  let current_column, valid_start =
    fold_utf8_range text actual_start actual_end advance (column, actual_start)
  in
  let final_column =
    column_after_valid_utf8 text ~column:current_column valid_start actual_end
  in
  final_column - start_column + virtual_width text byte_start byte_end

let underline_range ~(left : pos) ~(right : pos) ~lineno ~text =
  let col_start = if lineno = left.line then max 0 left.column else 0 in
  let col_end =
    if lineno = right.line then max (col_start + 1) right.column
    else max (col_start + 1) (String.length text)
  in
  (col_start, col_end)

let lineno_gutter ~width n =
  let s = string_of_int n in
  String.make (max 0 (width - String.length s)) ' ' ^ s

let range_inclusive lo hi = List.init (hi - lo + 1) (fun i -> lo + i)

let render_snippet ~ansi ~cache ~indent ~underline_style ~(left : pos)
    ~(right : pos) : string option =
  if right.line < left.line then None
  else
    match Source_cache.get_line cache left.file left.line with
    | None -> None
    | Some _ ->
        let gutter_width = String.length (string_of_int right.line) in
        let blank_gutter = String.make gutter_width ' ' in
        let render_line_block lineno text =
          let byte_start, byte_end =
            underline_range ~left ~right ~lineno ~text
          in
          let text_column = String.length indent + gutter_width + 3 in
          let indent_width =
            terminal_width text ~column:text_column 0 byte_start
          in
          let caret_width =
            terminal_width text
              ~column:(text_column + indent_width)
              byte_start byte_end
          in
          let underline =
            String.make indent_width ' '
            ^ Ansi.style ansi underline_style
                (String.make (max 1 caret_width) '^')
          in
          Printf.sprintf "%s%s | %s\n%s%s | %s" indent
            (Ansi.style ansi [ Dim ] (lineno_gutter ~width:gutter_width lineno))
            text indent blank_gutter underline
        in
        let opening = Printf.sprintf "%s%s |" indent blank_gutter in
        Some
          (range_inclusive left.line right.line
          |> List.filter_map (fun lineno ->
                 Source_cache.get_line cache left.file lineno
                 |> Option.map (render_line_block lineno))
          |> List.cons opening |> String.concat "\n")

let display_position cache (position : pos) : pos =
  match Source_cache.get_line cache position.file position.line with
  | Some text ->
      { position with column = utf8_character_count text 0 position.column }
  | None -> position

let display_region cache (region : region) : region =
  {
    left = display_position cache region.left;
    right = display_position cache region.right;
  }

let render_region_block ~ansi ~cache ~indent ~underline_style (region : region)
    : string =
  let arrow = indent ^ Ansi.style ansi [ Bold; Blue ] "  --> " in
  let loc =
    if region = region_of_file region.left.file then region.left.file
    else
      let left = display_position cache region.left in
      Printf.sprintf "%s:%d:%d" left.file left.line (left.column + 1)
  in
  let arrow_line = arrow ^ loc in
  match
    render_snippet ~ansi ~cache ~indent ~underline_style ~left:region.left
      ~right:region.right
  with
  | None -> arrow_line
  | Some snippet -> arrow_line ^ "\n" ^ snippet

let render_location ~ansi ~cache (d : Record.t) : string option =
  if d.region = no_region then None
  else
    Some
      (render_region_block ~ansi ~cache ~indent:""
         ~underline_style:(severity_styles d.severity)
         d.region)

let snippet_gutter (d : Record.t) : string =
  if d.region = no_region then ""
  else String.make (String.length (string_of_int d.region.left.line)) ' '

(* [|] prefix aligns annotation fields under the snippet border *)
let border_prefix (d : Record.t) : string =
  if d.region = no_region then "  " else " " ^ snippet_gutter d ^ "| "

(* Code-bearing diagnostics already name the pass via [code]. *)
let show_source (d : Record.t) : bool = d.source <> "" && d.code = None

let field_separator (d : Record.t) : string =
  if d.region = no_region then "\n\n" else "\n " ^ snippet_gutter d ^ "|\n"

let render_source_tag ~ansi (d : Record.t) : string option =
  if not (show_source d) then None
  else
    Some
      (border_prefix d
      ^ Ansi.style ansi [ Dim ] (Printf.sprintf "source: %s" d.source))

let split_words (s : string) : string list =
  let words = ref [] in
  let buf = Buffer.create 16 in
  let in_code = ref false in
  let flush () =
    if Buffer.length buf > 0 then (
      words := Buffer.contents buf :: !words;
      Buffer.clear buf)
  in
  String.iter
    (fun c ->
      match c with
      | '`' ->
          in_code := not !in_code;
          Buffer.add_char buf c
      | (' ' | '\t') when not !in_code -> flush ()
      | _ -> Buffer.add_char buf c)
    s;
  flush ();
  List.rev !words

let wrap_prose ~ansi ~max_width ~first_line_prefix_width ~continuation_prefix
    (s : string) : string =
  let style word =
    let n = String.length word in
    if n >= 2 && word.[0] = '`' && word.[n - 1] = '`' then
      Ansi.style ansi [ Bold; Cyan ] word
    else word
  in
  let render_line words = String.concat " " (List.rev_map style words) in
  let first_line_budget = max 1 (max_width - first_line_prefix_width) in
  let continuation_budget =
    max 1 (max_width - String.length continuation_prefix)
  in
  let rec fill_one_line budget line line_width = function
    | [] -> (line, [])
    | word :: rest as remaining ->
        let word_width = String.length word in
        let with_word =
          if line = [] then word_width else line_width + 1 + word_width
        in
        if line <> [] && with_word > budget then (line, remaining)
        else fill_one_line budget (word :: line) with_word rest
  in
  let rec produce_lines budget lines remaining =
    let line, rest = fill_one_line budget [] 0 remaining in
    let rendered = render_line line in
    if rest = [] then List.rev (rendered :: lines)
    else produce_lines continuation_budget (rendered :: lines) rest
  in
  produce_lines first_line_budget [] (split_words s)
  |> String.concat ("\n" ^ continuation_prefix)

let render_detail ~ansi (d : Record.t) : string option =
  match d.detail with
  | None -> None
  | Some s ->
      let prefix = border_prefix d in
      let label = "note: " in
      let continuation_prefix =
        prefix ^ String.make (String.length label) ' '
      in
      let wrapped =
        wrap_prose ~ansi ~max_width:80
          ~first_line_prefix_width:(String.length continuation_prefix)
          ~continuation_prefix s
      in
      Some (prefix ^ Ansi.style ansi [ Bold; Cyan ] label ^ wrapped)

let render_related ~ansi ~cache (d : Record.t) : string option =
  if d.related = [] then None
  else
    let one (r : Record.related) =
      let header =
        border_prefix d
        ^ Ansi.style ansi [ Bold; Blue ] "related: "
        ^ style_inline_code ~ansi ~outer:[] r.message
      in
      if r.region = no_region then header
      else
        header ^ "\n"
        ^ render_region_block ~ansi ~cache ~indent:(border_prefix d)
            ~underline_style:[ Bold; Blue ] r.region
    in
    Some (List.map one d.related |> String.concat (field_separator d))

(* [render_node_label] separates a region from its message to limit line width. *)
let render_node_label ~ansi ~cache ~prefix ~body_prefix region message =
  let msg = style_inline_code ~ansi ~outer:[] message in
  if region = no_region then [ prefix ^ msg ]
  else
    [
      prefix
      ^ Ansi.style ansi [ Dim ] (string_of_region (display_region cache region));
      body_prefix ^ msg;
    ]

let rec render_trace_node ~ansi ~cache ~indent ~is_last
    (node : Record.trace_node) : string =
  let { Record.region; message; children } = node in
  let connector =
    Ansi.style ansi [ Dim ] (if is_last then "└── " else "├── ")
  in
  let child_indent =
    indent ^ if is_last then "    " else Ansi.style ansi [ Dim ] "│   "
  in
  let label =
    render_node_label ~ansi ~cache ~prefix:(indent ^ connector)
      ~body_prefix:child_indent region message
  in
  String.concat "\n"
    (label @ render_children ~ansi ~cache ~indent:child_indent children)

and render_children ~ansi ~cache ~indent children =
  let n = List.length children in
  List.mapi
    (fun i c -> render_trace_node ~ansi ~cache ~indent ~is_last:(i = n - 1) c)
    children

let render_trace ~ansi ~cache (d : Record.t) : string option =
  if d.trace = [] then None
  else
    let header = border_prefix d ^ Ansi.style ansi [ Bold; Blue ] "trace:" in
    let indent = border_prefix d in
    let render_root (node : Record.trace_node) =
      let { Record.region; message; children } = node in
      let label =
        render_node_label ~ansi ~cache ~prefix:indent ~body_prefix:indent region
          message
      in
      String.concat "\n" (label @ render_children ~ansi ~cache ~indent children)
    in
    Some (String.concat "\n" (header :: List.map render_root d.trace))

let render ?(show_trace = true) ~ansi ~cache (d : Record.t) : string =
  let intro =
    [ Some (render_header ~ansi d); render_location ~ansi ~cache d ]
    |> List.filter_map Fun.id |> String.concat "\n"
  in
  let fields =
    List.filter_map Fun.id
      [
        render_source_tag ~ansi d;
        render_detail ~ansi d;
        render_related ~ansi ~cache d;
        (if show_trace then render_trace ~ansi ~cache d else None);
      ]
  in
  String.concat (field_separator d) (intro :: fields)

let render_report ?(show_trace = true) ~ansi report =
  let cache = Source_cache.create () in
  Record.Report.to_sorted_list report
  |> List.map (render ~show_trace ~ansi ~cache)
  |> String.concat "\n\n"
