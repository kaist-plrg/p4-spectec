open Utils
module F = Format

(* Types *)

type prose =
  | TextP of string
  | CodeP of code
  | LinkP of link * prose
  | FallthroughP of string * fallthrough_label
  | SeqP of prose list
  | EmptyP

and code =
  | TokenC of string
  | LinkC of link * code
  | SeqC of code list
  | EmptyC

and link = Direct of string | Subject of subject
and subject = Function of string | Relation of string
and fallthrough_label = Derived | Explicit of string

type item_kind = Ordered of string option | Unordered

type block =
  | EmptyB
  | RawB of string
  | InlineB of prose
  | ItemB of int * item_kind * prose * block
  | ConcatB of block list
  | SeqB of block list
  | TableB of int * prose list * string list list

(* Prose constructors *)

let text (s : string) : prose = TextP s
let code_prose (c : code) : prose = CodeP c
let link_prose ~(target : string) (p : prose) : prose = LinkP (Direct target, p)

let link_subject_prose (subject : subject) (p : prose) : prose =
  LinkP (Subject subject, p)

let fallthrough_prose ~(anchor : string) ~(label : fallthrough_label) : prose =
  FallthroughP (anchor, label)

let seq_prose (ps : prose list) : prose = SeqP ps
let empty_prose : prose = EmptyP

(* Code constructors *)

let token (s : string) : code = TokenC s
let link_code ~(target : string) (c : code) : code = LinkC (Direct target, c)

let link_subject_code (subject : subject) (c : code) : code =
  LinkC (Subject subject, c)

let seq_code (cs : code list) : code = SeqC cs
let empty_code : code = EmptyC

(* Block constructors *)

let raw_block (s : string) : block = RawB s
let inline_block (d : prose) : block = InlineB d

let item_ordered_block ~(level : int) ?anchor:anchor_opt ?(block_body = EmptyB)
    (prose_head : prose) : block =
  ItemB (level, Ordered anchor_opt, prose_head, block_body)

let item_unordered_block ~(level : int) ?(block_body = EmptyB)
    (prose_head : prose) : block =
  ItemB (level, Unordered, prose_head, block_body)

let concat_block (ts : block list) : block = ConcatB ts
let seq_block (ts : block list) : block = SeqB ts

let table_block ~(cols : int) ~(header : prose list) (rows : string list list) :
    block =
  TableB (cols, header, rows)

(* Capitalization *)

type cap_step = Done of prose | Skip | Stop

let rec capitalize_first_prose_step (prose : prose) : cap_step =
  match prose with
  | TextP "" -> Skip
  | TextP s ->
      let c = s.[0] in
      if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') then
        Done (TextP (String.capitalize_ascii s))
      else Skip
  | CodeP _ | LinkP _ | FallthroughP _ -> Stop
  | SeqP [] -> Skip
  | SeqP (prose_h :: proses_t) -> (
      match capitalize_first_prose_step prose_h with
      | Done prose_h -> Done (SeqP (prose_h :: proses_t))
      | Stop -> Stop
      | Skip -> (
          match capitalize_first_prose_step (SeqP proses_t) with
          | Done (SeqP proses_t) -> Done (SeqP (prose_h :: proses_t))
          | Done _ -> assert false
          | Skip -> Skip
          | Stop -> Stop))
  | EmptyP -> Skip

let capitalize_first_prose (prose : prose) : prose =
  match capitalize_first_prose_step prose with
  | Done prose -> prose
  | Skip | Stop -> prose

let rec capitalize_first_block_step (block : block) : block option =
  match block with
  | EmptyB | RawB _ -> None
  | InlineB prose -> (
      match capitalize_first_prose_step prose with
      | Done prose -> Some (InlineB prose)
      | Skip -> None
      | Stop -> Some block)
  | ItemB (level, item_kind, prose_head, block_body) -> (
      match capitalize_first_prose_step prose_head with
      | Done prose_head ->
          Some (ItemB (level, item_kind, prose_head, block_body))
      | Stop -> Some block
      | Skip ->
          block_body |> capitalize_first_block_step
          |> Option.map (fun block_body ->
                 ItemB (level, item_kind, prose_head, block_body)))
  | ConcatB blocks ->
      blocks |> capitalize_first_blocks_step
      |> Option.map (fun blocks -> ConcatB blocks)
  | SeqB blocks ->
      blocks |> capitalize_first_blocks_step
      |> Option.map (fun blocks -> SeqB blocks)
  | TableB _ -> Some block

and capitalize_first_blocks_step (blocks : block list) : block list option =
  match blocks with
  | [] -> None
  | block_h :: blocks_t -> (
      match capitalize_first_block_step block_h with
      | Some block_h -> Some (block_h :: blocks_t)
      | None ->
          blocks_t |> capitalize_first_blocks_step
          |> Option.map (fun blocks_t -> block_h :: blocks_t))

let capitalize_first_block (block : block) : block =
  match capitalize_first_block_step block with
  | Some block -> block
  | None -> block

(* Concatenation operators *)

let ( ++ ) (prose_a : prose) (prose_b : prose) : prose =
  SeqP [ prose_a; prose_b ]

let ( ^^ ) (code_a : code) (code_b : code) : code = SeqC [ code_a; code_b ]

(* Ordered-list markers *)

type ordered_style =
  | Arabic
  | Loweralpha
  | Lowerroman
  | Upperalpha
  | Upperroman

let style_at_level (level : int) : ordered_style =
  let cycle = [| Arabic; Loweralpha; Lowerroman; Upperalpha; Upperroman |] in
  cycle.(((level mod 5) + 5) mod 5)

let ordered_marker (level : int) (idx : int) : string =
  let to_roman ?(upper = false) (n : int) =
    let units =
      [| ""; "i"; "ii"; "iii"; "iv"; "v"; "vi"; "vii"; "viii"; "ix" |]
    in
    let tens =
      [| ""; "x"; "xx"; "xxx"; "xl"; "l"; "lx"; "lxx"; "lxxx"; "xc" |]
    in
    let n = max 1 n in
    let s = tens.(n / 10 mod 10) ^ units.(n mod 10) in
    if upper then String.uppercase_ascii s else s
  in
  let n = idx + 1 in
  match style_at_level level with
  | Arabic -> string_of_int n
  | Loweralpha when idx < 26 -> String.make 1 (Char.chr (Char.code 'a' + idx))
  | Upperalpha when idx < 26 -> String.make 1 (Char.chr (Char.code 'A' + idx))
  | Lowerroman -> to_roman n
  | Upperroman -> to_roman ~upper:true n
  | Loweralpha | Upperalpha -> F.asprintf "arm%d" n

let anchor_markers_of_block (b : block) : (string, string) Hashtbl.t =
  let markers = Hashtbl.create 16 in
  let ordinals = Hashtbl.create 8 in
  let reset_deeper level =
    Hashtbl.filter_map_inplace
      (fun level' ordinal -> if level' > level then None else Some ordinal)
      ordinals
  in
  let next_marker level =
    reset_deeper level;
    let ordinal = Option.value (Hashtbl.find_opt ordinals level) ~default:0 in
    Hashtbl.replace ordinals level (ordinal + 1);
    ordered_marker level ordinal
  in
  let rec visit_block = function
    | ItemB (level, Unordered, _, block_body) ->
        reset_deeper level;
        Hashtbl.remove ordinals level;
        visit_block block_body
    | ItemB (level, Ordered anchor_opt, _, block_body) ->
        let marker = next_marker level in
        Option.iter
          (fun anchor_item -> Hashtbl.replace markers anchor_item marker)
          anchor_opt;
        visit_block block_body
    | ConcatB blocks | SeqB blocks -> List.iter visit_block blocks
    | EmptyB | RawB _ | InlineB _ | TableB _ -> ()
  in
  visit_block b;
  markers

(* Anchor resolution *)

type anchor = subject -> string option

let anchor ~(func : string -> string option) ~(rel : string -> string option) :
    anchor = function
  | Function name -> func name
  | Relation name -> rel name

let subject_name = function Function name | Relation name -> Some name

let target_of_link anchor = function
  | Direct target -> Some target
  | Subject subject -> anchor subject

(* Serialization *)

let warned : (string, unit) Hashtbl.t = Hashtbl.create 64

let warn (msg : string) : unit =
  if not (Hashtbl.mem warned msg) then (
    Hashtbl.add warned msg ();
    Util.Error.warn Util.Source.no_region "prose" msg)

let warn_nested ~(lint : bool) ~(outer : string) ~(inner : string) : unit =
  if lint then
    warn
      (Printf.sprintf
         "nested link: cross-reference to %S is dropped inside the link to %S \
          (asciidoc cannot nest cross-references)"
         inner outer)

type code_segment = { target : string option; text : string }

let rec is_empty_code = function
  | TokenC "" | EmptyC -> true
  | LinkC (_, code_inner) -> is_empty_code code_inner
  | SeqC codes -> List.for_all is_empty_code codes
  | TokenC _ -> false

let code_segments_of_code ~anchor ~(link_ctx : string option) ~(lint : bool)
    (code : code) : code_segment list =
  let add_code_segment target text code_segments_rev =
    if text = "" then code_segments_rev
    else
      match code_segments_rev with
      | code_segment_last :: code_segments_rev'
        when target = code_segment_last.target ->
          { target; text = code_segment_last.text ^ text } :: code_segments_rev'
      | _ -> { target; text } :: code_segments_rev
  in
  let rec collect_code_segments ~target ~link_ctx code_segments_rev = function
    | TokenC text -> add_code_segment target text code_segments_rev
    | LinkC (link, code_inner) -> (
        match target_of_link anchor link with
        | None ->
            collect_code_segments ~target ~link_ctx code_segments_rev code_inner
        | Some target_inner -> (
            if lint && target_inner = "" then warn "link with empty target";
            match link_ctx with
            | Some target_outer ->
                warn_nested ~lint ~outer:target_outer ~inner:target_inner;
                collect_code_segments ~target ~link_ctx code_segments_rev
                  code_inner
            | None ->
                if lint && is_empty_code code_inner then
                  warn (Printf.sprintf "link to %S has empty body" target_inner);
                collect_code_segments ~target:(Some target_inner)
                  ~link_ctx:(Some target_inner) code_segments_rev code_inner))
    | SeqC codes ->
        List.fold_left
          (collect_code_segments ~target ~link_ctx)
          code_segments_rev codes
    | EmptyC -> code_segments_rev
  in
  collect_code_segments ~target:None ~link_ctx [] code |> List.rev

let ser_code_ ~anchor ~link_ctx ~lint ~(ser_text : string -> string)
    (code : code) =
  code
  |> code_segments_of_code ~anchor ~link_ctx ~lint
  |> List.map (fun { target; text } ->
         let text = ser_text text in
         match target with
         | None -> text
         | Some target -> adoc_link ~link:target text)
  |> String.concat ""

let rec ser_prose_ ~anchor ~(anchor_markers : (string, string) Hashtbl.t)
    ~(link_ctx : string option) ~(lint : bool) (p : prose) : string =
  match p with
  | TextP s -> s
  | CodeP code ->
      ser_code_ ~anchor ~link_ctx ~lint ~ser_text:adoc_mono_chopped code
  | LinkP (link, p) -> (
      match target_of_link anchor link with
      | None -> ser_prose_ ~anchor ~anchor_markers ~link_ctx ~lint p
      | Some target -> (
          if lint && target = "" then warn "link with empty target";
          match link_ctx with
          | Some outer ->
              warn_nested ~lint ~outer ~inner:target;
              ser_prose_ ~anchor ~anchor_markers ~link_ctx ~lint p
          | None ->
              let s =
                ser_prose_ ~anchor ~anchor_markers ~link_ctx:(Some target) ~lint
                  p
              in
              if lint && s = "" then
                warn (Printf.sprintf "link to %S has empty body" target);
              adoc_link ~link:target s))
  | FallthroughP (target, label) ->
      let text =
        match label with
        | Derived -> (
            match Hashtbl.find_opt anchor_markers target with
            | Some marker -> marker
            | None ->
                invalid_arg
                  (F.asprintf "no ordered-list marker for arm anchor %S" target)
            )
        | Explicit text -> text
      in
      F.asprintf "+++<sub class=\"bk-mark\">[<a href=\"#%s\">→ %s</a>]</sub>+++"
        target text
  | SeqP ps ->
      String.concat ""
        (List.map (ser_prose_ ~anchor ~anchor_markers ~link_ctx ~lint) ps)
  | EmptyP -> ""

let ser_prose ?(anchor = subject_name) (p : prose) : string =
  ser_prose_ ~anchor ~anchor_markers:(Hashtbl.create 0) ~link_ctx:None
    ~lint:true p

let ser_prose_in_link (p : prose) : string =
  ser_prose_
    ~anchor:(fun _ -> None)
    ~anchor_markers:(Hashtbl.create 0) ~link_ctx:(Some "") ~lint:false p

let ser_code ?(anchor = subject_name) (code : code) : string =
  ser_code_ ~anchor ~link_ctx:None ~lint:false ~ser_text:Fun.id code

let ser_block ?(anchor = subject_name) (b : block) : string =
  let anchor_markers = anchor_markers_of_block b in
  let rec ser = function
    | EmptyB -> ""
    | RawB s -> s
    | InlineB d ->
        ser_prose_ ~anchor ~anchor_markers ~link_ctx:None ~lint:true d
    | ConcatB ts -> String.concat "" (List.map ser ts)
    | SeqB ts -> String.concat "\n" (List.map ser ts)
    | ItemB (level, item_kind, prose_head, block_body) ->
        let text_bullet, text_anchor =
          match item_kind with
          | Unordered -> (adoc_unordered_bullet level, "")
          | Ordered None -> (adoc_ordered_bullet level, "")
          | Ordered (Some anchor_item) ->
              ( adoc_ordered_bullet level,
                F.asprintf
                  "+++<span class=\"bk-arm-anchor\" id=\"%s\"></span>+++"
                  anchor_item )
        in
        let text_head =
          text_bullet ^ text_anchor
          ^ ser_prose_ ~anchor ~anchor_markers ~link_ctx:None ~lint:true
              prose_head
        in
        let text_body = ser block_body in
        if text_body = "" then text_head else text_head ^ "\n" ^ text_body
    | TableB (cols, header, rows) ->
        let header_line =
          "| "
          ^ String.concat " | "
              (List.map
                 (ser_prose_ ~anchor ~anchor_markers ~link_ctx:None ~lint:true)
                 header)
          ^ " \n\n"
        in
        let row_lines =
          rows
          |> List.map (fun cells -> "| " ^ String.concat " | " cells)
          |> String.concat "\n"
        in
        Printf.sprintf "[cols=\"%d\", options=\"header\"]\n|===\n%s%s\n\n|==="
          cols header_line row_lines
  in
  ser b

(* Width: visible-text length, ignoring markup *)

let rec width_prose (p : prose) : int =
  match p with
  | TextP s -> String.length s
  | CodeP c -> width_code c
  | LinkP (_, p) -> width_prose p
  | FallthroughP (_, Derived) -> 0
  | FallthroughP (_, Explicit text) -> String.length text + 4
  | SeqP ps -> List.fold_left (fun acc p -> acc + width_prose p) 0 ps
  | EmptyP -> 0

and width_code (c : code) : int =
  match c with
  | TokenC s -> String.length s
  | LinkC (_, c) -> width_code c
  | SeqC cs -> List.fold_left (fun acc c -> acc + width_code c) 0 cs
  | EmptyC -> 0
