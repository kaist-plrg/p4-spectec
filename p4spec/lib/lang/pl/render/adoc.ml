open Utils

(* Types *)

type prose =
  | TextP of string
  | CodeP of code
  | LinkP of string * prose
  | SeqP of prose list
  | EmptyP

and code =
  | TokenC of string
  | LinkC of string * code
  | SeqC of code list
  | EmptyC

type block =
  | EmptyB
  | RawB of string
  | InlineB of prose
  | BulletB of [ `Unordered of int | `Ordered of int ]
  | ConcatB of block list
  | SeqB of block list
  | TableB of int * prose list * string list list

(* Prose constructors *)

let text_prose (s : string) : prose = TextP s
let code_prose (c : code) : prose = CodeP c
let link_prose ~(target : string) (p : prose) : prose = LinkP (target, p)
let seq_prose (ps : prose list) : prose = SeqP ps
let empty_prose : prose = EmptyP

(* Code constructors *)

let token_code (s : string) : code = TokenC s
let link_code ~(target : string) (c : code) : code = LinkC (target, c)
let seq_code (cs : code list) : code = SeqC cs
let empty_code : code = EmptyC

(* Block constructors *)

let raw_block (s : string) : block = RawB s
let inline_block (d : prose) : block = InlineB d

let bullet_block (style : [ `Unordered of int | `Ordered of int ]) : block =
  BulletB style

let concat_block (ts : block list) : block = ConcatB ts
let seq_block (ts : block list) : block = SeqB ts

let table_block ~(cols : int) ~(header : prose list) (rows : string list list) :
    block =
  TableB (cols, header, rows)

(* Capitalization *)

type cap_step = Done of prose | Skip | Stop

let rec capitalize_first_step (p : prose) : cap_step =
  match p with
  | TextP "" -> Skip
  | TextP s -> Done (TextP (String.capitalize_ascii s))
  | CodeP _ | LinkP _ -> Stop
  | SeqP [] -> Skip
  | SeqP (p0 :: ps) -> (
      match capitalize_first_step p0 with
      | Done p0' -> Done (SeqP (p0' :: ps))
      | Stop -> Stop
      | Skip -> (
          match capitalize_first_step (SeqP ps) with
          | Done (SeqP ps') -> Done (SeqP (p0 :: ps'))
          | Done _ -> assert false
          | Skip -> Skip
          | Stop -> Stop))
  | EmptyP -> Skip

let capitalize_first (p : prose) : prose =
  match capitalize_first_step p with Done p' -> p' | Skip | Stop -> p

let ( ++ ) (a : prose) (b : prose) : prose = SeqP [ a; b ]

(* Serialization *)

let warned : (string, unit) Hashtbl.t = Hashtbl.create 64

let warn (msg : string) : unit =
  if not (Hashtbl.mem warned msg) then (
    Hashtbl.add warned msg ();
    Util.Error.warn_prose Util.Source.no_region msg)

let warn_nested ~(lint : bool) ~(outer : string) ~(inner : string) : unit =
  if lint then
    warn
      (Printf.sprintf
         "nested link: cross-reference to %S is dropped inside the link to %S \
          (asciidoc cannot nest cross-references)"
         inner outer)

let rec ser_prose_ ~(link_ctx : string option) ~(lint : bool) (p : prose) :
    string =
  match p with
  | TextP s -> s
  | CodeP c ->
      let s = ser_code_ ~link_ctx ~lint c in
      if lint && s = "" then warn "code span wraps empty content";
      adoc_mono_chopped s
  | LinkP (target, p) -> (
      if lint && target = "" then warn "link with empty target";
      match link_ctx with
      | Some outer ->
          warn_nested ~lint ~outer ~inner:target;
          ser_prose_ ~link_ctx ~lint p
      | None ->
          let s = ser_prose_ ~link_ctx:(Some target) ~lint p in
          if lint && s = "" then
            warn (Printf.sprintf "link to %S has empty body" target);
          adoc_link ~link:target s)
  | SeqP ps -> String.concat "" (List.map (ser_prose_ ~link_ctx ~lint) ps)
  | EmptyP -> ""

and ser_code_ ~(link_ctx : string option) ~(lint : bool) (c : code) : string =
  match c with
  | TokenC s -> s
  | LinkC (target, c) -> (
      if lint && target = "" then warn "link with empty target";
      match link_ctx with
      | Some outer ->
          warn_nested ~lint ~outer ~inner:target;
          ser_code_ ~link_ctx ~lint c
      | None ->
          let s = ser_code_ ~link_ctx:(Some target) ~lint c in
          if lint && s = "" then
            warn (Printf.sprintf "link to %S has empty body" target);
          adoc_link ~link:target s)
  | SeqC cs -> String.concat "" (List.map (ser_code_ ~link_ctx ~lint) cs)
  | EmptyC -> ""

let ser_prose (p : prose) : string = ser_prose_ ~link_ctx:None ~lint:true p

let ser_prose_in_link (p : prose) : string =
  ser_prose_ ~link_ctx:(Some "") ~lint:false p

let ser_code (c : code) : string = ser_code_ ~link_ctx:None ~lint:false c

let rec ser_block (b : block) : string =
  match b with
  | EmptyB -> ""
  | RawB s -> s
  | InlineB d -> ser_prose d
  | ConcatB ts -> String.concat "" (List.map ser_block ts)
  | SeqB ts -> String.concat "\n" (List.map ser_block ts)
  | BulletB (`Unordered level) -> adoc_unordered_bullet level
  | BulletB (`Ordered level) -> adoc_ordered_bullet level
  | TableB (cols, header, rows) ->
      let header_line =
        "| " ^ String.concat " | " (List.map ser_prose header) ^ " \n\n"
      in
      let row_lines =
        rows
        |> List.map (fun cells -> "| " ^ String.concat " | " cells)
        |> String.concat "\n"
      in
      Printf.sprintf "[cols=\"%d\", options=\"header\"]\n|===\n%s%s\n\n|==="
        cols header_line row_lines

(* Width: visible-text length, ignoring markup (link targets, monospace delimiters) that serialization adds but never renders on screen. *)

let rec width_prose (p : prose) : int =
  match p with
  | TextP s -> String.length s
  | CodeP c -> width_code c
  | LinkP (_, p) -> width_prose p
  | SeqP ps -> List.fold_left (fun acc p -> acc + width_prose p) 0 ps
  | EmptyP -> 0

and width_code (c : code) : int =
  match c with
  | TokenC s -> String.length s
  | LinkC (_, c) -> width_code c
  | SeqC cs -> List.fold_left (fun acc c -> acc + width_code c) 0 cs
  | EmptyC -> 0
