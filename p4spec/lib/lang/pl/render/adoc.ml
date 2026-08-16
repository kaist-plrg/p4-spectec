open Utils

(* Types *)

type prose =
  | TextP of string
  | CodeP of code
  | LinkP of link * prose
  | SeqP of prose list
  | EmptyP

and code =
  | TokenC of string
  | LinkC of link * code
  | SeqC of code list
  | EmptyC

and link = Direct of string | Subject of subject
and subject = Function of string | Relation of string

type block =
  | EmptyB
  | RawB of string
  | InlineB of prose
  | BulletB of [ `Unordered of int | `Ordered of int ]
  | ConcatB of block list
  | SeqB of block list
  | TableB of int * prose list * string list list

(* Prose constructors *)

let text (s : string) : prose = TextP s
let code_prose (c : code) : prose = CodeP c
let link_prose ~(target : string) (p : prose) : prose = LinkP (Direct target, p)

let link_subject_prose (subject : subject) (p : prose) : prose =
  LinkP (Subject subject, p)

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

let bullet_block (style : [ `Unordered of int | `Ordered of int ]) : block =
  BulletB style

let concat_block (ts : block list) : block = ConcatB ts
let seq_block (ts : block list) : block = SeqB ts

let bullet_inline_block (style : [ `Unordered of int | `Ordered of int ])
    (p : prose) : block =
  concat_block [ bullet_block style; inline_block p ]

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
  | CodeP _ | LinkP _ -> Stop
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
  | EmptyB | RawB _ | BulletB _ -> None
  | InlineB prose -> (
      match capitalize_first_prose_step prose with
      | Done prose -> Some (InlineB prose)
      | Skip -> None
      | Stop -> Some block)
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

(* Serialization *)

type anchor = subject -> string option

let anchor ~(func : string -> string option) ~(rel : string -> string option) :
    anchor = function
  | Function name -> func name
  | Relation name -> rel name

let subject_name = function Function name | Relation name -> Some name
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

let target_of_link anchor = function
  | Direct target -> Some target
  | Subject subject -> anchor subject

let rec ser_prose_ ~anchor ~(link_ctx : string option) ~(lint : bool)
    (p : prose) : string =
  match p with
  | TextP s -> s
  | CodeP c ->
      let s = ser_code_ ~anchor ~link_ctx ~lint c in
      if lint && s = "" then warn "code span wraps empty content";
      adoc_mono_chopped s
  | LinkP (link, p) -> (
      match target_of_link anchor link with
      | None -> ser_prose_ ~anchor ~link_ctx ~lint p
      | Some target -> (
          if lint && target = "" then warn "link with empty target";
          match link_ctx with
          | Some outer ->
              warn_nested ~lint ~outer ~inner:target;
              ser_prose_ ~anchor ~link_ctx ~lint p
          | None ->
              let s = ser_prose_ ~anchor ~link_ctx:(Some target) ~lint p in
              if lint && s = "" then
                warn (Printf.sprintf "link to %S has empty body" target);
              adoc_link ~link:target s))
  | SeqP ps ->
      String.concat "" (List.map (ser_prose_ ~anchor ~link_ctx ~lint) ps)
  | EmptyP -> ""

and ser_code_ ~anchor ~(link_ctx : string option) ~(lint : bool) (c : code) :
    string =
  match c with
  | TokenC s -> s
  | LinkC (link, c) -> (
      match target_of_link anchor link with
      | None -> ser_code_ ~anchor ~link_ctx ~lint c
      | Some target -> (
          if lint && target = "" then warn "link with empty target";
          match link_ctx with
          | Some outer ->
              warn_nested ~lint ~outer ~inner:target;
              ser_code_ ~anchor ~link_ctx ~lint c
          | None ->
              let s = ser_code_ ~anchor ~link_ctx:(Some target) ~lint c in
              if lint && s = "" then
                warn (Printf.sprintf "link to %S has empty body" target);
              adoc_link ~link:target s))
  | SeqC cs ->
      String.concat "" (List.map (ser_code_ ~anchor ~link_ctx ~lint) cs)
  | EmptyC -> ""

let ser_prose ?(anchor = subject_name) (p : prose) : string =
  ser_prose_ ~anchor ~link_ctx:None ~lint:true p

let ser_prose_in_link (p : prose) : string =
  ser_prose_ ~anchor:(fun _ -> None) ~link_ctx:(Some "") ~lint:false p

let ser_code ?(anchor = subject_name) (c : code) : string =
  ser_code_ ~anchor ~link_ctx:None ~lint:false c

let rec ser_block ?(anchor = subject_name) (b : block) : string =
  match b with
  | EmptyB -> ""
  | RawB s -> s
  | InlineB d -> ser_prose ~anchor d
  | ConcatB ts -> String.concat "" (List.map (ser_block ~anchor) ts)
  | SeqB ts -> String.concat "\n" (List.map (ser_block ~anchor) ts)
  | BulletB (`Unordered level) -> adoc_unordered_bullet level
  | BulletB (`Ordered level) -> adoc_ordered_bullet level
  | TableB (cols, header, rows) ->
      let header_line =
        "| "
        ^ String.concat " | " (List.map (ser_prose ~anchor) header)
        ^ " \n\n"
      in
      let row_lines =
        rows
        |> List.map (fun cells -> "| " ^ String.concat " | " cells)
        |> String.concat "\n"
      in
      Printf.sprintf "[cols=\"%d\", options=\"header\"]\n|===\n%s%s\n\n|==="
        cols header_line row_lines

(* Width: visible-text length, ignoring markup *)

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
