open Utils

(* [prose]/[code] split in two so the type system forbids monospacing prose: [Code] is the only bridge into [code] and can't nest or wrap prose. *)

type prose =
  | Text of string (* prose words -- never monospaced *)
  | PSeq of prose list
  | PLink of string * prose (* cross-reference wrapping prose *)
  | Code of code (* a monospace span -- the only bridge into [code] *)

and code =
  | Token of string (* a code token / run *)
  | CSeq of code list
  | CLink of string * code (* cross-reference to a definition, inside code *)

(* Prose constructors *)

let text (s : string) : prose = Text s
let pseq (ps : prose list) : prose = PSeq ps
let plink ~(target : string) (p : prose) : prose = PLink (target, p)

(* Capitalizes the first letter of [p]'s own text, skipping past empty [Text]
   runs; stops (a no-op) at the first [PLink]/[Code], since that content isn't
   ours to alter and never starts with a lowercase letter to begin with. *)
type cap_step = Done of prose | Skip | Stop

let rec capitalize_first_step (p : prose) : cap_step =
  match p with
  | Text "" -> Skip
  | Text s -> Done (Text (String.capitalize_ascii s))
  | PSeq [] -> Skip
  | PSeq (p0 :: ps) -> (
      match capitalize_first_step p0 with
      | Done p0' -> Done (PSeq (p0' :: ps))
      | Stop -> Stop
      | Skip -> (
          match capitalize_first_step (PSeq ps) with
          | Done (PSeq ps') -> Done (PSeq (p0 :: ps'))
          | Done _ -> assert false
          | Skip -> Skip
          | Stop -> Stop))
  | PLink _ | Code _ -> Stop

let capitalize_first (p : prose) : prose =
  match capitalize_first_step p with Done p' -> p' | Skip | Stop -> p

let code (c : code) : prose = Code c
let pempty : prose = PSeq []
let ( ++ ) (a : prose) (b : prose) : prose = PSeq [ a; b ]

(* Code constructors *)

let token (s : string) : code = Token s
let cseq (cs : code list) : code = CSeq cs
let clink ~(target : string) (c : code) : code = CLink (target, c)
let cempty : code = CSeq []

(* Structural lints: warn (stderr, de-duplicated) on malformed docs that string concat used to hide -- nested cross-references, empty link targets, empty link/code bodies. *)

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

(* [link_ctx] is the innermost open link's target ([None] at top level); [lint] is off for the deliberate raw / links-off serializers. *)

let rec ser_prose ~(link_ctx : string option) ~(lint : bool) (p : prose) :
    string =
  match p with
  | Text s -> s
  | PSeq ps -> String.concat "" (List.map (ser_prose ~link_ctx ~lint) ps)
  | Code c ->
      let s = ser_code ~link_ctx ~lint c in
      if lint && s = "" then warn "code span wraps empty content";
      adoc_mono_chopped s
  | PLink (target, p) -> (
      if lint && target = "" then warn "link with empty target";
      match link_ctx with
      | Some outer ->
          warn_nested ~lint ~outer ~inner:target;
          ser_prose ~link_ctx ~lint p
      | None ->
          let s = ser_prose ~link_ctx:(Some target) ~lint p in
          if lint && s = "" then
            warn (Printf.sprintf "link to %S has empty body" target);
          adoc_link ~link:target s)

and ser_code ~(link_ctx : string option) ~(lint : bool) (c : code) : string =
  match c with
  | Token s -> s
  | CSeq cs -> String.concat "" (List.map (ser_code ~link_ctx ~lint) cs)
  | CLink (target, c) -> (
      if lint && target = "" then warn "link with empty target";
      match link_ctx with
      | Some outer ->
          warn_nested ~lint ~outer ~inner:target;
          ser_code ~link_ctx ~lint c
      | None ->
          let s = ser_code ~link_ctx:(Some target) ~lint c in
          if lint && s = "" then
            warn (Printf.sprintf "link to %S has empty body" target);
          adoc_link ~link:target s)

(* Serialize a prose document at the top level *)

let to_adoc (p : prose) : string = ser_prose ~link_ctx:None ~lint:true p

(* Serialize a code fragment as raw tokens (no enclosing monospace span) *)

let to_adoc_code (c : code) : string = ser_code ~link_ctx:None ~lint:false c

(* Serialize prose with cross-references suppressed and no surrounding link *)

let to_adoc_in_link (p : prose) : string =
  ser_prose ~link_ctx:(Some "") ~lint:false p

(* Block documents: [prose] sentences joined by literal scaffolding (bullets, "\n", tables); [Raw] carries literal markup, [Inline] carries rendered content. *)

type block =
  | Inline of prose (* one inline sentence *)
  | Raw of
      string (* literal scaffolding: bullets, "\n", "--", "|===", anchors *)
  | Concat of block list (* children concatenated with no separator *)
  | Vseq of block list (* children joined by "\n" *)
  | Empty

let inline (d : prose) : block = Inline d
let raw (s : string) : block = Raw s
let concat (ts : block list) : block = Concat ts
let vseq (ts : block list) : block = Vseq ts
let empty : block = Empty

(* Sole owner of the ordered-list bullet format, so instruction renderers stop threading a pre-formatted bullet string. *)
let bullet (level : int) : block = Raw (adoc_ordered_bullet level)

(* Owns the "[cols=...]|===...|===" envelope; [rows] are pre-serialized (table cells are raw code by spec, see PROSE.md). *)
let table ~(cols : int) ~(header : prose list) (rows : string list list) : block
    =
  let header_line =
    "| " ^ String.concat " | " (List.map to_adoc header) ^ " \n\n"
  in
  let row_lines =
    rows
    |> List.map (fun cells -> "| " ^ String.concat " | " cells)
    |> String.concat "\n"
  in
  Raw
    (Printf.sprintf "[cols=\"%d\", options=\"header\"]\n|===\n%s%s\n\n|===" cols
       header_line row_lines)

let rec serialize (b : block) : string =
  match b with
  | Empty -> ""
  | Inline d -> to_adoc d
  | Raw s -> s
  | Concat ts -> String.concat "" (List.map serialize ts)
  | Vseq ts -> String.concat "\n" (List.map serialize ts)
