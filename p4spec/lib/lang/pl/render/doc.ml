open Adoc

(* Document representation

   Inline content is split into two layers so the type system forbids
   monospacing prose: a monospace [Code] span may contain only [code] tokens,
   never [prose] words. [Code] is the sole bridge from [code] into [prose], and
   it is a [prose] constructor, so [Code] can neither wrap prose nor nest inside
   another [Code]. Monospacing is therefore applied exactly once, structurally,
   with no [in_code] bookkeeping. *)

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
let code (c : code) : prose = Code c
let pempty : prose = PSeq []
let ( ++ ) (a : prose) (b : prose) : prose = PSeq [ a; b ]

(* Code constructors *)

let token (s : string) : code = Token s
let cseq (cs : code list) : code = CSeq cs
let clink ~(target : string) (c : code) : code = CLink (target, c)
let cempty : code = CSeq []

(* Structural lints

   The typed representation lets the serializer report malformed documents that
   string concatenation hid: a cross-reference nested inside another (asciidoc
   renders only the outer, dropping the inner xref), an empty link target, or a
   link / code span wrapping nothing. Warnings go to stderr, de-duplicated.
   (Monospacing prose and nested code are now unrepresentable, so they need no
   runtime check.) *)

let warned : (string, unit) Hashtbl.t = Hashtbl.create 64

let warn (msg : string) : unit =
  if not (Hashtbl.mem warned msg) then (
    Hashtbl.add warned msg ();
    Format.eprintf "Warning [pl/render]: %s\n%!" msg)

let warn_nested ~(lint : bool) ~(outer : string) ~(inner : string) : unit =
  if lint then
    warn
      (Printf.sprintf
         "nested link: cross-reference to %S is dropped inside the link to %S \
          (asciidoc cannot nest cross-references)"
         inner outer)

(* [link_ctx] is the target of the innermost open link, or [None] at top level.
   [lint] is off for the deliberate raw / links-off serializers. *)

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
