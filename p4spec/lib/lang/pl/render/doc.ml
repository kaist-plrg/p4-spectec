open Adoc

(* Document representation *)

type t =
  | Text of string
  | Seq of t list
  | Code of t
  | Link of string * t (* target, body *)

let text (s : string) : t = Text s
let seq (ts : t list) : t = Seq ts
let code (t : t) : t = Code t
let link ~(target : string) (t : t) : t = Link (target, t)
let empty : t = Seq []
let ( ++ ) (a : t) (b : t) : t = Seq [ a; b ]

(* Structural lints

   Because wrapping is now structural (rather than hand-threaded strings), the
   serializer can see -- and report -- malformed inline documents that string
   concatenation silently swallowed: a cross-reference nested inside another one
   (asciidoc renders only the outer, so the inner xref is lost), an empty link
   target, or a link/code span wrapping nothing. Warnings go to stderr and are
   de-duplicated so each distinct issue is reported once per run. *)

let warned : (string, unit) Hashtbl.t = Hashtbl.create 64

let warn (msg : string) : unit =
  if not (Hashtbl.mem warned msg) then (
    Hashtbl.add warned msg ();
    Format.eprintf "Warning [pl/render]: %s\n%!" msg)

(* [link_ctx] is the target of the innermost open link, or [None] at top level.
   [lint] is off for the deliberate raw / links-off internal serializers. *)

let rec serialize ~(in_code : bool) ~(link_ctx : string option) ~(lint : bool)
    (t : t) : string =
  match t with
  | Text s -> s
  | Seq ts -> String.concat "" (List.map (serialize ~in_code ~link_ctx ~lint) ts)
  | Code inner ->
      let s = serialize ~in_code:true ~link_ctx ~lint inner in
      if lint && s = "" then warn "code span wraps empty content";
      if in_code then s else adoc_mono_chopped s
  | Link (target, inner) -> (
      if lint && target = "" then warn "link with empty target";
      match link_ctx with
      | Some outer ->
          if lint then
            warn
              (Printf.sprintf
                 "nested link: cross-reference to %S is dropped inside the link \
                  to %S (asciidoc cannot nest cross-references)"
                 target outer);
          (* inner link is suppressed: keep the outer link context *)
          serialize ~in_code ~link_ctx ~lint inner
      | None ->
          let s = serialize ~in_code ~link_ctx:(Some target) ~lint inner in
          if lint && s = "" then
            warn (Printf.sprintf "link to %S has empty body" target);
          adoc_link ~link:target s)

(* Serialize at the top level: neither a code span nor a link is open *)

let to_adoc (t : t) : string =
  serialize ~in_code:false ~link_ctx:None ~lint:true t

(* Serialize as if already inside a code span *)

let to_adoc_code (t : t) : string =
  serialize ~in_code:true ~link_ctx:None ~lint:false t

(* Serialize as if already inside a link *)

let to_adoc_in_link (t : t) : string =
  serialize ~in_code:false ~link_ctx:(Some "") ~lint:false t
