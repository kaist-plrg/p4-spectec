module F = Format

(* Asciidoc utils *)

let rec adoc_escape (c : char) (text : string) =
  match String.index_opt text c with
  | None -> text
  | Some idx ->
      let text_before = String.sub text 0 idx in
      let text_after =
        String.sub text (idx + 1) (String.length text - idx - 1)
      in
      text_before ^ "+" ^ String.make 1 c ^ "+" ^ adoc_escape c text_after

let adoc_width_short = 30
let adoc_fits_in_width_short (s : string) = String.length s <= adoc_width_short
let adoc_subscript (s : string) = "~" ^ s ^ "~"
let adoc_superscript (s : string) = "^" ^ s ^ "^"
let adoc_bold (s : string) = "*" ^ s ^ "*"
let adoc_mono (s : string) = "``" ^ s ^ "``"

let adoc_mono_chopped (s : string) =
  s |> String.split_on_char ' ' |> List.map adoc_mono |> String.concat " "

let adoc_ordered_bullet (level : int) =
  Format.asprintf "%s%s " (String.make level ' ') (String.make (level + 1) '.')

let adoc_unordered_bullet (level : int) =
  Format.asprintf "%s%s " (String.make level ' ') (String.make (level + 1) '*')

let adoc_link ~(link : string) (text : string) : string =
  let brackets = String.contains text '[' || String.contains text ']' in
  let angles = String.contains text '<' || String.contains text '>' in
  match (brackets, angles) with
  | false, false | false, true -> "xref:" ^ link ^ "[" ^ text ^ "]"
  | true, false -> "<<" ^ link ^ "," ^ text ^ ">>"
  | true, true ->
      Format.eprintf
        "Warning: Asciidoc link text contains both brackets and angle \
         brackets. Link may not render correctly.\n\
         \t%s\n"
        text;
      text

let adoc_attach_block = "+\n"
let adoc_open_block (s : string) = F.asprintf "--\n%s\n--" s

(* Inline document

   A structured representation of inline asciidoc content, serialized once by
   [to_adoc]. A [Code]/[Link] wrapper is emitted only when one of the same kind
   is not already open, so nested wrappers collapse structurally -- this
   replaces the old [in_code]/[in_link] suppression booleans. *)

module Inline = struct
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

  let rec serialize ~(in_code : bool) ~(in_link : bool) (t : t) : string =
    match t with
    | Text s -> s
    | Seq ts -> String.concat "" (List.map (serialize ~in_code ~in_link) ts)
    | Code inner ->
        let s = serialize ~in_code:true ~in_link inner in
        if in_code then s else adoc_mono_chopped s
    | Link (target, inner) ->
        let s = serialize ~in_code ~in_link:true inner in
        if in_link then s else adoc_link ~link:target s

  (* Serialize at the top level: neither a code span nor a link is open. *)
  let to_adoc (t : t) : string = serialize ~in_code:false ~in_link:false t

  (* Serialize as if already inside a code span (no extra mono-wrapping of the
     whole, leaves stay raw) -- the structural equivalent of the old
     [render_exp in_code] used to build raw fragments for string assembly. *)
  let to_adoc_code (t : t) : string = serialize ~in_code:true ~in_link:false t

  (* Serialize as if already inside a link (nested links suppressed) -- the
     structural equivalent of the old [render_exp in_link] used inside hint
     alternations whose whole result is then wrapped in one outer link. *)
  let to_adoc_in_link (t : t) : string = serialize ~in_code:false ~in_link:true t
end

let reindent_lines ?(level = 0) (s : string) : string =
  let lines = String.split_on_char '\n' s in
  String.concat ("\n" ^ adoc_unordered_bullet level) lines

let unindent_lines (s : string) : string =
  s |> String.split_on_char '\n' |> String.concat ""
