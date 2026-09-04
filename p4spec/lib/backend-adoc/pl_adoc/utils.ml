(* Asciidoc utils *)

let adoc_width_short = 30

let rec adoc_escape (c : char) (text : string) =
  match String.index_opt text c with
  | None -> text
  | Some idx ->
      let text_before = String.sub text 0 idx in
      let text_after =
        String.sub text (idx + 1) (String.length text - idx - 1)
      in
      text_before ^ "+" ^ String.make 1 c ^ "+" ^ adoc_escape c text_after

let adoc_subscript (s : string) = "~" ^ s ^ "~"
let adoc_superscript (s : string) = "^" ^ s ^ "^"
let adoc_mono (s : string) = "``" ^ s ^ "``"

let adoc_escape_quotes (text : string) =
  text |> String.split_on_char '"' |> String.concat "{quot}"

let adoc_escape_ambiguous_quotes (text : string) =
  let quote_count =
    text |> String.to_seq
    |> Seq.fold_left (fun count c -> if c = '"' then count + 1 else count) 0
  in
  if quote_count > 2 then adoc_escape_quotes text else text

let adoc_mono_chopped (s : string) =
  s |> adoc_escape_ambiguous_quotes |> String.split_on_char ' '
  |> List.map (function "" -> "" | text -> adoc_mono text)
  |> String.concat " "

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

(* Indentation *)

let reindent_lines ?(level = 0) (s : string) : string =
  let lines = String.split_on_char '\n' s in
  String.concat ("\n" ^ adoc_unordered_bullet level) lines

let unindent_lines (s : string) : string =
  s |> String.split_on_char '\n' |> String.concat ""
