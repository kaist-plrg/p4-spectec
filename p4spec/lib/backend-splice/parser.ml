open Error
open Util.Source

(* Parsing string with expects *)

let rec parse_string' (s : string) (i : int) (s_expect : string) (j : int) :
    bool =
  j = String.length s_expect
  || (s.[i] = s_expect.[j] && parse_string' s (i + 1) s_expect (j + 1))

let parse_string (source : Source.t) (s : string) : bool =
  Source.left source >= String.length s
  && parse_string' source.s source.i s 0
  &&
  (Source.advn source (String.length s);
   true)

(* Whitespace parsing *)

let rec parse_space (source : Source.t) : unit =
  if
    (not (Source.eos source))
    && (Source.get source = ' '
       || Source.get source = '\t'
       || Source.get source = '\n')
  then (
    Source.adv source;
    parse_space source)

(* Splice anchor parsing *)

let parse_splice_start (source : Source.t) (name : string) : bool =
  parse_string source ("${" ^ name ^ ":")

(* Identifier parsing *)

let rec parse_id' (source : Source.t) : unit =
  if not (Source.eos source) then
    match Source.get source with
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' | '`' | '-' | '*' | '.'
      ->
        Source.adv source;
        parse_id' source
    | _ -> ()

let parse_id_ (source : Source.t) : string =
  let i_prev = source.i in
  parse_id' source;
  if i_prev = source.i then error no_region "cannot parse identifier";
  Source.str source i_prev

let parse_id_with_sub_ (source : Source.t) : string * string =
  let id = parse_id_ source in
  let id_sub = if parse_string source "/" then parse_id_ source else "" in
  (id, id_sub)

(* Entry point *)

let rec parse_ids (source : Source.t) : string list =
  parse_space source;
  if parse_string source "}" then []
  else
    let id = parse_id_ source in
    id :: parse_ids source

let parse_id_with_sub (source : Source.t) : string * string =
  parse_space source;
  let id_with_sub = parse_id_with_sub_ source in
  parse_space source;
  let _ = parse_string source "}" in
  id_with_sub
