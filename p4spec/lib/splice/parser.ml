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

let parse_id (source : Source.t) : string =
  let i_prev = source.i in
  parse_id' source;
  if i_prev = source.i then error no_region "cannot parse identifier";
  Source.str source i_prev

let parse_id_with_sub (source : Source.t) : Kinds.RuleGroupId.t =
  let id = parse_id source in
  let id_sub = if parse_string source "/" then parse_id source else "" in
  (id, id_sub)

let rec parse_syntax_ids (source : Source.t) : Kinds.SyntaxId.t list =
  parse_space source;
  if parse_string source "}" then []
  else
    let id = parse_id source in
    id :: parse_syntax_ids source

let rec parse_relation_ids (source : Source.t) : Kinds.RelationId.t list =
  parse_space source;
  if parse_string source "}" then []
  else
    let id = parse_id source in
    id :: parse_relation_ids source

let parse_rulegroup_id (source : Source.t) : Kinds.RuleGroupId.t =
  parse_space source;
  let id_rulegroup = parse_id_with_sub source in
  parse_space source;
  let _ = parse_string source "}" in
  id_rulegroup

let parse_ruleprose_id (source : Source.t) : Kinds.RuleProseId.t =
  parse_rulegroup_id source

let parse_funcprose_id (source : Source.t) : Kinds.FuncProseId.t =
  parse_space source;
  let id_funcprose = parse_id source in
  parse_space source;
  let _ = parse_string source "}" in
  id_funcprose
