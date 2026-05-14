(* Copyright 2018-present Cornell University
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy
 * of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License.
*)

{
open Lexing
open Context
open Parser
module Value = Runtime.Value
open Value.Make
module F = Format
open Util.Source

exception Error of string

type lexer_state =
  (* Nothing to recall from the previous tokens *)
  | SRegular
  | SRangle of Lexing.position
  | SPragma
  (* We have seen a template *)
  | STemplate
  (* We have seen an identifier:
   * we have just emitted a [NAME] token.
   * The next token will be either [IDENTIFIER] or [TYPENAME],
   * depending on what kind of identifier this is *)
  | SIdent of string * lexer_state
let lexer_state = ref SRegular
    
let reset () =
  Context.reset ();
  lexer_state := SRegular

let set_line lexbuf line =
  let position = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { position with pos_lnum = line }

let set_start_of_line lexbuf bol =
  let position = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { position with pos_bol = bol }

let position_to_pos position =
  {
    file = position.pos_fname;
    line = position.pos_lnum;
    column = position.pos_cnum - position.pos_bol
  }

let positions_to_region position_left position_right =
  {
    left = position_to_pos position_left;
    right = position_to_pos position_right
  }

let at lexbuf =
  positions_to_region (lexeme_start_p lexbuf) (lexeme_end_p lexbuf)

let follows_position position_left position_right : bool =
  position_left.pos_fname = position_right.pos_fname
  && position_left.pos_lnum = position_right.pos_lnum
  && position_left.pos_cnum = position_right.pos_cnum

let sanitize s =
  String.concat "" (String.split_on_char '_' s)

let strip_prefix s =
  let length = String.length s in
  assert (length > 2);
  String.sub s 2 (length - 2)

let parse_int at n =
  let i = Bigint.of_string (sanitize n) in
  Value.Make.int ~at i

let parse_width_int at s n =
  let l_s = String.length s in
  let width = String.sub s 0 (l_s - 1) in
  let sign = String.sub s (l_s - 1) 1 in
  let i = Bigint.of_string (sanitize n) in
  let w = Bigint.of_string width in
  match sign with
  | "s" ->
    if (int_of_string width < 2)
    then raise (Error "signed integers must have width at least 2")
    else 
      let value_width = Value.Make.nat ~at w in
      let value_int = Value.Make.int ~at i in
      "nat S int" <| [ value_width; value_int ] <<| "integerLiteral" <<<| at
  | "w" ->
    let value_width = Value.Make.nat ~at w in
    let value_int = Value.Make.int ~at i in
    "nat W int" <| [ value_width; value_int ] <<| "integerLiteral" <<<| at
  | _ ->
    raise (Error "Illegal integer constant")
}

let name = [ 'A'-'Z' 'a'-'z' '_' ] [ 'A'-'Z' 'a'-'z' '0'-'9' '_' ]*
let hex_number = '0' [ 'x' 'X' ] [ '0'-'9' 'a'-'f' 'A'-'F' '_' ]+
let dec_number = '0' [ 'd' 'D' ] [ '0'-'9' '_' ]+
let oct_number = '0' [ 'o' 'O' ] [ '0'-'7' '_' ]+
let bin_number = '0' [ 'b' 'B' ] [ '0' '1' '_' ]+
let int = [ '0'-'9' ] [ '0'-'9' '_' ]*
let sign = [ '0'-'9' ]+ [ 'w' 's' ]

let whitespace = [ ' ' '\t' '\012' '\r' ]

rule tokenize = parse
  | "/*"
      { match multiline_comment None lexbuf with 
        | None -> tokenize lexbuf
        | Some _info -> PRAGMA_END }
  | "//"
      { singleline_comment lexbuf; tokenize lexbuf }
  | '\n'
      { Lexing.new_line lexbuf; PRAGMA_END }
  | '"'
      { let start_region = at lexbuf in
        let str, end_region = (string lexbuf) in
        let token_region = over_region [ start_region; end_region ] in
        let value = Value.Make.text ~at:token_region str in
        STRING_LITERAL value
      }
  | whitespace
      { tokenize lexbuf }
  | '#'
      { preprocessor lexbuf ; tokenize lexbuf }
  | "@pragma"
      { PRAGMA }
  | hex_number as n
      { NUMBER_INT (parse_int (at lexbuf) n, n) }
  | dec_number as n
      { NUMBER_INT (parse_int (at lexbuf) (strip_prefix n), n) }
  | oct_number as n
      { NUMBER_INT (parse_int (at lexbuf) n, n) }
  | bin_number as n
      { NUMBER_INT (parse_int (at lexbuf) n, n) }
  | int as n
      { NUMBER_INT (parse_int (at lexbuf) n, n) }
  | (sign as s) (hex_number as n)
      { NUMBER (parse_width_int (at lexbuf) s n, n) }
  | (sign as s) (dec_number as n)
      { NUMBER (parse_width_int (at lexbuf) s (strip_prefix n), n) }
  | (sign as s) (oct_number as n)
      { NUMBER (parse_width_int (at lexbuf) s n, n) }
  | (sign as s) (bin_number as n)
      { NUMBER (parse_width_int (at lexbuf) s n, n) }
  | (sign as s) (int as n)
      { NUMBER (parse_width_int (at lexbuf) s n, n) }
  | "abstract"
      { ABSTRACT }
  | "action"
      { ACTION }
  | "actions"
      { ACTIONS }
  | "apply"
      { APPLY }
  | "bool"
      { BOOL }
  | "bit"
      { BIT }
  | "break"
      { BREAK }
  | "const"
      { CONST }
  | "continue"
      { CONTINUE }
  | "control"
      { CONTROL }
  | "default"
      { DEFAULT }
  | "else"
      { ELSE }
  | "entries"
      { ENTRIES }
  | "enum"
      { ENUM }
  | "error"
      { ERROR }
  | "exit"
      { EXIT }
  | "extern"
      { EXTERN }
  | "header"
      { HEADER }
  | "header_union"
      { HEADER_UNION }
  | "true"
      { TRUE }
  | "false"
      { FALSE }
  | "for"
      { FOR }
  | "if"
      { IF }
  | "in"
      { IN }
  | "inout"
      { INOUT }
  | "int"
      { INT }
  | "key"
      { KEY }
  | "list"
      { LIST }
  | "match_kind"
      { MATCH_KIND }
  | "out"
      { OUT }
  | "parser"
      { PARSER }
  | "package"
      { PACKAGE }
  | "pragma" 
      { PRAGMA }
  | "priority"
      { PRIORITY }
  | "return"
      { RETURN }
  | "select"
      { SELECT }
  | "state"
      { STATE }
  | "string"
      { STRING }
  | "struct"
      { STRUCT }
  | "switch"
      { SWITCH }
  | "table"
      { TABLE }
  | "this"
      { THIS }  
  | "transition"
      { TRANSITION }
  | "tuple"
      { TUPLE }
  | "typedef"
      { TYPEDEF }
  | "type"
      { TYPE }
  | "value_set"
      { VALUE_SET }
  | "varbit"
      { VARBIT }
  | "void"
      { VOID }
  | "_"
      { DONTCARE }
  | name
      { let text = Lexing.lexeme lexbuf in
        let value = Value.Make.text ~at:(at lexbuf) text in
        NAME value }
  | "<="
      { LE }
  | ">="
      { GE }
  | "<<"
      { SHL }
  | "&&"
      { AND }
  | "||"
      { OR }
  | "!="
      { NE }
  | "=="
      { EQ }
  | "+:"
      { PLUSCOLON }
  | "+"
      { PLUS }
  | "-"
      { MINUS }
  | "|+|"
      { PLUS_SAT }
  | "|-|"
      { MINUS_SAT }
  | "*"
      { MUL }
  | "{#}"
      { INVALID }
  | "/"
      { DIV }
  | "%"
      { MOD }
  | "|"
      { BIT_OR }
  | "&"
      { BIT_AND }
  | "^"
      { BIT_XOR }
  | "~"
      { COMPLEMENT }
  | "["
      { L_BRACKET }
  | "]"
      { R_BRACKET }
  | "{"
      { L_BRACE }
  | "}"
      { R_BRACE }
  | "<"
      { L_ANGLE }
  | ">"
      { R_ANGLE }
  | "("
      { L_PAREN }
  | ")"
      { R_PAREN }
  | "!"
      { NOT }
  | ":"
      { COLON }
  | ","
      { COMMA }
  | "?"
      { QUESTION }
  | "."
      { DOT }
  | "="
      { ASSIGN }
  | ";"
      { SEMICOLON }
  | "@"
      { AT }
  | "++"
      { PLUSPLUS }
  | "&&&"
      { MASK }
  | "..."
      { DOTS }
  | ".."
      { RANGE }
  | "+="
      { PLUS_ASSIGN }
  | "|+|="
      { PLUS_SAT_ASSIGN }
  | "-="
      { MINUS_ASSIGN }
  | "|-|="
      { MINUS_SAT_ASSIGN }
  | "*="
      { MUL_ASSIGN }
  | "/="
      { DIV_ASSIGN } 
  | "%="
      { MOD_ASSIGN }
  | "<<="
      { SHL_ASSIGN }
  | ">>="
      { SHR_ASSIGN }
  | "&="
      { BIT_AND_ASSIGN }
  | "^="
      { BIT_XOR_ASSIGN }
  | "|="
      { BIT_OR_ASSIGN }
  | eof
      { END }
  | _
      { let text = lexeme lexbuf in
        let value = Value.Make.text ~at:(at lexbuf) text in
        UNEXPECTED_TOKEN value }
      
and string = parse
  | eof
      { raise (Error "File ended while reading a string literal") }
  | "\\\""
      { let rest, end_region = (string lexbuf) in
        ("\"" ^ rest, end_region) }
  | '\\' 'n'
      { let rest, end_region = (string lexbuf) in
        ("\n" ^ rest, end_region) }
  | '\\' '\\'
      { let rest, end_region = (string lexbuf) in
        ("\\" ^ rest, end_region) }
  | '\\' _ as c
      { raise (Error ("Escape sequences not yet supported: \\" ^ c)) }
  | '"'
      { ("", at lexbuf) }
  | _ as chr
      { let rest, end_region = (string lexbuf) in
        ((String.make 1 chr) ^ rest, end_region) }
    
(* Preprocessor annotations indicate line and path *)
and preprocessor = parse
  | ' '
      { preprocessor lexbuf }
  | int
      { let line = int_of_string (lexeme lexbuf) in
        set_line lexbuf line; preprocessor lexbuf }
  | '"'
      { preprocessor_string lexbuf }
  | '\n'
      { set_start_of_line lexbuf (lexeme_end lexbuf) }
  | _
      { preprocessor lexbuf }
      
and preprocessor_string = parse
  | [^ '"'] * '"'
    { let path = lexeme lexbuf in 
      let path = String.sub path 0 (String.length path - 1) in
      Lexing.set_filename lexbuf path;
      preprocessor_column lexbuf }
      
(* Once a path has been recognized, ignore the rest of the line *)
and preprocessor_column = parse
  | ' ' 
      { preprocessor_column lexbuf }
  | '\n'
      { set_start_of_line lexbuf (lexeme_end lexbuf) }
  | eof
      { () }
  | _
      { preprocessor_column lexbuf }
      
(* Multi-line comment terminated by "*/" *)
and multiline_comment opt = parse
  | "*/"   { opt }
  | eof    { failwith "unterminated comment" }
  | '\n'   { Lexing.new_line lexbuf; multiline_comment (Some(at lexbuf)) lexbuf }
  | _      { multiline_comment opt lexbuf }
      
(* Single-line comment terminated by a newline *)
and singleline_comment = parse
  | '\n'   { Lexing.new_line lexbuf }
  | eof    { () }
  | _      { singleline_comment lexbuf }
      
{
let rec lexer (lexbuf:lexbuf): token = 
   match !lexer_state with
    | SIdent(id, next) ->
      begin match get_kind id with
      | TypeName true ->
        lexer_state := STemplate;
        TYPENAME
      | Ident true ->
        lexer_state := STemplate;
        IDENTIFIER
      | TypeName false ->
        lexer_state := next;
        TYPENAME
      | Ident false ->
        lexer_state := next;
        IDENTIFIER
      end
    | SRangle endp1 -> 
      begin match tokenize lexbuf with
      | R_ANGLE when follows_position endp1 lexbuf.lex_start_p -> 
        lexer_state := SRegular;
        R_ANGLE_SHIFT
      | PRAGMA as token ->
        lexer_state := SPragma;
        token
      | PRAGMA_END -> 
        lexer_state := SRegular;
        lexer lexbuf
      | NAME value as token ->
        let text = Value.Get.text value in
        lexer_state := SIdent (text, SRegular);
        token          
      | token -> 
        lexer_state := SRegular;
        token
      end
    | SRegular ->
      begin match tokenize lexbuf with
      | NAME value as token ->
        let text = Value.Get.text value in
        lexer_state := SIdent (text, SRegular);
        token
      | PRAGMA as token ->
        lexer_state := SPragma;
        token
      | PRAGMA_END ->
        lexer lexbuf
      | R_ANGLE as token -> 
        lexer_state := SRangle lexbuf.lex_curr_p;
        token
      | token ->
        lexer_state := SRegular;
        token
      end
    | STemplate ->
      begin match tokenize lexbuf with
      | L_ANGLE -> L_ANGLE_ARGS
      | NAME value as token ->
        let text = Value.Get.text value in
        lexer_state := SIdent (text, SRegular);
        token
      | PRAGMA as token ->
        lexer_state := SPragma;
        token
      | PRAGMA_END -> lexer lexbuf
      | R_ANGLE as token -> 
        lexer_state := SRangle lexbuf.lex_curr_p;
        token
      | token ->
        lexer_state := SRegular;
        token
      end
    | SPragma -> 
      begin match tokenize lexbuf with
      | PRAGMA_END as token -> 
        lexer_state := SRegular;
        token
      | NAME value as token ->
        let text = Value.Get.text value in
        lexer_state := SIdent(text, SPragma);
        token
      | token -> token
      end
}
