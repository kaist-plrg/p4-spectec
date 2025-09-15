
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
open Il.Ast
open Util.Source
open Parser
open Wrap
module F = Format

exception Error of string

let current_line  = ref 1 
let current_fname = ref ""
let line_start    = ref 1

type lexer_state =
  (* Nothing to recall from the previous tokens *)
  | SRegular
  | SRangle of Source.info
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
  lexer_state := SRegular;
  current_line := 1;
  current_fname := "";
  line_start := 1

let line_number () = !current_line
let filename () = !current_fname
let start_of_line () = !line_start

let set_line n =
  current_line := n

let set_start_of_line c =
  line_start := c

let set_filename s =
  current_fname := s

let newline lexbuf =
  current_line := line_number() + 1 ;
  set_start_of_line (lexeme_end lexbuf)

let info lexbuf : Source.info =
  let f = filename () in
  let c1 = lexeme_start lexbuf in
  let c2 = lexeme_end lexbuf in
  let c = start_of_line () in
  let l = line_number() in
  Source.I 
    { filename = f;
      line_start = l;
      line_end = None;
      col_start = c1 - c;
      col_end = c2 - c }

let sanitize s =
  String.concat "" (String.split_on_char '_' s)

let strip_prefix s =
  let length = String.length s in
  assert (length > 2);
  String.sub s 2 (length - 2)

let parse_int n _info =
  let i = Bigint.of_string (sanitize n) in
  NumV (`Int i) |> with_typ (NumT `IntT)

let parse_width_int s n _info =
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
        let value_width =
          NumV (`Nat w) |> with_typ (NumT `NatT)
        in
        let value_int =
          NumV (`Int i) |> with_typ (NumT `IntT)
        in
        [ Term "FINT"; NT value_width; NT value_int ]
        |> wrap_case_v |> with_typ (wrap_var_t "number")
    | "w" ->
      let value_width =
        NumV (`Nat w) |> with_typ (NumT `NatT)
      in
      let value_int =
        NumV (`Int i) |> with_typ (NumT `IntT)
      in
      [ Term "FBIT"; NT value_width; NT value_int ]
      |> wrap_case_v |> with_typ (wrap_var_t "number")
    | _ ->
      raise (Error "Illegal integer constant")

let parse_text text  =
  TextV text $$$ { vid = Runtime_dynamic.Value.fresh (); typ = TextT }
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
       | Some info -> PRAGMA_END (info) }
  | "//"
      { singleline_comment lexbuf; tokenize lexbuf }
  | '\n'
      { newline lexbuf; PRAGMA_END (info lexbuf) }
  | '"'
      { let str, end_info = (string lexbuf) in
        end_info |> ignore;
        let value = 
          TextV str $$$ { vid = Runtime_dynamic.Value.fresh (); typ = TextT }
        in
        STRING_LITERAL value
      }
  | whitespace
      { tokenize lexbuf }
  | '#'
      { preprocessor lexbuf ; tokenize lexbuf }
  | "@pragma"
      { PRAGMA (parse_text "@pragma") }
  | hex_number as n
      { NUMBER_INT (parse_int n (info lexbuf), n) }
  | dec_number as n
      { NUMBER_INT (parse_int (strip_prefix n) (info lexbuf), n) }
  | oct_number as n
      { NUMBER_INT (parse_int n (info lexbuf), n) }
  | bin_number as n
      { NUMBER_INT (parse_int n (info lexbuf), n) }
  | int as n
      { NUMBER_INT (parse_int n (info lexbuf), n) }
  | (sign as s) (hex_number as n)
      { NUMBER (parse_width_int s n (info lexbuf), n) }
  | (sign as s) (dec_number as n)
      { NUMBER (parse_width_int s (strip_prefix n) (info lexbuf), n) }
  | (sign as s) (oct_number as n)
      { NUMBER (parse_width_int s n (info lexbuf), n) }
  | (sign as s) (bin_number as n)
      { NUMBER (parse_width_int s n (info lexbuf), n) }
  | (sign as s) (int as n)
      { NUMBER (parse_width_int s n (info lexbuf), n) }
  | "abstract"
      { ABSTRACT (parse_text "abstract") }
  | "action"
      { ACTION (parse_text "action") }
  | "actions"
      { ACTIONS (parse_text "actions") }
  | "apply"
      { APPLY (parse_text "apply") }
  | "bool"
      { BOOL (parse_text "bool") }
  | "bit"
      { BIT (parse_text "bit") }
  | "const"
      { CONST (parse_text "const") }
  | "control"
      { CONTROL (parse_text "control") }
  | "default"
      { DEFAULT (parse_text "default") }
  | "else"
      { ELSE (parse_text "else") }
  | "entries"
      { ENTRIES (parse_text "entries") }
  | "enum"
      { ENUM (parse_text "enum") }
  | "error"
      { ERROR (parse_text "error") }
  | "exit"
      { EXIT (parse_text "exit") }
  | "extern"
      { EXTERN (parse_text "extern") }
  | "header"
      { HEADER (parse_text "header") }
  | "header_union"
      { HEADER_UNION (parse_text "header_union") }
  | "true"
      { TRUE (parse_text "true") }
  | "false"
      { FALSE (parse_text "false") }
  | "for"
      { FOR (parse_text "for") }
  | "if"
      { IF (parse_text "if") }
  | "in"
      { IN (parse_text "in") }
  | "inout"
      { INOUT (parse_text "inout") }
  | "int"
      { INT (parse_text "int") }
  | "key"
      { KEY (parse_text "key") }
  | "list"
      { LIST (parse_text "list") }
  | "match_kind"
      { MATCH_KIND (parse_text "match_kind") }
  | "out"
      { OUT (parse_text "out") }
  | "parser"
      { PARSER (parse_text "parser") }
  | "package"
      { PACKAGE (parse_text "package") }
  | "pragma" 
      { PRAGMA (parse_text "pragma") }
  | "priority"
      { PRIORITY (parse_text "priority") }
  | "return"
      { RETURN (parse_text "return") }
  | "select"
      { SELECT (parse_text "select") }
  | "state"
      { STATE (parse_text "state") }
  | "string"
      { STRING (parse_text "string") }
  | "struct"
      { STRUCT (parse_text "struct") }
  | "switch"
      { SWITCH (parse_text "switch") }
  | "table"
      { TABLE (parse_text "table") }
  | "this"
      { THIS (parse_text "this") }  
  | "transition"
      { TRANSITION (parse_text "transition") }
  | "tuple"
      { TUPLE (parse_text "tuple") }
  | "typedef"
      { TYPEDEF (parse_text "typedef") }
  | "type"
      { TYPE (parse_text "type") }
  | "value_set"
      { VALUE_SET (parse_text "value_set") }
  | "varbit"
      { VARBIT (parse_text "varbit") }
  | "void"
      { VOID (parse_text "void") }
  | "_"
      { DONTCARE (parse_text "_") }
  | name
      { let text = Lexing.lexeme lexbuf in
        let value =
          let vid = Runtime_dynamic.Value.fresh () in
          let typ = Il.Ast.TextT in
          TextV text $$$ { vid; typ }
        in
        NAME value }
  | "<="
      {  LE (info lexbuf) }
  | ">="
      { GE (info lexbuf) }
  | "<<"
      { SHL (info lexbuf) }
  | "&&"
      { AND (info lexbuf) }
  | "||"
      { OR (info lexbuf) }
  | "!="
      { NE (info lexbuf) }
  | "=="
      { EQ (info lexbuf) }
  | "+"
      { PLUS (info lexbuf) }
  | "-"
      { MINUS (info lexbuf) }
  | "|+|"
      { PLUS_SAT (info lexbuf) }
  | "|-|"
      { MINUS_SAT (info lexbuf) }
  | "*"
      { MUL (info lexbuf) }
  | "{#}"
      { INVALID (info lexbuf) }
  | "/"
      { DIV (info lexbuf) }
  | "%"
      { MOD (info lexbuf) }
  | "|"
      { BIT_OR (info lexbuf) }
  | "&"
      { BIT_AND (info lexbuf) }
  | "^"
      { BIT_XOR (info lexbuf) }
  | "~"
      { COMPLEMENT (info lexbuf) }
  | "["
      { L_BRACKET (info lexbuf) }
  | "]"
      { R_BRACKET (info lexbuf) }
  | "{"
      { L_BRACE (info lexbuf) }
  | "}"
      { R_BRACE (info lexbuf) }
  | "<"
      { L_ANGLE (info lexbuf) }
  | ">"
      { R_ANGLE (info lexbuf) }
  | "("
      { L_PAREN (info lexbuf) }
  | ")"
      { R_PAREN (info lexbuf) }
  | "!"
      { NOT (info lexbuf) }
  | ":"
      { COLON (info lexbuf) }
  | ","
      { COMMA (info lexbuf) }
  | "?"
      { QUESTION (info lexbuf) }
  | "."
      { DOT (info lexbuf) }
  | "="
      { ASSIGN (info lexbuf) }
  | ";"
      { SEMICOLON (info lexbuf) }
  | "@"
      { AT (info lexbuf) }
  | "++"
      { PLUSPLUS (info lexbuf) }
  | "&&&"
      { MASK (info lexbuf) }
  | "..."
      { DOTS (info lexbuf) }
  | ".."
      { RANGE (info lexbuf) }
  | eof
      { END (info lexbuf) }
  | _
      { let text = lexeme lexbuf in
        let value =
          let vid = Runtime_dynamic.Value.fresh () in
          let typ = Il.Ast.TextT in
          TextV text $$$ { vid; typ }
        in
        UNEXPECTED_TOKEN value }
      
and string = parse
  | eof
      { raise (Error "File ended while reading a string literal") }
  | "\\\""
      { let rest, end_info = (string lexbuf) in
        ("\"" ^ rest, end_info) }
  | '\\' 'n'
      { let rest, end_info = (string lexbuf) in
        ("\n" ^ rest, end_info) }
  | '\\' '\\'
      { let rest, end_info = (string lexbuf) in
        ("\\" ^ rest, end_info) }
  | '\\' _ as c
      { raise (Error ("Escape sequences not yet supported: \\" ^ c)) }
  | '"'
      { ("", info lexbuf) }
  | _ as chr
      { let rest, end_info = (string lexbuf) in
        ((String.make 1 chr) ^ rest, end_info) }
    
(* Preprocessor annotations indicate line and filename *)
and preprocessor = parse
  | ' '
      { preprocessor lexbuf }
  | int
      { let line = int_of_string (lexeme lexbuf) in
        set_line line ; preprocessor lexbuf }
  | '"'
      { preprocessor_string lexbuf }
  | '\n'
      { set_start_of_line (lexeme_end lexbuf) }
  | _
      { preprocessor lexbuf }
      
and preprocessor_string = parse
  | [^ '"'] * '"'
    { let filename = lexeme lexbuf in 
      let filename = String.sub filename 0 (String.length filename - 1) in
      set_filename filename;
      preprocessor_column lexbuf }
      
(* Once a filename has been recognized, ignore the rest of the line *)
and preprocessor_column = parse
  | ' ' 
      { preprocessor_column lexbuf }
  | '\n'
      { set_start_of_line (lexeme_end lexbuf) }
  | eof
      { () }
  | _
      { preprocessor_column lexbuf }
      
(* Multi-line comment terminated by "*/" *)
and multiline_comment opt = parse
  | "*/"   { opt }
  | eof    { failwith "unterminated comment" }
  | '\n'   { newline lexbuf; multiline_comment (Some(info lexbuf)) lexbuf }
  | _      { multiline_comment opt lexbuf }
      
(* Single-line comment terminated by a newline *)
and singleline_comment = parse
  | '\n'   { newline lexbuf }
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
    | SRangle info1 -> 
      begin match tokenize lexbuf with
      | R_ANGLE info2 when Source.follows info1 info2 -> 
        lexer_state := SRegular;
        R_ANGLE_SHIFT info2
      | PRAGMA _ as token ->
        lexer_state := SPragma;
        token
      | PRAGMA_END _ -> 
        lexer_state := SRegular;
        lexer lexbuf
      | NAME value as token ->
        let text = Value.get_text value in
        lexer_state := SIdent (text, SRegular);
        token          
      | token -> 
        lexer_state := SRegular;
        token
      end
    | SRegular ->
      begin match tokenize lexbuf with
      | NAME value as token ->
        let text = Value.get_text value in
        lexer_state := SIdent (text, SRegular);
        token
      | PRAGMA _ as token ->
        lexer_state := SPragma;
        token
      | PRAGMA_END _ ->
        lexer lexbuf
      | R_ANGLE info as token -> 
        lexer_state := SRangle info;
        token
      | token ->
        lexer_state := SRegular;
        token
       end
    | STemplate ->
      begin match tokenize lexbuf with
      | L_ANGLE info -> L_ANGLE_ARGS info
      | NAME value as token ->
        let text = Value.get_text value in
        lexer_state := SIdent (text, SRegular);
        token
      | PRAGMA _ as token ->
        lexer_state := SPragma;
        token
      | PRAGMA_END _ -> lexer lexbuf
      | R_ANGLE info as token -> 
        lexer_state := SRangle info;
        token
      | token ->
        lexer_state := SRegular;
        token
       end
    | SPragma -> 
      begin match tokenize lexbuf with
      | PRAGMA_END _info as token -> 
         lexer_state := SRegular;
         token
      | NAME value as token ->
         let text = Value.get_text value in
         lexer_state := SIdent(text, SPragma);
         token
      | token -> token
      end
}

