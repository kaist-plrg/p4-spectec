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
open Lang
open Il
open Lexing
open Context
open Tokens
open Wrap
module Value = Runtime.Dynamic_Il.Value
module F = Format

exception Error of string

let debug_channel = ref stderr
let set_debug_channel ch = debug_channel := ch
let lexer_debug_enabled () = Debug_config.lexer_debug_enabled Debug_config.Basic

let debug_print fmt =
  if Debug_config.lexer_debug_enabled Debug_config.Basic then
    Printf.fprintf !debug_channel fmt
  else
    Printf.ifprintf !debug_channel fmt

let debug_token lexeme =
  debug_print "%s" lexeme

let current_line  = ref 1 
let current_fname = ref ""
let line_start    = ref 1

(* let lexer_state = ref SRegular *)
(**)
(* let reset () = *)
(*   Context.reset (); *)
(*   lexer_state := SRegular; *)
(*   current_line := 1; *)
(*   current_fname := ""; *)
(*   line_start := 1 *)

let line_number () = !current_line
let filename () = !current_fname
let start_of_line () = !line_start

let set_lexer_debug_channel ch = set_debug_channel ch
let newline (env : Env.t) lexbuf =
  env.line <- env.line + 1;
  env.line_start <- lexeme_end lexbuf

let info (env : Env.t) lexbuf : Source.info =
  Source.I 
    { filename = env.fname;
      line_start = env.line;
      line_end = None;
      col_start = lexeme_start lexbuf - env.line_start;
      col_end = lexeme_end lexbuf - env.line_start; }

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
        [ NT value_width; Term "S"; NT value_int ]
        |> wrap_case_v |> with_typ (wrap_var_t "integerLiteral")
    | "w" ->
      let value_width =
        NumV (`Nat w) |> with_typ (NumT `NatT)
      in
      let value_int =
        NumV (`Int i) |> with_typ (NumT `IntT)
      in
      [ NT value_width; Term "W"; NT value_int ]
      |> wrap_case_v |> with_typ (wrap_var_t "integerLiteral")
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

rule tokenize env = parse
  | "/*"
      { debug_token "/*";
        match multiline_comment env None lexbuf with 
       | None -> tokenize env lexbuf
       | Some info -> PRAGMA_END (info) }
  | "//"
      { singleline_comment env lexbuf; tokenize env lexbuf }
  | '\n'
      { debug_token "⏎\n"; newline env lexbuf; PRAGMA_END (info env lexbuf) }
  | '"'
      { let str, end_info = (string env lexbuf) in
        debug_token ("\"" ^ str ^ "\"");
        end_info |> ignore;
        let value = Value.make TextT (TextV str) in
        STRING_LITERAL value
      }
  | whitespace
      { debug_token " "; tokenize env lexbuf }
  | '#'
      { debug_token ""; preprocessor env lexbuf ; tokenize env lexbuf }
  | "@pragma"
      { debug_token "@pragma"; PRAGMA (info env lexbuf) }
  | hex_number as n
      { debug_token n; NUMBER_INT (parse_int n (info env lexbuf), n) }
  | dec_number as n
      { debug_token n; NUMBER_INT (parse_int (strip_prefix n) (info env lexbuf), n) }
  | oct_number as n
      { debug_token n; NUMBER_INT (parse_int n (info env lexbuf), n) }
  | bin_number as n
      { debug_token n; NUMBER_INT (parse_int n (info env lexbuf), n) }
  | int as n
      { debug_token n; NUMBER_INT (parse_int n (info env lexbuf), n) }
  | (sign as s) (hex_number as n)
      { NUMBER (parse_width_int s n (info env lexbuf), n) }
  | (sign as s) (dec_number as n)
      { NUMBER (parse_width_int s (strip_prefix n) (info env lexbuf), n) }
  | (sign as s) (oct_number as n)
      { NUMBER (parse_width_int s n (info env lexbuf), n) }
  | (sign as s) (bin_number as n)
      { NUMBER (parse_width_int s n (info env lexbuf), n) }
  | (sign as s) (int as n)
      { NUMBER (parse_width_int s n (info env lexbuf), n) }
  | "abstract"
      { debug_token "abstract"; ABSTRACT (info env lexbuf) }
  | "action"
      { debug_token "action"; ACTION (info env lexbuf) }
  | "actions"
      { debug_token "actions"; ACTIONS (info env lexbuf) }
  | "apply"
      { debug_token "apply"; APPLY (info env lexbuf) }
  | "bool"
      { debug_token "bool"; BOOL (info env lexbuf) }
  | "bit"
      { debug_token "bit"; BIT (info env lexbuf) }
  | "break"
      { debug_token "break"; BREAK (info env lexbuf) }
  | "const"
      { debug_token "const"; CONST (info env lexbuf) }
  | "continue"
      { debug_token "continue"; CONTINUE (info env lexbuf) }
  | "control"
      { debug_token "control"; CONTROL (info env lexbuf) }
  | "default"
      { debug_token "default"; DEFAULT (info env lexbuf) }
  | "else"
      { debug_token "else"; ELSE (info env lexbuf) }
  | "entries"
      { debug_token "entries"; ENTRIES (info env lexbuf) }
  | "enum"
      { debug_token "enum"; ENUM (info env lexbuf) }
  | "error"
      { debug_token "error"; ERROR (info env lexbuf) }
  | "exit"
      { debug_token "exit"; EXIT (info env lexbuf) }
  | "extern"
      { debug_token "extern"; EXTERN (info env lexbuf) }
  | "header"
      { debug_token "header"; HEADER (info env lexbuf) }
  | "header_union"
      { debug_token "header_union"; HEADER_UNION (info env lexbuf) }
  | "true"
      { debug_token "true"; TRUE (info env lexbuf) }
  | "false"
      { debug_token "false"; FALSE (info env lexbuf) }
  | "for"
      { debug_token "for"; FOR (info env lexbuf) }
  | "if"
      { debug_token "if"; IF (info env lexbuf) }
  | "in"
      { debug_token "in"; IN (info env lexbuf) }
  | "inout"
      { debug_token "inout"; INOUT (info env lexbuf) }
  | "int"
      { debug_token "int"; INT (info env lexbuf) }
  | "key"
      { debug_token "key"; KEY (info env lexbuf) }
  | "list"
      { debug_token "list"; LIST (info env lexbuf) }
  | "match_kind"
      { debug_token "match_kind"; MATCH_KIND (info env lexbuf) }
  | "out"
      { debug_token "out"; OUT (info env lexbuf) }
  | "parser"
      { debug_token "parser"; PARSER (info env lexbuf) }
  | "package"
      { debug_token "package"; PACKAGE (info env lexbuf) }
  | "pragma" 
      { debug_token "pragma"; PRAGMA (info env lexbuf) }
  | "priority"
      { debug_token "priority"; PRIORITY (info env lexbuf) }
  | "return"
      { debug_token "return"; RETURN (info env lexbuf) }
  | "select"
      { debug_token "select"; SELECT (info env lexbuf) }
  | "state"
      { debug_token "state"; STATE (info env lexbuf) }
  | "string"
      { debug_token "string"; STRING (info env lexbuf) }
  | "struct"
      { debug_token "struct"; STRUCT (info env lexbuf) }
  | "switch"
      { debug_token "switch"; SWITCH (info env lexbuf) }
  | "table"
      { debug_token "table"; TABLE (info env lexbuf) }
  | "this"
      { debug_token "this"; THIS (info env lexbuf) }  
  | "transition"
      { debug_token "transition"; TRANSITION (info env lexbuf) }
  | "tuple"
      { debug_token "tuple"; TUPLE (info env lexbuf) }
  | "typedef"
      { debug_token "typedef"; TYPEDEF (info env lexbuf) }
  | "type"
      { debug_token "type"; TYPE (info env lexbuf) }
  | "value_set"
      { debug_token "value_set"; VALUE_SET (info env lexbuf) }
  | "varbit"
      { debug_token "varbit"; VARBIT (info env lexbuf) }
  | "void"
      { debug_token "void"; VOID (info env lexbuf) }
  | "_"
      { debug_token "_"; DONTCARE (info env lexbuf) }
  | name
      { let text = Lexing.lexeme lexbuf in
        debug_token text;
        let value = Value.make Il.TextT (TextV text) in
        NAME value }
  | "<="
      { debug_token "<="; LE (info env lexbuf) }
  | ">="
      { debug_token ">="; GE (info env lexbuf) }
  | "<<"
      { debug_token "<<"; SHL (info env lexbuf) }
  | "&&"
      { debug_token "&&"; AND (info env lexbuf) }
  | "||"
      { debug_token "||"; OR (info env lexbuf) }
  | "!="
      { debug_token "!="; NE (info env lexbuf) }
  | "=="
      { debug_token "=="; EQ (info env lexbuf) }
  | "+"
      { debug_token "+"; PLUS (info env lexbuf) }
  | "-"
      { debug_token "-"; MINUS (info env lexbuf) }
  | "|+|"
      { debug_token "|+|"; PLUS_SAT (info env lexbuf) }
  | "|-|"
      { debug_token "|-|"; MINUS_SAT (info env lexbuf) }
  | "*"
      { debug_token "*"; MUL (info env lexbuf) }
  | "{#}"
      { debug_token "{#}"; INVALID (info env lexbuf) }
  | "/"
      { debug_token "/"; DIV (info env lexbuf) }
  | "%"
      { debug_token "%"; MOD (info env lexbuf) }
  | "|"
      { debug_token "|"; BIT_OR (info env lexbuf) }
  | "&"
      { debug_token "&"; BIT_AND (info env lexbuf) }
  | "^"
      { debug_token "^"; BIT_XOR (info env lexbuf) }
  | "~"
      { debug_token "~"; COMPLEMENT (info env lexbuf) }
  | "["
      { debug_token "["; L_BRACKET (info env lexbuf) }
  | "]"
      { debug_token "]"; R_BRACKET (info env lexbuf) }
  | "{"
      { debug_token "{"; L_BRACE (info env lexbuf) }
  | "}"
      { debug_token "}"; R_BRACE (info env lexbuf) }
  | "<"
      { debug_token "<"; L_ANGLE (info env lexbuf) }
  | ">"
      { debug_token ">"; R_ANGLE (info env lexbuf) }
  | "("
      { debug_token "("; L_PAREN (info env lexbuf) }
  | ")"
      { debug_token ")"; R_PAREN (info env lexbuf) }
  | "!"
      { debug_token "!"; NOT (info env lexbuf) }
  | ":"
      { debug_token ":"; COLON (info env lexbuf) }
  | ","
      { debug_token ","; COMMA (info env lexbuf) }
  | "?"
      { debug_token "?"; QUESTION (info env lexbuf) }
  | "."
      { debug_token "."; DOT (info env lexbuf) }
  | "="
      { debug_token "="; ASSIGN (info env lexbuf) }
  | ";"
      { debug_token ";"; SEMICOLON (info env lexbuf) }
  | "@"
      { debug_token "@"; AT (info env lexbuf) }
  | "++"
      { debug_token "++"; PLUSPLUS (info env lexbuf) }
  | "&&&"
      { debug_token "&&&"; MASK (info env lexbuf) }
  | "..."
      { debug_token "..."; DOTS (info env lexbuf) }
  | ".."
      { debug_token ".."; RANGE (info env lexbuf) }
  | "+="
      { debug_token "+="; PLUS_ASSIGN (info env lexbuf) }
  | "|+|="
      { debug_token "|+|="; PLUS_SAT_ASSIGN (info env lexbuf) }
  | "-="
      { debug_token "-="; MINUS_ASSIGN (info env lexbuf) }
  | "|-|="
      { debug_token "|-|="; MINUS_SAT_ASSIGN (info env lexbuf) }
  | "*="
      { debug_token "*="; MUL_ASSIGN (info env lexbuf) }
  | "/="
      { debug_token "/="; DIV_ASSIGN (info env lexbuf) } 
  | "%="
      { debug_token "%="; MOD_ASSIGN (info env lexbuf) }
  | "<<="
      { debug_token "<<="; SHL_ASSIGN (info env lexbuf) }
  | ">>="
      { debug_token ">>="; SHR_ASSIGN (info env lexbuf) }
  | "&="
      { debug_token "&="; BIT_AND_ASSIGN (info env lexbuf) }
  | "^="
      { debug_token "^="; BIT_XOR_ASSIGN (info env lexbuf) }
  | "|="
      { debug_token "|="; BIT_OR_ASSIGN (info env lexbuf) }
  | eof
      { debug_token "EOF"; END (info env lexbuf) }
  | _
      { let text = lexeme lexbuf in
        debug_token text;
        let value = Value.make Il.TextT (TextV text) in
        UNEXPECTED_TOKEN value }

and string env = parse
  | eof
      { raise (Error "File ended while reading a string literal") }
  | "\\\""
      { let rest, end_info = (string env lexbuf) in
        ("\"" ^ rest, end_info) }
  | '\\' 'n'
      { let rest, end_info = (string env lexbuf) in
        ("\n" ^ rest, end_info) }
  | '\\' '\\'
      { let rest, end_info = (string env lexbuf) in
        ("\\" ^ rest, end_info) }
  | '\\' _ as c
      { raise (Error ("Escape sequences not yet supported: \\" ^ c)) }
  | '"'
      { ("", info env lexbuf) }
  | _ as chr
      { let rest, end_info = (string env lexbuf) in
        ((String.make 1 chr) ^ rest, end_info) }
    
(* Preprocessor annotations indicate line and filename *)
and preprocessor env = parse
  | ' '
      { preprocessor env lexbuf }
  | int
      { let line = int_of_string (lexeme lexbuf) in
        env.line <- line ; preprocessor env lexbuf }
  | '"'
      { preprocessor_string env lexbuf }
  | '\n'
      { env.line_start <- (lexeme_end lexbuf) }
  | _
      { preprocessor env lexbuf }

and preprocessor_string env = parse
  | [^ '"'] * '"'
    { let filename = lexeme lexbuf in 
      let filename = String.sub filename 0 (String.length filename - 1) in
      env.fname <- filename; 
      preprocessor_column env lexbuf }

(* Once a filename has been recognized, ignore the rest of the line *)
and preprocessor_column env = parse
  | ' ' 
      { preprocessor_column env lexbuf }
  | '\n'
      { env.line_start <- (lexeme_end lexbuf) }
  | eof
      { () }
  | _
      { preprocessor_column env lexbuf }

(* Multi-line comment terminated by "*/" *)
and multiline_comment env opt = parse
  | "*/"   { opt }
  | eof    { failwith "unterminated comment" }
  | '\n'   { newline env lexbuf; multiline_comment env (Some(info env lexbuf)) lexbuf }
  | _      { multiline_comment env opt lexbuf }

(* Single-line comment terminated by a newline *)
and singleline_comment env = parse
  | '\n'   { newline env lexbuf }
  | eof    { () }
  | _      { singleline_comment env lexbuf }

{
let rec lexer (env : Env.t) (lexbuf : lexbuf): token = 
   match env.state with
    | SIdent(id, next) ->
      begin match get_kind env.context id with
      | TypeName true ->
        env.state <- STemplate;
        TYPENAME
      | Ident true ->
        env.state <- STemplate;
        IDENTIFIER
      | TypeName false ->
        env.state <- next;
        TYPENAME
      | Ident false ->
        env.state <- next;
        IDENTIFIER
      end
    | SRangle info1 -> 
      begin match tokenize env lexbuf with
      | R_ANGLE info2 when Source.follows info1 info2 -> 
        env.state <- SRegular;
        R_ANGLE_SHIFT info2
      | PRAGMA _ as token ->
        env.state <- SPragma;
        token
      | PRAGMA_END _ -> 
        env.state <- SRegular;
        lexer env lexbuf
      | NAME value as token ->
        let text = Value.get_text value in
        env.state <- SIdent (text, SRegular);
        token
      | token -> 
        env.state <- SRegular;
        token
      end
    | SRegular ->
      begin match tokenize env lexbuf with
      | NAME value as token ->
        let text = Value.get_text value in
        env.state <- SIdent (text, SRegular);
        token
      | PRAGMA _ as token ->
        env.state <- SPragma;
        token
      | PRAGMA_END _ ->
        lexer env lexbuf
      | R_ANGLE info as token -> 
        env.state <- SRangle info;
        token
      | token ->
        env.state <- SRegular;
        token
       end
    | STemplate ->
      begin match tokenize env lexbuf with
      | L_ANGLE info -> L_ANGLE_ARGS info
      | NAME value as token ->
        let text = Value.get_text value in
        env.state <- SIdent (text, SRegular);
        token
      | PRAGMA _ as token ->
        env.state <- SPragma;
        token
      | PRAGMA_END _ -> lexer env lexbuf
      | R_ANGLE info as token -> 
        env.state <- SRangle info;
        token
      | token ->
        env.state <- SRegular;
        token
       end
    | SPragma -> 
      begin match tokenize env lexbuf with
      | PRAGMA_END _info as token -> 
         env.state <- SRegular;
         token
      | NAME value as token ->
         let text = Value.get_text value in
         env.state <- SIdent(text, SPragma);
         token
      | token -> token
      end
}
