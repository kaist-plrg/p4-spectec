{
open Lexing
open Context
open Parser
module Value = Runtime.Value
open Value.Make
open Util.Source

exception Error of string

type lexer_state =
  | SRegular
  | SIdent of string

let lexer_state = ref SRegular

let reset () =
  Context.reset ();
  lexer_state := SRegular

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
      { multiline_comment lexbuf; tokenize lexbuf }
  | "//"
      { singleline_comment lexbuf; tokenize lexbuf }
  | '\n'
      { Lexing.new_line lexbuf; tokenize lexbuf }
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
      { preprocessor lexbuf; tokenize lexbuf }
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
  | "const"
      { CONST }
  | "control"
      { CONTROL }
  | "else"
      { ELSE }
  | "entries"
      { ENTRIES }
  | "error"
      { ERROR }
  | "extern"
      { EXTERN }
  | "header"
      { HEADER }
  | "true"
      { TRUE }
  | "false"
      { FALSE }
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
  | "select"
      { SELECT }
  | "state"
      { STATE }
  | "string"
      { STRING }
  | "struct"
      { STRUCT }
  | "table"
      { TABLE }
  | "transition"
      { TRANSITION }
  | "void"
      { VOID }
  | "_"
      { DONTCARE }
  | name
      { let text = Lexing.lexeme lexbuf in
        let value = Value.Make.text ~at:(at lexbuf) text in
        lexer_state := SIdent text;
        NAME value }
  | "<="
      { LE }
  | ">="
      { GE }
  | "<<"
      { SHL }
  | ">>"
      { SHR }
  | "&&"
      { AND }
  | "||"
      { OR }
  | "!="
      { NE }
  | "=="
      { EQ }
  | "++"
      { PLUSPLUS }
  | "+"
      { PLUS }
  | "-"
      { MINUS }
  | "*"
      { MUL }
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
  | eof
      { END }
  | _
      { let text = lexeme lexbuf in
        raise (Error (Printf.sprintf "unexpected character: %s" text)) }

and preprocessor = parse
  | ' '
      { preprocessor lexbuf }
  | int
      { let line = int_of_string (Lexing.lexeme lexbuf) in
        let pos = lexbuf.Lexing.lex_curr_p in
        lexbuf.Lexing.lex_curr_p <- { pos with Lexing.pos_lnum = line };
        preprocessor lexbuf }
  | '"'
      { preprocessor_string lexbuf }
  | '\n'
       { let bol = Lexing.lexeme_end lexbuf in
         let pos = lexbuf.Lexing.lex_curr_p in
         lexbuf.Lexing.lex_curr_p <- { pos with Lexing.pos_bol = bol } }
  | _
      { preprocessor lexbuf }
  | eof
      { () }

and preprocessor_string = parse
  | [^ '"'] * '"'
    { let path = Lexing.lexeme lexbuf in
      let path = String.sub path 0 (String.length path - 1) in
      Lexing.set_filename lexbuf path;
      preprocessor_rest lexbuf }

and preprocessor_rest = parse
  | '\n'
    { let bol = Lexing.lexeme_end lexbuf in
      let pos = lexbuf.Lexing.lex_curr_p in
      lexbuf.Lexing.lex_curr_p <- { pos with Lexing.pos_bol = bol } }
  | eof
    { () }
  | _
    { preprocessor_rest lexbuf }

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

and multiline_comment = parse
  | "*/"   { () }
  | eof    { failwith "unterminated comment" }
  | '\n'   { Lexing.new_line lexbuf; multiline_comment lexbuf }
  | _      { multiline_comment lexbuf }

and singleline_comment = parse
  | '\n'   { Lexing.new_line lexbuf }
  | eof    { () }
  | _      { singleline_comment lexbuf }

{
let lexer (lexbuf : lexbuf) : token =
  match !lexer_state with
  | SIdent id ->
    lexer_state := SRegular;
    if is_typename id then TYPENAME else IDENTIFIER
  | SRegular ->
    tokenize lexbuf
}
