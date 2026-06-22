module Value = Runtime.Value
open Util.Error

let error = error_parse
let error_no_region = error_parse_no_region

let lex (path : string) (file : string) =
  try
    let () = Lexer.reset () in
    let lexbuf = Lexing.from_string file in
    let () = Lexing.set_filename lexbuf path in
    lexbuf
  with Lexer.Error s -> Format.asprintf "lexer error: %s" s |> error_no_region

let parse (lexbuf : Lexing.lexbuf) : Value.t =
  try Parser.p4program Lexer.lexer lexbuf with
  | Lexer.Error s ->
      let at = Lexer.at lexbuf in
      let msg = Format.asprintf "lexer error: %s" s in
      error at msg
  | Parser.Error ->
      let at = Lexer.at lexbuf in
      let msg = Format.asprintf "syntax error" in
      error at msg
  | e -> raise e

let parse_string (path : string) (str : string) : Value.t =
  let tokens = lex path str in
  parse tokens

let parse_file (path : string) : Value.t =
  let ic = open_in path in
  let content =
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    close_in ic;
    Bytes.to_string s
  in
  parse_string path content

let parse_file_fresh (path : string) : Value.t =
  Value.Fresh_.refresh ();
  parse_file path
