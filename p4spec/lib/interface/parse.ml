open Lang
module Value = Runtime.Dynamic_Sl.Value
open Util.Error

let error = error_parse
let error_no_region = error_parse_no_region

let preprocess (includes : string list) (filename : string) =
  try Preprocessor.preprocess includes filename
  with _ -> "preprocessor error" |> error_no_region

let lex (filename : string) (file : string) =
  try
    let () = Lexer.reset () in
    let () = Lexer.set_filename filename in
    Lexing.from_string file
  with Lexer.Error s -> Format.asprintf "lexer error: %s" s |> error_no_region

let parse (lexbuf : Lexing.lexbuf) =
  let debug_level = Debug_config.get_parser_debug_level () in
  try
    match debug_level with
    | Debug_config.Quiet -> Parser.p4program Lexer.lexer lexbuf
    | _ -> Parser_debug.debug_parse Lexer.lexer lexbuf
  with
  | Lexer.Error s ->
      let info = Lexer.info lexbuf in
      let msg = Format.asprintf "lexer error: %s" s in
      error (Source.to_region info) msg
  | Parser.Error ->
      let info = Lexer.info lexbuf in
      let msg = Format.asprintf "syntax error" in
      error (Source.to_region info) msg
  | e -> raise e

let parse_string (filename : string) (str : string) : Il.value =
  (* Assume str is preprocessed *)
  let tokens = lex filename str in
  parse tokens

let parse_file (includes : string list) (filename : string) : Il.value =
  let program = preprocess includes filename in
  parse_string filename program

let parse_file_fresh (includes : string list) (filename : string) : Il.value =
  Value.refresh ();
  parse_file includes filename
