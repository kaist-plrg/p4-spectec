open Util.Error
open Util.Source

let error = error_parse

let preprocess (includes : string list) (filename : string) =
  try Preprocessor.preprocess includes filename
  with _ -> error no_region "preprocessor error"

let lex (filename : string) (file : string) =
  try
    let () = Lexer.reset () in
    let () = Lexer.set_filename filename in
    Lexing.from_string file
  with Lexer.Error s -> error no_region (Format.asprintf "lexer error: %s" s)

let parse (lexbuf : Lexing.lexbuf) =
  try Parser.p4program Lexer.lexer lexbuf with
  | Lexer.Error s ->
      let info = Lexer.info lexbuf in
      let msg = Format.asprintf "lexer error: %s" s in
      error (Source.to_region info) msg
  | Parser.Error ->
      let info = Lexer.info lexbuf in
      let msg = Format.asprintf "syntax error" in
      error (Source.to_region info) msg
  | e -> raise e

let parse_string (filename : string) (str : string) : Il.Ast.value =
  (* assume str is preprocessed *)
  let tokens = lex filename str in
  parse tokens

let parse_file (includes : string list) (filename : string) : Il.Ast.value =
  let program = preprocess includes filename in
  parse_string filename program
