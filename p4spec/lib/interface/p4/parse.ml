module Value = Runtime.Value

let error = Error.error
let error_no_region = Error.error_no_region

let preprocess (includes : string list) (path : string) =
  try Preprocessor.preprocess includes path
  with _ -> "preprocessor error" |> error_no_region

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
  (* Assume str is preprocessed *)
  let tokens = lex path str in
  parse tokens

let parse_file (includes : string list) (path : string) : Value.t =
  let program = preprocess includes path in
  parse_string path program

let parse_file_fresh (includes : string list) (path : string) : Value.t =
  Value.Fresh_.refresh ();
  parse_file includes path
