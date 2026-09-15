open Domain
open Lang
open Error
module Source = Util.Source
open Source

type error = Diagnostic.t

let label_lexbuf name lexbuf =
  let open Lexing in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name }

let unexpected_token_message lexbuf =
  match Lexing.lexeme lexbuf with
  | "" -> "unexpected end of input"
  | token -> Format.asprintf "unexpected token %s" (Diagnostic.quote token)

let with_lexbuf name lexbuf start =
  label_lexbuf name lexbuf;
  try start Lexer.token lexbuf
  with Parser.Error ->
    error ~code:Unexpected_token (Lexer.region lexbuf)
      (unexpected_token_message lexbuf)

let parse_mixop str =
  let rec mixop_of_nottyp (nottyp : El.nottyp) =
    match nottyp.it with
    | AtomT atom -> Mixfix.Atom atom
    | SeqT typs ->
        let mixops = List.map mixop_of_typ typs in
        Mixfix.Seq mixops
    | InfixT (typ_l, atom, typ_r) ->
        let mixop_l = mixop_of_typ typ_l in
        let mixop_r = mixop_of_typ typ_r in
        Mixfix.Infix (mixop_l, atom, mixop_r)
    | BrackT (atom_l, typ, atom_r) ->
        let mixop = mixop_of_typ typ in
        Mixfix.Brack (atom_l, mixop, atom_r)
  and mixop_of_typ (typ : El.typ) =
    match typ with
    | PlainT _ -> Mixfix.Arg ()
    | NotationT nottyp -> mixop_of_nottyp nottyp
  in
  let lexbuf = Lexing.from_string str in
  label_lexbuf "<mixfix>" lexbuf;
  let typ =
    try Parser.check_typ Lexer.token lexbuf
    with Parser.Error ->
      error ~code:Malformed_mixop no_region
        (if str = "" then "mixfix operator must not be empty"
         else
           Format.asprintf "mixfix operator %s is malformed"
             (Diagnostic.quote str))
  in
  mixop_of_typ typ

let parse_file file =
  try
    let ic = open_in file in
    Fun.protect
      (fun () -> with_lexbuf file (Lexing.from_channel ic) Parser.spec)
      ~finally:(fun () -> close_in ic)
  with Sys_error msg ->
    error ~code:File_io_error
      (Source.region_of_file file)
      ("I/O error: " ^ Diagnostic.quote msg)

let expand_path path =
  if Sys_unix.is_directory_exn path then
    Util.Filesys.collect_files ~suffix:".watsup" path
  else [ path ]

let parse_files paths =
  try
    Ok (paths |> List.concat_map expand_path |> List.concat_map parse_file)
  with
  | ParseError d -> Error d
  | Sys_error msg ->
      Error
        (Diagnostic.error
           ~code:(render_code Input_path_io_error)
           ~source:"parse" no_region
           ("I/O error: " ^ Diagnostic.quote msg))

let parse_string str =
  let lexbuf = Lexing.from_string str in
  try Ok (with_lexbuf "<string>" lexbuf Parser.spec)
  with ParseError d -> Error d
