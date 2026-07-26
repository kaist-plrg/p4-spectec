open Domain
open Lang
open Error
module Source = Util.Source
open Source

(* Errors *)

type error = { at : region; msg : string }

let to_region_msg { at; msg } = (at, msg)

let with_lexbuf name lexbuf start =
  let open Lexing in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };
  try start Lexer.token lexbuf
  with Parser.Error ->
    error (Lexer.region lexbuf) "syntax error: unexpected token"

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
  let typ =
    try Parser.check_typ Lexer.token lexbuf
    with Parser.Error ->
      error (Lexer.region lexbuf)
        (Format.asprintf "syntax error in mixop string: %s" str)
  in
  mixop_of_typ typ

let parse_file file =
  let ic = open_in file in
  try
    Fun.protect
      (fun () -> with_lexbuf file (Lexing.from_channel ic) Parser.spec)
      ~finally:(fun () -> close_in ic)
  with Sys_error msg ->
    error (Source.region_of_file file) ("i/o error: " ^ msg)

let parse_files files =
  try Ok (List.concat_map parse_file files)
  with ParseError (at, msg) -> Error { at; msg }

let parse_string str =
  let lexbuf = Lexing.from_string str in
  try Ok (Parser.spec Lexer.token lexbuf) with
  | Parser.Error ->
      let at = Lexer.region lexbuf in
      Error { at; msg = Format.asprintf "syntax error in spec string: %s" str }
  | ParseError (at, msg) -> Error { at; msg }
