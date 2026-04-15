open Domain
open Lang
open Error
module Source = Util.Source
open Source

let with_lexbuf name lexbuf start =
  let open Lexing in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };
  try start Lexer.token lexbuf
  with Parser.Error ->
    error (Lexer.region lexbuf) "syntax error: unexpected token"

let parse_mixop str =
  let rec mixop_of_nottyp (nottyp : El.nottyp) (mixop_rev : Mixop.t) : Mixop.t =
    match nottyp.it with
    | AtomT atom -> Mixop.Atom atom :: mixop_rev
    | SeqT typs ->
        List.fold_left
          (fun mixop_rev typ -> mixop_of_typ typ mixop_rev)
          mixop_rev typs
    | InfixT (typ_l, atom, typ_r) ->
        let mixop_rev = mixop_of_typ typ_l mixop_rev in
        let mixop_rev = Mixop.Atom atom :: mixop_rev in
        mixop_of_typ typ_r mixop_rev
    | BrackT (atom_l, typ, atom_r) ->
        let mixop_rev = Mixop.Atom atom_l :: mixop_rev in
        let mixop_rev = mixop_of_typ typ mixop_rev in
        Mixop.Atom atom_r :: mixop_rev
  and mixop_of_typ (typ : El.typ) (mixop_rev : Mixop.t) =
    match typ with
    | PlainT _ -> Mixop.Arg :: mixop_rev
    | NotationT nottyp -> mixop_of_nottyp nottyp mixop_rev
  in
  let typ = Parser.check_typ Lexer.token (Lexing.from_string str) in
  List.rev (mixop_of_typ typ [])

let parse_file file =
  let ic = open_in file in
  try
    Fun.protect
      (fun () -> with_lexbuf file (Lexing.from_channel ic) Parser.spec)
      ~finally:(fun () -> close_in ic)
  with Sys_error msg ->
    error (Source.region_of_file file) ("i/o error: " ^ msg)
