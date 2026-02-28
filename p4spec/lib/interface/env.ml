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

type t = {
  mutable context : Context.t;
  mutable line : int;
  mutable fname : string;
  mutable line_start : int;
  mutable state : lexer_state;
}

type parse_state = {
  lex_env : t;
}

let init filename = {
  context = Context.empty;
  line = 1;
  fname = filename;
  line_start = 1;
  state = SRegular;
}
