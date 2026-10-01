open Util.Source

(* [code] identifies a parser diagnostic across runs. *)

type code =
  | Unexpected_character
  | Misplaced_control_char
  | Misplaced_unicode_char
  | Malformed_utf8
  | Malformed_utf8_in_comment
  | Hole_index_out_of_range
  | Unclosed_text_literal
  | Illegal_control_in_text_literal
  | Illegal_escape
  | Unclosed_block_comment
  | Notation_type_expected
  | Struct_no_fields
  | Variant_no_cases
  | Syntax_empty_body
  | Syntax_no_ids
  | Hint_on_plain_type
  | Unexpected_token
  | Malformed_mixop
  | File_io_error
  | Input_path_io_error

let string_of_code = function
  | Unexpected_character -> "unexpected-character"
  | Misplaced_control_char -> "misplaced-control-char"
  | Misplaced_unicode_char -> "misplaced-unicode-char"
  | Malformed_utf8 -> "malformed-utf8"
  | Malformed_utf8_in_comment -> "malformed-utf8-in-comment"
  | Hole_index_out_of_range -> "hole-index-out-of-range"
  | Unclosed_text_literal -> "unclosed-text-literal"
  | Illegal_control_in_text_literal -> "illegal-control-in-text-literal"
  | Illegal_escape -> "illegal-escape"
  | Unclosed_block_comment -> "unclosed-block-comment"
  | Notation_type_expected -> "notation-type-expected"
  | Struct_no_fields -> "struct-no-fields"
  | Variant_no_cases -> "variant-no-cases"
  | Syntax_empty_body -> "syntax-empty-body"
  | Syntax_no_ids -> "syntax-no-ids"
  | Hint_on_plain_type -> "hint-on-plain-type"
  | Unexpected_token -> "unexpected-token"
  | Malformed_mixop -> "malformed-mixop"
  | File_io_error -> "file-io-error"
  | Input_path_io_error -> "input-path-io-error"

let render_code (c : code) : string = "parse/" ^ string_of_code c

exception ParseError of Diagnostic.t

let error ?code ?detail (at : region) (msg : string) =
  raise
    (ParseError
       (Diagnostic.error
          ?code:(Option.map render_code code)
          ?detail ~source:"parse" at msg))
