open Util.Source

(* [code] identifies a prose-generation diagnostic across runs. *)

type code =
  | Hint_placeholder_out_of_bounds
  | Hint_fields_arity_mismatch
  | Hint_fields_text_expected

let string_of_code = function
  | Hint_placeholder_out_of_bounds -> "hint-placeholder-out-of-bounds"
  | Hint_fields_arity_mismatch -> "hint-fields-arity-mismatch"
  | Hint_fields_text_expected -> "hint-fields-text-expected"

let render_code (c : code) : string = "prose/" ^ string_of_code c

exception ProseError of Diagnostic.t

let error ?code ?(related = []) (at : region) (msg : string) =
  let related =
    List.map (fun (region, message) -> { Diagnostic.region; message }) related
  in
  raise
    (ProseError
       (Diagnostic.error
          ?code:(Option.map render_code code)
          ~related ~source:"prose" at msg))
