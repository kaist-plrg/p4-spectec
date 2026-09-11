open Util.Source

(* [code] identifies a splice diagnostic across runs. *)

type code = File_io_error

let string_of_code = function File_io_error -> "file-io-error"
let render_code (code : code) : string = "splice/" ^ string_of_code code

(* Error *)

exception SpliceError of Diagnostic.t

let error ?code (at : region) (msg : string) =
  raise
    (SpliceError
       (Diagnostic.error
          ?code:(Option.map render_code code)
          ~source:"splice" at msg))

let warn ?(detail : string option) (at : region) (msg : string) =
  Diagnostic.warn ?detail ~source:"splice" at msg

(* Check *)

let check (b : bool) (at : region) (msg : string) : unit =
  if not b then error at msg

let guard (b : bool) (at : region) (msg : string) : unit =
  if not b then warn at msg
