module Value = Runtime.Value
module Run = Runtime.Dynamic_Runner.Signature
open Util.Error
open Util.Source

(* Program unparser *)

let unparser = ref (fun (_ : Value.t) -> "")

(* Program parsing *)

let parse_program (includes_p4 : string list) (filenames_p4 : string list) :
    Run.parse_result =
  try
    match filenames_p4 with
    | [ filename_p4 ] ->
        let value_program = Parse.parse_file includes_p4 filename_p4 in
        Run.Pass value_program
    | _ ->
        Run.Fail (`Syntax (no_region, "exactly one P4 file must be provided"))
  with ParseError (at, msg) -> Run.Fail (`Syntax (at, msg))

let parse_string (filename_p4 : string) (str : string) : Run.parse_result =
  try
    let value_program = Parse.parse_string filename_p4 str in
    Run.Pass value_program
  with ParseError (at, msg) -> Run.Fail (`Syntax (at, msg))

(* Program unparsing *)

let unparse_program (value_program : Value.t) : string = !unparser value_program

(* Initialization *)

let init (spec : Run.spec) : unit =
  let printer (value : Value.t) =
    match spec with
    | IL spec_il ->
        let henv = Unparse.hints_of_spec_il spec_il in
        Format.asprintf "%a" (Unparse.pp_value henv) value
    | SL spec_sl ->
        let henv = Unparse.hints_of_spec_sl spec_sl in
        Format.asprintf "%a" (Unparse.pp_value henv) value
    | Empty -> assert false
  in
  unparser := printer
