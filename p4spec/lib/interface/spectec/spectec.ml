open Lang
module Value = Runtime.Value
module Run = Runtime.Dynamic_Runner.Signature
open Util.Error

(* Program parsing *)

let parse_program (_includes : string list) (filenames : string list) :
    Run.parse_result =
  try
    let value_spec = Parse.parse_files filenames in
    Run.Pass value_spec
  with
  | ParseError (at, msg) -> Run.Fail (`Syntax (at, msg))
  | ElabError (at, msg) -> Run.Fail (`Syntax (at, msg))

let parse_string (filename : string) (str : string) : Run.parse_result =
  try
    let value_spec = Parse.parse_string filename str in
    Run.Pass value_spec
  with
  | ParseError (at, msg) -> Run.Fail (`Syntax (at, msg))
  | ElabError (at, msg) -> Run.Fail (`Syntax (at, msg))

(* Program unparsing *)

let unparse_program (value_program : Value.t) : string =
  value_program |> Unboot.unboot_spec |> Il.Print.string_of_spec

(* Initialization *)

let init (_spec : Run.spec) : unit = ()
