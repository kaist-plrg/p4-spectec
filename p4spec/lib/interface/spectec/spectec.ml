open Lang
module Value = Runtime.Value
module Sim = Runtime.Sim.Simulator
open Util.Error

(* Program parsing *)

let parse_program (_includes : string list) (filenames : string list) :
    Sim.parse_result =
  try
    let value_spec = Parse.parse_files filenames in
    Sim.Pass value_spec
  with
  | ParseError (at, msg) -> Sim.Fail (`Syntax (at, msg))
  | ElabError (at, msg) -> Sim.Fail (`Syntax (at, msg))

let parse_string (_filename : string) (_str : string) : Sim.parse_result =
  failwith "TODO"

(* Program unparsing *)

let unparse_program (value_program : Value.t) : string =
  value_program |> Unboot.unboot_spec |> Il.Print.string_of_spec

(* Initialization *)

let init (_spec : Sim.spec) : unit = ()
