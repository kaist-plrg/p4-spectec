open Lang
module Value = Runtime.Value
module Run = Runtime.Dynamic_Runner.Signature
open Util.Error
open Util.Source

(* Interfaces *)

(* P4 *)

module P4 = struct
  (* Program unparser *)

  let unparser = ref (fun (_ : Value.t) -> "")

  (* Program parsing *)

  let parse_program (includes_p4 : string list) (filenames_p4 : string list) :
      Run.parse_result =
    try
      match filenames_p4 with
      | [ filename_p4 ] ->
          let value_program = P4.Parse.parse_file includes_p4 filename_p4 in
          Run.Pass value_program
      | _ ->
          Run.Fail (`Syntax (no_region, "exactly one P4 file must be provided"))
    with ParseError (at, msg) -> Run.Fail (`Syntax (at, msg))

  let parse_string (filename_p4 : string) (str : string) : Run.parse_result =
    try
      let value_program = P4.Parse.parse_string filename_p4 str in
      Run.Pass value_program
    with ParseError (at, msg) -> Run.Fail (`Syntax (at, msg))

  (* Program unparsing *)

  let unparse_program (value_program : Value.t) : string =
    !unparser value_program

  (* Initialization *)

  let init (spec : Run.spec) : unit =
    let printer (value : Value.t) =
      match spec with
      | IL spec_il ->
          let henv = P4.Unparse.hints_of_spec_il spec_il in
          Format.asprintf "%a" (P4.Unparse.pp_value henv) value
      | SL spec_sl ->
          let henv = P4.Unparse.hints_of_spec_sl spec_sl in
          Format.asprintf "%a" (P4.Unparse.pp_value henv) value
      | Empty -> assert false
    in
    unparser := printer
end

(* SpecTec *)

module SpecTec = struct
  include Spectec.Boot
  include Spectec.Unboot

  (* Program parsing *)

  let parse_program (_includes : string list) (filenames : string list) :
      Run.parse_result =
    try
      let value_spec = Spectec.Parse.parse_files filenames in
      Run.Pass value_spec
    with
    | ParseError (at, msg) -> Run.Fail (`Syntax (at, msg))
    | ElabError (at, msg) -> Run.Fail (`Syntax (at, msg))

  let parse_string (filename : string) (str : string) : Run.parse_result =
    try
      let value_spec = Spectec.Parse.parse_string filename str in
      Run.Pass value_spec
    with
    | ParseError (at, msg) -> Run.Fail (`Syntax (at, msg))
    | ElabError (at, msg) -> Run.Fail (`Syntax (at, msg))

  (* Program unparsing *)

  let unparse_program (value_program : Value.t) : string =
    value_program |> unboot_spec |> Il.Print.string_of_spec

  (* Initialization *)

  let init (_spec : Run.spec) : unit = ()
end
