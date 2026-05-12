open Lang
module Typ = Runtime.Type.Typ
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

  (* Builtins *)

  module Builtin_P4_Ext = struct
    (* dec $print_<X>(X) : text *)

    let print (add : Value.t -> unit) (at : region) (targs : Typ.t list)
        (values_input : Value.t list) : Value.t =
      let _typ = Builtin.Extract.one at targs in
      let value = Builtin.Extract.one at values_input in
      let text = !unparser value in
      let value = Value.Make.text text in
      add value;
      value

    (* Builtin extension entries *)

    let entries = [ ("print_", print) ]
  end

  module Builtin_P4 = Builtin.Call.Make (Builtin_P4_Ext) ()

  let call_builtin = Builtin_P4.invoke

  (* State management *)

  let checkpoint = Builtin_P4.checkpoint
  let seff = Builtin_P4.seff

  (* Cache management *)

  module Cache = struct
    let cache_on () = ()
    let cache_off () = ()
  end

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
  include Spectec.Common.Boot
  include Spectec.Common.Unboot
  include Spectec.Ili.Boot
  include Spectec.Ili.Unboot
  include Spectec.Sli.Boot
  include Spectec.Sli.Unboot
  include Spectec.Caches

  (* Mode *)

  let mode : Run.mode ref = ref Run.Empty_mode

  (* Program parsing *)

  let parse_program (_includes : string list) (filenames : string list) :
      Run.parse_result =
    try
      let value_spec = Spectec.Parse.parse_files !mode filenames in
      Run.Pass value_spec
    with
    | ParseError (at, msg) -> Run.Fail (`Syntax (at, msg))
    | ElabError (at, msg) -> Run.Fail (`Syntax (at, msg))

  let parse_string (filename : string) (str : string) : Run.parse_result =
    try
      let value_spec = Spectec.Parse.parse_string !mode filename str in
      Run.Pass value_spec
    with
    | ParseError (at, msg) -> Run.Fail (`Syntax (at, msg))
    | ElabError (at, msg) -> Run.Fail (`Syntax (at, msg))

  (* Program unparsing *)

  let unparse_program (value_script : Value.t) : string =
    match !mode with
    | IL_mode -> value_script |> unboot_scriptIL |> Il.Print.string_of_spec
    | SL_mode -> value_script |> unboot_scriptSL |> Sl.Print.string_of_spec
    | Empty_mode -> assert false

  (* Builtins *)

  module Builtin_SpecTec = Builtin.Call.Make (Builtin.Call.No_ext) ()

  let call_builtin = Builtin_SpecTec.invoke

  (* State management *)

  let checkpoint = Builtin_SpecTec.checkpoint
  let seff = Builtin_SpecTec.seff

  (* Initialization *)

  let init (spec : Run.spec) : unit =
    match spec with
    | IL _ -> mode := IL_mode
    | SL _ -> mode := SL_mode
    | Empty -> assert false
end
