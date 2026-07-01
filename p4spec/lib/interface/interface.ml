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

  let parse_program (includes_p4 : string list) (paths_p4 : string list) :
      Run.parse_result =
    try
      match paths_p4 with
      | [ path_p4 ] ->
          let value_program = P4.Parse.parse_file includes_p4 path_p4 in
          Run.Pass value_program
      | _ ->
          Run.Fail (`Syntax (no_region, "exactly one P4 file must be provided"))
    with ParseError (at, msg) -> Run.Fail (`Syntax (at, msg))

  let parse_string (path_p4 : string) (str : string) : Run.parse_result =
    try
      let value_program = P4.Parse.parse_string path_p4 str in
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

(* Nano-P4 *)

module NanoP4 = struct
  let parse_program (includes : string list) (paths : string list) :
      Run.parse_result =
    try
      match paths with
      | [ path ] ->
          let value_program = Nano.Parse.parse_file includes path in
          Run.Pass value_program
      | _ ->
          Run.Fail
            (`Syntax (no_region, "exactly one nano-P4 file must be provided"))
    with ParseError (at, msg) -> Run.Fail (`Syntax (at, msg))

  let parse_string (path : string) (str : string) : Run.parse_result =
    try
      let value_program = Nano.Parse.parse_string path str in
      Run.Pass value_program
    with ParseError (at, msg) -> Run.Fail (`Syntax (at, msg))

  let unparse_program (_value_program : Value.t) : string = "<nano-p4>"

  module Builtin_NanoP4_Ext = struct
    (* dec $print_<X>(X) : text *)

    let print (add : Value.t -> unit) (at : region) (targs : Typ.t list)
        (values_input : Value.t list) : Value.t =
      let _typ = Builtin.Extract.one at targs in
      let value = Builtin.Extract.one at values_input in
      let text = Lang.Il.Print.string_of_value value in
      let value = Value.Make.text text in
      add value;
      value

    let entries = [ ("print_", print) ]
  end

  module Builtin_NanoP4 = Builtin.Call.Make (Builtin_NanoP4_Ext) ()

  let call_builtin = Builtin_NanoP4.invoke
  let checkpoint = Builtin_NanoP4.checkpoint
  let seff = Builtin_NanoP4.seff

  module Cache = struct
    let cache_on () = ()
    let cache_off () = ()
  end

  let init (_spec : Run.spec) : unit = ()
end

(* SpecTec IL *)

module SpecTec_IL = struct
  include Spectec.Common.Boot
  include Spectec.Common.Unboot
  include Spectec.Ili.Boot
  include Spectec.Ili.Unboot
  include Spectec.Caches

  (* Program parsing *)

  let parse_program (_includes : string list) (paths : string list) :
      Run.parse_result =
    try
      let value_spec = Spectec.Parse.parse_files Run.IL_mode paths in
      Run.Pass value_spec
    with
    | ParseError (at, msg) -> Run.Fail (`Syntax (at, msg))
    | ElabError (at, msg) -> Run.Fail (`Syntax (at, msg))

  let parse_string (path : string) (str : string) : Run.parse_result =
    try
      let value_spec = Spectec.Parse.parse_string Run.IL_mode path str in
      Run.Pass value_spec
    with
    | ParseError (at, msg) -> Run.Fail (`Syntax (at, msg))
    | ElabError (at, msg) -> Run.Fail (`Syntax (at, msg))

  (* Program unparsing *)

  let unparse_program (value_script : Value.t) : string =
    value_script |> unboot_script |> Il.Print.string_of_spec

  (* Builtins *)

  module Builtin_SpecTec = Builtin.Call.Make (Builtin.Call.No_ext) ()

  let call_builtin = Builtin_SpecTec.invoke

  (* State management *)

  let checkpoint = Builtin_SpecTec.checkpoint
  let seff = Builtin_SpecTec.seff

  (* Initialization *)

  let init (_spec : Run.spec) : unit = ()
end

(* SpecTec SL *)

module SpecTec_SL = struct
  include Spectec.Common.Boot
  include Spectec.Common.Unboot
  include Spectec.Sli.Boot
  include Spectec.Sli.Unboot
  include Spectec.Caches

  (* Program parsing *)

  let parse_program (_includes : string list) (paths : string list) :
      Run.parse_result =
    try
      let value_spec = Spectec.Parse.parse_files Run.SL_mode paths in
      Run.Pass value_spec
    with
    | ParseError (at, msg) -> Run.Fail (`Syntax (at, msg))
    | ElabError (at, msg) -> Run.Fail (`Syntax (at, msg))

  let parse_string (path : string) (str : string) : Run.parse_result =
    try
      let value_spec = Spectec.Parse.parse_string Run.SL_mode path str in
      Run.Pass value_spec
    with
    | ParseError (at, msg) -> Run.Fail (`Syntax (at, msg))
    | ElabError (at, msg) -> Run.Fail (`Syntax (at, msg))

  (* Program unparsing *)

  let unparse_program (value_script : Value.t) : string =
    value_script |> unboot_script |> Sl.Print.string_of_spec

  (* Builtins *)

  module Builtin_SpecTec = Builtin.Call.Make (Builtin.Call.No_ext) ()

  let call_builtin = Builtin_SpecTec.invoke

  (* State management *)

  let checkpoint = Builtin_SpecTec.checkpoint
  let seff = Builtin_SpecTec.seff

  (* Initialization *)

  let init (_spec : Run.spec) : unit = ()
end
