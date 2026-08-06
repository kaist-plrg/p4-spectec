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
      | AL spec_al ->
          let henv = P4.Unparse.hints_of_spec_al spec_al in
          Format.asprintf "%a" (P4.Unparse.pp_value henv) value
      | SL spec_sl ->
          let henv = P4.Unparse.hints_of_spec_sl spec_sl in
          Format.asprintf "%a" (P4.Unparse.pp_value henv) value
      | PL spec_pl ->
          let henv = P4.Unparse.hints_of_spec_pl spec_pl in
          Format.asprintf "%a" (P4.Unparse.pp_value henv) value
      | Empty -> assert false
    in
    unparser := printer
end

(* SpecTec IL *)

module SpecTec_AL = struct
  include Spectec.Common.Boot
  include Spectec.Common.Unboot
  include Spectec.Ali.Boot
  include Spectec.Ali.Unboot
  include Spectec.Caches

  (* Program parsing *)

  let parse_program (_includes : string list) (paths : string list) :
      Run.parse_result =
    try
      let value_spec = Spectec.Parse.parse_files Run.AL_mode paths in
      Run.Pass value_spec
    with
    | ParseError (at, msg) -> Run.Fail (`Syntax (at, msg))
    | ElabError (at, msg) -> Run.Fail (`Syntax (at, msg))

  let parse_string (path : string) (str : string) : Run.parse_result =
    try
      let value_spec = Spectec.Parse.parse_string Run.AL_mode path str in
      Run.Pass value_spec
    with
    | ParseError (at, msg) -> Run.Fail (`Syntax (at, msg))
    | ElabError (at, msg) -> Run.Fail (`Syntax (at, msg))

  (* Program unparsing *)

  let unparse_program (value_script : Value.t) : string =
    value_script |> unboot_script |> Al.Print.string_of_spec

  (* Program rendering as KAST, for the K specification in `spec-meta-k/` *)

  exception Kast_error = Spectec.Ali.Kast.Error

  let kast_of_program (value_script : Value.t) : string =
    Spectec.Ali.Kast.string_of_script value_script

  (* External interface for builtin calls, for the K specification in
     `spec-meta-k/`.  K has no builtin implementations of its own: it
     serializes the call and shells out to `spectec-boot builtin`, so this
     module is the authority for what a builtin computes. *)

  exception Extern_error = Spectec.Ali.Extern.Error

  let request_of_json = Spectec.Ali.Extern.request_of_json
  let json_of_response = Spectec.Ali.Extern.json_of_response

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
