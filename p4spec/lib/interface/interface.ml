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

  (* dec $print_<X>(X) : text *)

  module Builtin_P4_Ext (V : Valrep.SAFE) = struct
    let print (add : V.t -> unit) (at : region) (targs : Typ.t list)
        (values_input : V.t list) : V.t =
      let typ = Builtin.Extract.one at targs in
      let value = Builtin.Extract.one at values_input in
      let text = !unparser (V.marshal typ value) in
      let value = V.Make.text text in
      add value;
      value

    (* Builtin extension entries *)

    let entries = [ ("print_", print) ]
  end

  module Builtins (V : Valrep.SAFE) : Run.BUILTINS with type vt = V.t = struct
    type vt = V.t

    module F = Builtin.Call.Make_funcs (V)
    include F.Make (Builtin_P4_Ext (V)) ()
  end

  module Builtin_P4 = Builtins (Valrep.V_value)

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
      | ML -> P4.Unparse_compiled.print value
      | Empty -> assert false
    in
    unparser := printer
end

(* SpecTec IL *)

module SpecTec_IL = struct
  module Boot_value = Spectec.Ili.Boot.Make (Valrep.V_value)
  module Unboot_value = Spectec.Ili.Unboot.Make (Valrep.V_value)
  module Boot_native = Spectec.Ili.Boot.Make (Backend_ocaml.Val_native.V_native)
  module Unboot_native =
    Spectec.Ili.Unboot.Make (Backend_ocaml.Val_native.V_native)

  include Spectec.Caches

  (* Boot-time-only entry points used by backend-boot/patch.ml,
     never under [ML_mode]. *)

  let boot_spec = Boot_value.boot_spec
  let unboot_script = Unboot_value.unboot_script

  (* The mode of the running meta-interpreter; picks which Boot/Unboot
     variant the boundary functions dispatch to. *)

  let cur_mode : Run.mode ref = ref Run.Empty_mode

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
    let spec =
      match !cur_mode with
      | Run.ML_mode -> Unboot_native.unboot_script (Obj.magic value_script)
      | _ -> Unboot_value.unboot_script value_script
    in
    Il.Print.string_of_spec spec

  (* Boundary functions bridge [Value.t] to mode-correct [V];
     exactly one [Obj.magic] per [ML_mode] branch. *)

  let boot_value (value : Value.t) : Value.t =
    match !cur_mode with
    | Run.ML_mode ->
        (Obj.magic (Boot_native.boot_value (Obj.magic value : Il.value))
          : Value.t)
    | _ -> Boot_value.boot_value (value : Il.value)

  let boot_values (values : Value.t list) : Value.t =
    match !cur_mode with
    | Run.ML_mode ->
        (Obj.magic
           (Boot_native.boot_values (Obj.magic values : Il.value list))
          : Value.t)
    | _ -> Boot_value.boot_values values

  let unboot_id (value : Value.t) : Il.id =
    match !cur_mode with
    | Run.ML_mode -> Unboot_native.unboot_id (Obj.magic value)
    | _ -> Unboot_value.unboot_id value

  let unboot_typs (value : Value.t) : Typ.t list =
    match !cur_mode with
    | Run.ML_mode -> Unboot_native.unboot_typs (Obj.magic value)
    | _ -> Unboot_value.unboot_typs value

  let unboot_values (value : Value.t) : Value.t list =
    match !cur_mode with
    | Run.ML_mode ->
        (Obj.magic (Unboot_native.unboot_values (Obj.magic value))
          : Value.t list)
    | _ -> Unboot_value.unboot_values value

  (* Builtins *)

  module Builtins (V : Valrep.SAFE) : Run.BUILTINS with type vt = V.t = struct
    type vt = V.t

    module F = Builtin.Call.Make_funcs (V)
    include F.Make (F.No_ext) ()
  end

  module Builtin_SpecTec = Builtins (Valrep.V_value)
  module Builtin_SpecTec_native = Builtins (Backend_ocaml.Val_native.V_native)

  let call_builtin (add : Value.t -> unit) (id : Domain.Lib.Id.t)
      (typs : Typ.t list) (values : Value.t list) : Value.t =
    match !cur_mode with
    | Run.ML_mode ->
        (Obj.magic
           (Builtin_SpecTec_native.invoke
              (fun v -> add (Obj.magic v : Value.t))
              id typs
              (Obj.magic values : Backend_ocaml.Val_native.V_native.t list))
          : Value.t)
    | _ -> Builtin_SpecTec.invoke add id typs values

  (* State management *)

  let checkpoint = Builtin_SpecTec.checkpoint
  let seff = Builtin_SpecTec.seff

  (* Initialization *)

  let init (spec : Run.spec) : unit =
    cur_mode :=
      (match spec with
      | Run.IL _ -> Run.IL_mode
      | Run.ML -> Run.ML_mode
      | Run.SL _ | Run.Empty -> assert false)
end

(* SpecTec SL *)

module SpecTec_SL = struct
  module Boot_value = Spectec.Sli.Boot.Make (Valrep.V_value)
  module Unboot_value = Spectec.Sli.Unboot.Make (Valrep.V_value)
  module Boot_native = Spectec.Sli.Boot.Make (Backend_ocaml.Val_native.V_native)
  module Unboot_native =
    Spectec.Sli.Unboot.Make (Backend_ocaml.Val_native.V_native)

  include Spectec.Caches

  (* Boot-time-only entry points used by backend-boot/patch.ml,
     never under [ML_mode]. *)

  let boot_spec = Boot_value.boot_spec
  let unboot_script = Unboot_value.unboot_script

  (* The mode of the running meta-interpreter; picks which Boot/Unboot
     variant the boundary functions dispatch to. *)

  let cur_mode : Run.mode ref = ref Run.Empty_mode

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
    let spec =
      match !cur_mode with
      | Run.ML_mode -> Unboot_native.unboot_script (Obj.magic value_script)
      | _ -> Unboot_value.unboot_script value_script
    in
    Sl.Print.string_of_spec spec

  (* Boundary functions bridge [Value.t] to mode-correct [V];
     exactly one [Obj.magic] per [ML_mode] branch. *)

  let boot_value (value : Value.t) : Value.t =
    match !cur_mode with
    | Run.ML_mode ->
        (Obj.magic (Boot_native.boot_value (Obj.magic value : Il.value))
          : Value.t)
    | _ -> Boot_value.boot_value (value : Il.value)

  let boot_values (values : Value.t list) : Value.t =
    match !cur_mode with
    | Run.ML_mode ->
        (Obj.magic
           (Boot_native.boot_values (Obj.magic values : Il.value list))
          : Value.t)
    | _ -> Boot_value.boot_values values

  let unboot_id (value : Value.t) : Il.id =
    match !cur_mode with
    | Run.ML_mode -> Unboot_native.unboot_id (Obj.magic value)
    | _ -> Unboot_value.unboot_id value

  let unboot_typs (value : Value.t) : Typ.t list =
    match !cur_mode with
    | Run.ML_mode -> Unboot_native.unboot_typs (Obj.magic value)
    | _ -> Unboot_value.unboot_typs value

  let unboot_values (value : Value.t) : Value.t list =
    match !cur_mode with
    | Run.ML_mode ->
        (Obj.magic (Unboot_native.unboot_values (Obj.magic value))
          : Value.t list)
    | _ -> Unboot_value.unboot_values value

  (* Builtins *)

  module Builtins (V : Valrep.SAFE) : Run.BUILTINS with type vt = V.t = struct
    type vt = V.t

    module F = Builtin.Call.Make_funcs (V)
    include F.Make (F.No_ext) ()
  end

  module Builtin_SpecTec = Builtins (Valrep.V_value)
  module Builtin_SpecTec_native = Builtins (Backend_ocaml.Val_native.V_native)

  let call_builtin (add : Value.t -> unit) (id : Domain.Lib.Id.t)
      (typs : Typ.t list) (values : Value.t list) : Value.t =
    match !cur_mode with
    | Run.ML_mode ->
        (Obj.magic
           (Builtin_SpecTec_native.invoke
              (fun v -> add (Obj.magic v : Value.t))
              id typs
              (Obj.magic values : Backend_ocaml.Val_native.V_native.t list))
          : Value.t)
    | _ -> Builtin_SpecTec.invoke add id typs values

  (* State management *)

  let checkpoint = Builtin_SpecTec.checkpoint
  let seff = Builtin_SpecTec.seff

  (* Initialization *)

  let init (spec : Run.spec) : unit =
    cur_mode :=
      (match spec with
      | Run.SL _ -> Run.SL_mode
      | Run.ML -> Run.ML_mode
      | Run.IL _ | Run.Empty -> assert false)
end
