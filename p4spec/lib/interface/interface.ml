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
  (* Boot-time-only entry points used by backend-boot/patch.ml: they
     process the spec itself (elaboration/patch input), never a runtime
     value under whatever mode the meta-interpreter ends up running, so
     they stay fixed to [Valrep.V_value] regardless of [Make]'s [V]. *)

  module Boot_value = Spectec.Ili.Boot.Make (Valrep.V_value)
  module Unboot_value = Spectec.Ili.Unboot.Make (Valrep.V_value)

  let boot_spec = Boot_value.boot_spec
  let unboot_script = Unboot_value.unboot_script

  (* Program parsing doesn't depend on the runtime value rep either. *)

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

  (* The mode-dependent boundary: which [V] the running meta-interpreter
     uses for runtime values (interpreted [Valrep.V_value], or under
     [ML_mode] the compiled interface's native [V_native]) is fixed at
     construction — the same call sites in [build.ml] that already pick
     [Interp_ml]/[V] for [Spectec.Make_null]/[Make_parametric] pick it
     here too, instead of a runtime [!cur_mode] branch with a bespoke
     [Obj.magic] in every function. *)

  module Make (V : Runtime.Valrep.VAL) = struct
    include Spectec.Caches

    let boot_spec = boot_spec
    let unboot_script = unboot_script
    let parse_program = parse_program
    let parse_string = parse_string

    module Boot = Spectec.Ili.Boot.Make (V)
    module Unboot = Spectec.Ili.Unboot.Make (V)

    (* [unparse_program] crosses [Value.t] -> [V] through [V.of_value] (the
       [SAFE]/[UNSAFE] boundary, not a hand-written [Obj.magic]) since
       [Run.INTERFACE] fixes its argument at [Value.t]. [boot_*]/[unboot_*]
       don't need that crossing at all — [INTERFACE_SPECTEC] types them
       directly over this functor's own [vt = V.t]. *)

    type vt = V.t

    let unparse_program (value_script : Value.t) : string =
      Il.Print.string_of_spec (Unboot.unboot_script (V.of_value value_script))

    let boot_value (value : Value.t) : vt = Boot.boot_value (value : Il.value)

    let boot_values (values : Value.t list) : vt =
      Boot.boot_values (values : Il.value list)

    let unboot_id (value : vt) : Il.id = Unboot.unboot_id value
    let unboot_typs (value : vt) : Typ.t list = Unboot.unboot_typs value
    let unboot_values (value : vt) : Value.t list = Unboot.unboot_values value

    (* Builtins *)

    module Builtins (V : Valrep.SAFE) : Run.BUILTINS with type vt = V.t = struct
      type vt = V.t

      module F = Builtin.Call.Make_funcs (V)
      include F.Make (F.No_ext) ()
    end

    module Builtin_SpecTec = Builtins (V)

    let call_builtin (add : Value.t -> unit) (id : Domain.Lib.Id.t)
        (typs : Typ.t list) (values : Value.t list) : Value.t =
      Builtin_SpecTec.invoke
        (fun v -> add (V.to_value v))
        id typs
        (List.map V.of_value values)
      |> V.to_value

    (* Fixed at [Valrep.V_value] regardless of this functor's own [V] — for
       callers (e.g. [Call_builtin_func]) whose [values] are already real
       [Value.t] (from [unboot_values]); [call_builtin] above would
       [V.of_value]-recast them with no actual conversion. *)

    module Builtin_SpecTec_value = Builtins (Valrep.V_value)

    let call_builtin_value (add : Value.t -> unit) (id : Domain.Lib.Id.t)
        (typs : Typ.t list) (values : Value.t list) : Value.t =
      Builtin_SpecTec_value.invoke add id typs values

    (* State management *)

    let checkpoint = Builtin_SpecTec.checkpoint
    let seff = Builtin_SpecTec.seff

    (* Initialization: [V] already fixes the mode, so there is nothing
       left to configure from [spec] at this point. *)

    let init (_ : Run.spec) : unit = ()
  end
end

(* SpecTec SL *)

module SpecTec_SL = struct
  (* Boot-time-only entry points used by backend-boot/patch.ml: they
     process the spec itself (elaboration/patch input), never a runtime
     value under whatever mode the meta-interpreter ends up running, so
     they stay fixed to [Valrep.V_value] regardless of [Make]'s [V]. *)

  module Boot_value = Spectec.Sli.Boot.Make (Valrep.V_value)
  module Unboot_value = Spectec.Sli.Unboot.Make (Valrep.V_value)

  let boot_spec = Boot_value.boot_spec
  let unboot_script = Unboot_value.unboot_script

  (* Program parsing doesn't depend on the runtime value rep either. *)

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

  (* The mode-dependent boundary: which [V] the running meta-interpreter
     uses for runtime values (interpreted [Valrep.V_value], or under
     [ML_mode] the compiled interface's native [V_native]) is fixed at
     construction — the same call sites in [build.ml] that already pick
     [Interp_ml]/[V] for [Spectec.Make_null]/[Make_parametric] pick it
     here too, instead of a runtime [!cur_mode] branch with a bespoke
     [Obj.magic] in every function. *)

  module Make (V : Runtime.Valrep.VAL) = struct
    include Spectec.Caches

    let boot_spec = boot_spec
    let unboot_script = unboot_script
    let parse_program = parse_program
    let parse_string = parse_string

    module Boot = Spectec.Sli.Boot.Make (V)
    module Unboot = Spectec.Sli.Unboot.Make (V)

    (* [unparse_program] crosses [Value.t] -> [V] through [V.of_value] (the
       [SAFE]/[UNSAFE] boundary, not a hand-written [Obj.magic]) since
       [Run.INTERFACE] fixes its argument at [Value.t]. [boot_*]/[unboot_*]
       don't need that crossing at all — [INTERFACE_SPECTEC] types them
       directly over this functor's own [vt = V.t]. *)

    type vt = V.t

    let unparse_program (value_script : Value.t) : string =
      Sl.Print.string_of_spec (Unboot.unboot_script (V.of_value value_script))

    let boot_value (value : Value.t) : vt = Boot.boot_value (value : Il.value)

    let boot_values (values : Value.t list) : vt =
      Boot.boot_values (values : Il.value list)

    let unboot_id (value : vt) : Il.id = Unboot.unboot_id value
    let unboot_typs (value : vt) : Typ.t list = Unboot.unboot_typs value
    let unboot_values (value : vt) : Value.t list = Unboot.unboot_values value

    (* Builtins *)

    module Builtins (V : Valrep.SAFE) : Run.BUILTINS with type vt = V.t = struct
      type vt = V.t

      module F = Builtin.Call.Make_funcs (V)
      include F.Make (F.No_ext) ()
    end

    module Builtin_SpecTec = Builtins (V)

    let call_builtin (add : Value.t -> unit) (id : Domain.Lib.Id.t)
        (typs : Typ.t list) (values : Value.t list) : Value.t =
      Builtin_SpecTec.invoke
        (fun v -> add (V.to_value v))
        id typs
        (List.map V.of_value values)
      |> V.to_value

    (* Fixed at [Valrep.V_value] regardless of this functor's own [V] — for
       callers (e.g. [Call_builtin_func]) whose [values] are already real
       [Value.t] (from [unboot_values]); [call_builtin] above would
       [V.of_value]-recast them with no actual conversion. *)

    module Builtin_SpecTec_value = Builtins (Valrep.V_value)

    let call_builtin_value (add : Value.t -> unit) (id : Domain.Lib.Id.t)
        (typs : Typ.t list) (values : Value.t list) : Value.t =
      Builtin_SpecTec_value.invoke add id typs values

    (* State management *)

    let checkpoint = Builtin_SpecTec.checkpoint
    let seff = Builtin_SpecTec.seff

    (* Initialization: [V] already fixes the mode, so there is nothing
       left to configure from [spec] at this point. *)

    let init (_ : Run.spec) : unit = ()
  end
end
