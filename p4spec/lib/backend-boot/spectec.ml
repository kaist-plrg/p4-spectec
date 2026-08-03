module Typ = Runtime.Type.Typ
module Value = Runtime.Value
module CCache = Runtime.Dynamic.Caches.CallCache
module Run = Runtime.Dynamic_Runner.Signature
open Error
open Util.Source

(* A wrapper for SpecTec interfaces, providing apis for caching boot/unboots *)

module type INTERFACE_SPECTEC = sig
  include Run.INTERFACE

  (* Interface cache *)

  type cache

  val make_cache : unit -> cache
  val push_cache : cache -> unit
  val pop_cache : unit -> unit
  val cache_enable : cache -> unit
  val cache_disable_reset : cache -> unit
  val cache_clear : cache -> unit

  (* Boot / unboots *)

  val boot_value : Value.t -> Value.t
  val boot_values : Value.t list -> Value.t
  val unboot_id : Value.t -> string phrase
  val unboot_typs : Value.t -> Typ.t list
  val unboot_values : Value.t -> Value.t list
end

(* The null layer *)

module Make_null
    (Interface_SpecTec : INTERFACE_SPECTEC)
    (Interp_AL : Run.INTERP_AL)
    (Interp_SL : Run.INTERP_SL)
    (Interp_PL : Run.INTERP_PL) : Run.EXTERN = struct
  (* Mode initialization *)

  let call_func = ref (fun _ _ _ -> assert false)

  let init_mode mode_ =
    let call_func_ name typs values =
      (match mode_ with
      | Run.AL_mode -> Interp_AL.eval_func name typs values
      | Run.SL_mode -> Interp_SL.eval_func name typs values
      | Run.PL_mode -> Interp_PL.eval_func name typs values
      | Run.Empty_mode -> assert false)
      |> function
      | Pass value -> value
      | Fail (at, msg) -> error at msg
    in
    call_func := call_func_;
    ()

  (* Threading extern calls to the interpreter *)

  let call_builtin_func (values_input : Value.t list) : Value.t list =
    let value_id, value_typs, value_values =
      match values_input with
      | [ value_id; value_typs; value_values ] ->
          (value_id, value_typs, value_values)
      | _ ->
          error_no_region "unexpected number of arguments to call_builtin_func"
    in
    let id = value_id |> Interface_SpecTec.unboot_id in
    let typs = value_typs |> Interface_SpecTec.unboot_typs in
    let values = value_values |> Interface_SpecTec.unboot_values in
    let value_output = !call_func id.it typs values in
    let value_value_output = Interface_SpecTec.boot_value value_output in
    let value_value_output_res =
      Value.Make.("OK val" <| [ value_value_output ] <<| "valres")
    in
    [ value_value_output_res ]

  (* Cache management *)

  (* Externs *)

  let eval_extern_rel (name : string) (values_input : Value.t list) :
      Run.rel_result =
    try
      Run.Pass
        (match name with
        | "Call_builtin_func" -> call_builtin_func values_input
        | _ ->
            error no_region
              (Format.asprintf "unimplemented extern relation: %s" name))
    with Util.Error.ExternError (at, msg) -> Run.Fail (at, msg)

  let eval_extern_func (name : string) (_typs : Typ.t list)
      (_values_input : Value.t list) : Run.func_result =
    try
      Run.Pass
        (match name with
        | _ ->
            error no_region
              (Format.asprintf "unimplemented extern function: %s" name))
    with Util.Error.ExternError (at, msg) -> Run.Fail (at, msg)

  (* State management *)

  let checkpoint () : int = 0
  let seff (before : int) (after : int) : bool = before <> after

  (* Clear the cache *)

  let clear () : unit = ()

  (* Cache management *)

  module Cache = struct
    let cache_on () = ()
    let cache_off () = ()
  end
end

(* The intermediate layer *)

module Make_parametric
    (Runner : Run.RUNNER)
    (Interface_SpecTec : INTERFACE_SPECTEC)
    () : Run.EXTERN = struct
  (* Mode initialization *)

  let init_mode _ = ()

  (* Caches
   * an interface cache for storing results of booting and unbooting values, types, and mixops *)

  type cache = { interface : Interface_SpecTec.cache }

  let cache : cache =
    let interface = Interface_SpecTec.make_cache () in
    { interface }

  module Cache = struct
    let cache_on () =
      Interface_SpecTec.cache_enable cache.interface

    let cache_off () =
      Interface_SpecTec.cache_disable_reset cache.interface
  end

  (* Threading extern calls to the runner *)

  let call_builtin_func (values_input : Value.t list) : Value.t list =
    let value_id, value_typs, value_values =
      match values_input with
      | [ value_id; value_typs; value_values ] ->
          (value_id, value_typs, value_values)
      | _ ->
          error_no_region "unexpected number of arguments to call_builtin_func"
    in
    Interface_SpecTec.push_cache cache.interface;
    let id = value_id |> Interface_SpecTec.unboot_id in
    let typs = value_typs |> Interface_SpecTec.unboot_typs in
    let values = value_values |> Interface_SpecTec.unboot_values in
    let value_output =
      match Runner.Interp.eval_func id.it typs values with
      | Pass value_output -> value_output
      | Fail (at, msg) -> error at msg
    in
    let value_value_output = Interface_SpecTec.boot_value value_output in
    let value_value_output_res =
      Value.Make.("OK val" <| [ value_value_output ] <<| "valres")
    in
    Interface_SpecTec.pop_cache ();
    [ value_value_output_res ]

  let call_extern_func (values_input : Value.t list) : Value.t list =
    let value_id, value_typs, value_values =
      match values_input with
      | [ value_id; value_typs; value_values ] ->
          (value_id, value_typs, value_values)
      | _ -> error_no_region "unexpected number of arguments to call_extern_rel"
    in
    Interface_SpecTec.push_cache cache.interface;
    let id = value_id |> Interface_SpecTec.unboot_id in
    let typs = value_typs |> Interface_SpecTec.unboot_typs in
    let values = value_values |> Interface_SpecTec.unboot_values in
    let value_output =
      match Runner.Interp.eval_func id.it typs values with
      | Pass value_output -> value_output
      | Fail (at, msg) -> error at msg
    in
    let value_value_output = Interface_SpecTec.boot_value value_output in
    let value_value_output_res =
      Value.Make.("OK val" <| [ value_value_output ] <<| "valsres")
    in
    Interface_SpecTec.pop_cache ();
    [ value_value_output_res ]

  let call_extern_rel (values_input : Value.t list) : Value.t list =
    let value_id, value_values =
      match values_input with
      | [ value_id; value_values ] -> (value_id, value_values)
      | _ -> error_no_region "unexpected number of arguments to call_extern_rel"
    in
    Interface_SpecTec.push_cache cache.interface;
    let id = value_id |> Interface_SpecTec.unboot_id in
    let values = value_values |> Interface_SpecTec.unboot_values in
    let values_output =
      match Runner.Interp.eval_rel id.it values with
      | Pass values_output -> values_output
      | Fail (at, msg) -> error at msg
    in
    let value_values_output = Interface_SpecTec.boot_values values_output in
    let value_values_output_res =
      Value.Make.("OK val*" <| [ value_values_output ] <<| "valsres")
    in
    Interface_SpecTec.pop_cache ();
    [ value_values_output_res ]

  (* Extern handlers *)

  let eval_extern_rel (name : string) (values_input : Value.t list) :
      Run.rel_result =
    try
      Run.Pass
        (match name with
        | "Call_builtin_func" -> call_builtin_func values_input
        | "Call_extern_func" -> call_extern_func values_input
        | "Call_extern_rel" -> call_extern_rel values_input
        | _ ->
            error no_region
              (Format.asprintf "unimplemented extern relation: %s" name))
    with Util.Error.ExternError (at, msg) -> Run.Fail (at, msg)

  let eval_extern_func (name : string) (_typs : Typ.t list)
      (_values_input : Value.t list) : Run.func_result =
    try
      Run.Pass
        (match name with
        | _ ->
            error no_region
              (Format.asprintf "unimplemented extern function: %s" name))
    with Util.Error.ExternError (at, msg) -> Run.Fail (at, msg)

  (* State management *)

  let checkpoint () : int = Runner.Interface.checkpoint ()

  let seff (before : int) (after : int) : bool =
    Runner.Interface.seff before after

  (* Clear the cache *)

  let clear_cache_interface () : unit =
    Interface_SpecTec.cache_clear cache.interface

  let clear () : unit =
    clear_cache_interface ()
end
