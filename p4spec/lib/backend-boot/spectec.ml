module Typ = Runtime.Type.Typ
module Value = Runtime.Value
module Cache = Runtime.Dynamic_Runner.Cache
module Run = Runtime.Dynamic_Runner.Signature
open Error
open Util.Source

(* The bottom layer *)

module Make_null (_ : Run.INTERP_IL) (_ : Run.INTERP_SL) : Run.EXTERN = struct
  (* Mode initialization *)

  let init_mode _ = ()

  (* Cache management *)

  let cache_find_func (values_input : Value.t list) : Value.t =
    let _value_id, _value_values_input =
      match values_input with
      | [ value_id; value_values_input ] -> (value_id, value_values_input)
      | _ -> error_no_region "unexpected number of arguments to cache_find_func"
    in
    Value.Make.("NONE" <| [] <<| "funccache")

  let cache_add_func_maybe (values_input : Value.t list) : Value.t =
    let _value_seff, _value_id, _value_values_input, _value_valres =
      match values_input with
      | [ value_seff; value_id; value_values_input; value_valres ] ->
          (value_seff, value_id, value_values_input, value_valres)
      | _ ->
          error_no_region
            "unexpected number of arguments to cache_add_func_maybe"
    in
    Value.Make.bool true

  let cache_find_rel (values_input : Value.t list) : Value.t =
    let _value_id, _value_values_input =
      match values_input with
      | [ value_id; value_values_input ] -> (value_id, value_values_input)
      | _ -> error_no_region "unexpected number of arguments to cache_find_rel"
    in
    Value.Make.("NONE" <| [] <<| "relcache")

  let cache_checkpoint (values_input : Value.t list) : Value.t =
    (match values_input with
    | [] -> ()
    | _ -> error_no_region "unexpected number of arguments to cache_checkpoint");
    Value.Make.extern (Typ.Make.var ("cachepoint" $ no_region) []) (`Int 42)

  let cache_add_rel_maybe (values_input : Value.t list) : Value.t =
    let _value_seff, _value_id, _value_values_input, _value_valsres =
      match values_input with
      | [ value_seff; value_id; value_values_input; value_valsres ] ->
          (value_seff, value_id, value_values_input, value_valsres)
      | _ ->
          error_no_region
            "unexpected number of arguments to cache_add_rel_maybe"
    in
    Value.Make.bool true

  let cache_seff (values_input : Value.t list) : Value.t =
    let _value_cachepoint_before, _value_cachepoint_after =
      match values_input with
      | [ value_cachepoint_before; value_cachepoint_after ] ->
          (value_cachepoint_before, value_cachepoint_after)
      | _ -> error_no_region "unexpected number of arguments to cache_seff"
    in
    Value.Make.bool false

  (* Externs *)

  let eval_extern_rel (name : string) (_values_input : Value.t list) :
      Run.rel_result =
    try
      error no_region (Format.asprintf "unimplemented extern relation: %s" name)
    with Util.Error.ExternError (at, msg) -> Run.Fail (at, msg)

  let eval_extern_func (name : string) (_typs : Typ.t list)
      (values_input : Value.t list) : Run.func_result =
    try
      Run.Pass
        (match name with
        | "cache_find_func" -> cache_find_func values_input
        | "cache_add_func_maybe" -> cache_add_func_maybe values_input
        | "cache_find_rel" -> cache_find_rel values_input
        | "cache_add_rel_maybe" -> cache_add_rel_maybe values_input
        | "cache_checkpoint" -> cache_checkpoint values_input
        | "cache_seff" -> cache_seff values_input
        | _ ->
            error no_region
              (Format.asprintf "unimplemented extern function: %s" name))
    with Util.Error.ExternError (at, msg) -> Run.Fail (at, msg)

  (* Clear the cache *)

  let clear () : unit = ()
end

(* The intermediate layer *)

module Make_parametric
    (Runner : Run.RUNNER)
    (_ : Run.INTERP_IL)
    (_ : Run.INTERP_SL) : Run.EXTERN = struct
  (* Mode initialization *)

  let init_mode _ = ()

  (* Threading extern calls to the runner *)

  let call_builtin_func (values_input : Value.t list) : Value.t list =
    let _value_ctx, value_id, value_typs, value_values =
      match values_input with
      | [ value_ctx; value_id; value_typs; value_values ] ->
          (value_ctx, value_id, value_typs, value_values)
      | _ ->
          error_no_region "unexpected number of arguments to call_builtin_func"
    in
    let id = value_id |> Interface.SpecTec.unboot_id in
    let typs = value_typs |> Interface.SpecTec.unboot_typs in
    let values = value_values |> Interface.SpecTec.unboot_values in
    let value_output =
      match Runner.Interp.eval_func id.it typs values with
      | Pass value_output -> value_output
      | Fail (at, msg) -> error at msg
    in
    let value_value_output = Interface.SpecTec.boot_value value_output in
    let value_value_output_res =
      Value.Make.("OK val" <| [ value_value_output ] <<| "valres")
    in
    [ value_value_output_res ]

  let call_extern_func (values_input : Value.t list) : Value.t list =
    let _value_ctx, value_id, value_typs, value_values =
      match values_input with
      | [ value_ctx; value_id; value_typs; value_values ] ->
          (value_ctx, value_id, value_typs, value_values)
      | _ -> error_no_region "unexpected number of arguments to call_extern_rel"
    in
    let id = value_id |> Interface.SpecTec.unboot_id in
    let typs = value_typs |> Interface.SpecTec.unboot_typs in
    let values = value_values |> Interface.SpecTec.unboot_values in
    let value_output =
      match Runner.Interp.eval_func id.it typs values with
      | Pass value_output -> value_output
      | Fail (at, msg) -> error at msg
    in
    let value_value_output = Interface.SpecTec.boot_value value_output in
    let value_value_output_res =
      Value.Make.("OK val" <| [ value_value_output ] <<| "valsres")
    in
    [ value_value_output_res ]

  let call_extern_rel (values_input : Value.t list) : Value.t list =
    let _value_ctx, value_id, value_values =
      match values_input with
      | [ value_ctx; value_id; value_values ] ->
          (value_ctx, value_id, value_values)
      | _ -> error_no_region "unexpected number of arguments to call_extern_rel"
    in
    let id = value_id |> Interface.SpecTec.unboot_id in
    let values = value_values |> Interface.SpecTec.unboot_values in
    let values_output =
      match Runner.Interp.eval_rel id.it values with
      | Pass values_output -> values_output
      | Fail (at, msg) -> error at msg
    in
    let value_values_output = Interface.SpecTec.boot_values values_output in
    let value_values_output_res =
      Value.Make.("OK val*" <| [ value_values_output ] <<| "valsres")
    in
    [ value_values_output_res ]

  (* Cache management *)

  let func_cache = ref (Cache.Cache.create ~size:10000)
  let rel_cache = ref (Cache.Cache.create ~size:10000)

  let cache_find_func (values_input : Value.t list) : Value.t =
    let value_id, value_values_input =
      match values_input with
      | [ value_id; value_values_input ] -> (value_id, value_values_input)
      | _ -> error_no_region "unexpected number of arguments to cache_find_func"
    in
    let id = value_id |> Interface.SpecTec.unboot_id in
    let cache_result =
      Cache.Cache.find !func_cache (id.it, [ value_values_input ])
    in
    match cache_result with
    | Some value_value_output ->
        Value.Make.("OK val" <| [ value_value_output ] <<| "funccache")
    | None -> Value.Make.("NONE" <| [] <<| "funccache")

  let cache_add_func_maybe (values_input : Value.t list) : Value.t =
    let value_seff, value_id, value_values_input, value_valres =
      match values_input with
      | [ value_seff; value_id; value_values_input; value_valres ] ->
          (value_seff, value_id, value_values_input, value_valres)
      | _ ->
          error_no_region
            "unexpected number of arguments to cache_add_func_maybe"
    in
    let seff = value_seff |> Value.Get.bool in
    (if not seff then
       match Value.Get.(value_valres |>>? "OK val") with
       | Some [ value_value_output ] ->
           let id = value_id |> Interface.SpecTec.unboot_id in
           Cache.Cache.add !func_cache
             (id.it, [ value_values_input ])
             value_value_output
       | _ -> ());
    Value.Make.bool true

  let cache_find_rel (values_input : Value.t list) : Value.t =
    let value_id, value_values_input =
      match values_input with
      | [ value_id; value_values_input ] -> (value_id, value_values_input)
      | _ -> error_no_region "unexpected number of arguments to cache_find_rel"
    in
    let id = value_id |> Interface.SpecTec.unboot_id in
    let cache_result =
      Cache.Cache.find !rel_cache (id.it, [ value_values_input ])
    in
    match cache_result with
    | Some value_values_output ->
        Value.Make.("OK val*" <| [ value_values_output ] <<| "relcache")
    | None -> Value.Make.("NONE" <| [] <<| "relcache")

  let cache_add_rel_maybe (values_input : Value.t list) : Value.t =
    let value_seff, value_id, value_values_input, value_valsres =
      match values_input with
      | [ value_seff; value_id; value_values_input; value_valsres ] ->
          (value_seff, value_id, value_values_input, value_valsres)
      | _ ->
          error_no_region
            "unexpected number of arguments to cache_add_rel_maybe"
    in
    let seff = value_seff |> Value.Get.bool in
    (if not seff then
       match Value.Get.(value_valsres |>>? "OK val*") with
       | Some [ value_values_output ] ->
           let id = value_id |> Interface.SpecTec.unboot_id in
           Cache.Cache.add !rel_cache
             (id.it, [ value_values_input ])
             value_values_output
       | _ -> ());
    Value.Make.bool true

  let cache_checkpoint (values_input : Value.t list) : Value.t =
    (match values_input with
    | [] -> ()
    | _ -> error_no_region "unexpected number of arguments to cache_checkpoint");
    let checkpoint = Runner.Interface.checkpoint () in
    Value.Make.extern
      (Typ.Make.var ("cachepoint" $ no_region) [])
      (`Int checkpoint)

  let cache_seff (values_input : Value.t list) : Value.t =
    let value_cachepoint_before, value_cachepoint_after =
      match values_input with
      | [ value_cachepoint_before; value_cachepoint_after ] ->
          (value_cachepoint_before, value_cachepoint_after)
      | _ -> error_no_region "unexpected number of arguments to cache_seff"
    in
    let cachepoint_before =
      value_cachepoint_before |> Value.Get.extern |> function
      | `Int i -> i
      | _ -> error_no_region "unexpected type for cachepoint_before"
    in
    let cachepoint_after =
      value_cachepoint_after |> Value.Get.extern |> function
      | `Int i -> i
      | _ -> error_no_region "unexpected type for cachepoint_after"
    in
    let seff = Runner.Interface.seff cachepoint_before cachepoint_after in
    Value.Make.bool seff

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
      (values_input : Value.t list) : Run.func_result =
    try
      Run.Pass
        (match name with
        | "cache_find_func" -> cache_find_func values_input
        | "cache_add_func_maybe" -> cache_add_func_maybe values_input
        | "cache_find_rel" -> cache_find_rel values_input
        | "cache_add_rel_maybe" -> cache_add_rel_maybe values_input
        | "cache_checkpoint" -> cache_checkpoint values_input
        | "cache_seff" -> cache_seff values_input
        | _ ->
            error no_region
              (Format.asprintf "unimplemented extern function: %s" name))
    with Util.Error.ExternError (at, msg) -> Run.Fail (at, msg)

  (* Clear the cache *)

  let clear () : unit =
    Cache.Cache.clear !func_cache;
    Cache.Cache.clear !rel_cache
end
