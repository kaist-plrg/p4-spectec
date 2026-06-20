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
    (Interp_IL : Run.INTERP_IL)
    (Interp_SL : Run.INTERP_SL)
    (Interp_ML : Run.INTERP_ML) : Run.EXTERN = struct
  (* Mode initialization *)

  let call_func = ref (fun _ _ _ -> assert false)

  let init_mode mode_ =
    let call_func_ name typs values =
      (match mode_ with
      | Run.IL_mode -> Interp_IL.eval_func name typs values
      | Run.SL_mode -> Interp_SL.eval_func name typs values
      | Run.ML_mode -> Interp_ML.eval_func name typs values
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
   * a meta-cache for storing results of meta-relation and meta-meta-function calls
   * an interface cache for storing results of booting and unbooting values, types, and mixops *)

  type cache_meta = {
    mutable enabled : bool;
    func : Value.t CCache.t;
    rel : Value.t CCache.t;
  }

  type cache = { meta : cache_meta; interface : Interface_SpecTec.cache }

  let cache : cache =
    let meta : cache_meta =
      {
        enabled = true;
        func = CCache.create ~size:(256 * 1024);
        rel = CCache.create ~size:(256 * 1024);
      }
    in
    let interface = Interface_SpecTec.make_cache () in
    { meta; interface }

  module Cache = struct
    let cache_on () =
      cache.meta.enabled <- true;
      Interface_SpecTec.cache_enable cache.interface

    let cache_off () =
      cache.meta.enabled <- false;
      CCache.reset cache.meta.func;
      CCache.reset cache.meta.rel;
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

  (* Meta-cache management *)

  let cache_find_func (values_input : Value.t list) : Value.t =
    if not cache.meta.enabled then Value.Make.("NONE" <| [] <<| "funccache")
    else
      let value_id, value_values_input =
        match values_input with
        | [ value_id; value_values_input ] -> (value_id, value_values_input)
        | _ ->
            error_no_region "unexpected number of arguments to cache_find_func"
      in
      let id = value_id |> Interface_SpecTec.unboot_id in
      let cache_result =
        CCache.find cache.meta.func (id.it, [ value_values_input ])
      in
      match cache_result with
      | Some value_value_output ->
          Value.Make.("OK val" <| [ value_value_output ] <<| "funccache")
      | None -> Value.Make.("NONE" <| [] <<| "funccache")

  let cache_add_func_maybe (values_input : Value.t list) : Value.t =
    if not cache.meta.enabled then Value.Make.bool true
    else
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
             let id = value_id |> Interface_SpecTec.unboot_id in
             CCache.add cache.meta.func
               (id.it, [ value_values_input ])
               value_value_output
         | _ -> ());
      Value.Make.bool true

  let cache_find_rel (values_input : Value.t list) : Value.t =
    if not cache.meta.enabled then Value.Make.("NONE" <| [] <<| "relcache")
    else
      let value_id, value_values_input =
        match values_input with
        | [ value_id; value_values_input ] -> (value_id, value_values_input)
        | _ ->
            error_no_region "unexpected number of arguments to cache_find_rel"
      in
      let id = value_id |> Interface_SpecTec.unboot_id in
      let cache_result =
        CCache.find cache.meta.rel (id.it, [ value_values_input ])
      in
      match cache_result with
      | Some value_values_output ->
          Value.Make.("OK val*" <| [ value_values_output ] <<| "relcache")
      | None -> Value.Make.("NONE" <| [] <<| "relcache")

  let cache_add_rel_maybe (values_input : Value.t list) : Value.t =
    if not cache.meta.enabled then Value.Make.bool true
    else
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
             let id = value_id |> Interface_SpecTec.unboot_id in
             CCache.add cache.meta.rel
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

  (* State management *)

  let checkpoint () : int = Runner.Interface.checkpoint ()

  let seff (before : int) (after : int) : bool =
    Runner.Interface.seff before after

  (* Clear the cache *)

  let clear_cache_interface () : unit =
    Interface_SpecTec.cache_clear cache.interface

  let clear () : unit =
    clear_cache_interface ();
    CCache.clear cache.meta.func;
    CCache.clear cache.meta.rel
end
