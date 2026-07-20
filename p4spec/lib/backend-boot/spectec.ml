module Typ = Runtime.Type.Typ
module Value = Runtime.Value
module CCache = Runtime.Dynamic.Caches.CallCache
module Run = Runtime.Dynamic_Runner.Signature
open Error
open Util.Source

(* [Il.typ] witnesses for the cache-shell types, used with the generic
   [V.Make.( <<| )] (which needs a real [Il.typ], unlike [Value.Make]'s own
   [( <<| )] convenience overload that takes the type name as a string). *)

let typ_funccache = Typ.Make.var ("funccache" $ no_region) []
let typ_relcache = Typ.Make.var ("relcache" $ no_region) []

(* [valres]/[valsres] (= [res<val>]/[res<val*>], FINDINGS.md §2c) dispatch
   through the generic ["res"] [parametric_heads] arm — keyed on the head
   name, the same way [maps.ml] keys map/pair/set lookups on "map"/"pair",
   never on a specific instantiation's alias. Both aliases share one [Il.typ]
   witness for this reason: the parametric arm is payload-erased, so it
   doesn't (can't) distinguish "valres" from "valsres" — only the actual
   argument list passed to [V.Make]/[V.Get] differs. *)

let typ_res = Typ.Make.var ("res" $ no_region) []
let typ_valres = typ_res
let typ_valsres = typ_res
let typ_val = Typ.Make.var ("val" $ no_region) []
let typ_val_star = Typ.Make.list typ_val
let mixop_ok_val = Value.Mixops.of_string "OK val"
let mixop_ok_vals = Value.Mixops.of_string "OK val*"

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

  (* Boot / unboots. [vt] is the concrete representation ([V.t]) the
     module implementing this interface was itself built over — [boot_*]/
     [unboot_*] operate on it directly instead of through the [Value.t]
     erasure carrier both sides would otherwise silently agree to
     disagree on. A [Make_null]/[Make_parametric] instantiated with some
     [V] now has to be paired with an [Interface_SpecTec] whose [vt = V.t]
     (enforced with [INTERFACE_SPECTEC with type vt = V.t] at the functor
     signature) — a mismatch is a type error instead of a silent
     representation-confusion bug at runtime. *)

  type vt

  val boot_value : Value.t -> vt
  val boot_values : Value.t list -> vt
  val unboot_id : vt -> string phrase
  val unboot_typs : vt -> Typ.t list
  val unboot_values : vt -> Value.t list

  (* [call_builtin], fixed at [Valrep.V_value] regardless of the module's
     own [vt]. For values that already are real [Value.t] (e.g. the
     output of [unboot_values]), [call_builtin] would recast them to
     [vt] with no actual conversion (a bare relabel when [vt <> Value.t])
     before handing them to a [vt]-typed builtin — this doesn't recast at
     all, since [Valrep.V_value.t = Value.t]. *)

  val call_builtin_value :
    (Value.t -> unit) ->
    Domain.Lib.Id.t ->
    Typ.t list ->
    Value.t list ->
    Value.t
end

(* The null layer *)

module Make_null
    (V : Runtime.Valrep.VAL)
    (Interface_SpecTec : INTERFACE_SPECTEC with type vt = V.t)
    (Interp_IL : Run.INTERP_IL)
    (Interp_SL : Run.INTERP_SL)
    (Interp_ML : Run.INTERP_ML) : Run.EXTERN = struct
  (* Mode initialization: nothing left to do — [V]/[Interface_SpecTec]
     already fix the representation at construction (see [build.ml]). *)

  let init_mode (_ : Run.mode) : unit = ()

  (* Threading extern calls to the interpreter.

     [Call_builtin_func] resolves a *builtin* (a `builtin dec` in the
     target script, e.g. $find_map/$add_map — FINDINGS.md §2a). Its
     arguments already went through [unboot_values], which produces real
     [Value.t] (confirmed: it dispatches via spec-meta's own declared
     "val" type, which decodes correctly). [Interface_SpecTec.call_builtin]
     is the wrong thing to hand them to here — it exists for spec-meta's
     own internal calls (e.g. [ctx.iface.call_builtin]'s venv/funcdef
     lookups), where the values genuinely are already native, and it
     unconditionally [V.of_value]-recasts its arguments to native before
     invoking the builtin. For values that are already real [Value.t],
     that recast doesn't convert anything — it just relabels a [Value.t]
     block as if it were [V]-shaped, and the builtin (instantiated at
     [V]) then misreads it (FINDINGS.md §2b, confirmed at the exact
     generated line: [case_of_typed]'s single-constructor arms).
     [call_builtin_value] is fixed at [Valrep.V_value] regardless of this
     functor's own [V], so passing already-real [Value.t] through it is a
     no-op cast ([V_value.of_value = Fun.id]), not a relabel.

     The result crosses back the other way: [call_builtin_value] returns
     a real, interpreted [Value.t], but the "valres" shell handed back
     across [Run.EXTERN] must be genuinely [V]-shaped, since compiled
     code [Obj.magic]s it straight into its native `` `OK_X `` / `` `FAIL
     `` on the other side (no dispatch, no conversion). [boot_value]
     deep-converts the payload from [Value.t] into [vt]; [V.Make] then
     builds the "valres" shell itself through the generic ["res"]
     [parametric_heads] arm (FINDINGS.md §2c — now covered) instead of
     [Value.Make], which would build the same wrong-for-this-boundary
     interpreted shape [cache_find_func] et al. were already fixed to
     avoid. *)

  let call_builtin_func (values_input : Value.t list) : Value.t list =
    let value_id, value_typs, value_values =
      match values_input with
      | [ value_id; value_typs; value_values ] ->
          (value_id, value_typs, value_values)
      | _ ->
          error_no_region "unexpected number of arguments to call_builtin_func"
    in
    let id = value_id |> V.of_value |> Interface_SpecTec.unboot_id in
    let typs = value_typs |> V.of_value |> Interface_SpecTec.unboot_typs in
    let values =
      value_values |> V.of_value |> Interface_SpecTec.unboot_values
    in
    let value_output =
      Interface_SpecTec.call_builtin_value (fun _ -> ()) id typs values
    in
    let vt_output = Interface_SpecTec.boot_value value_output in
    let value_output_res =
      V.Make.("OK val" <| [ vt_output ] <<| typ_valres) |> V.to_value
    in
    [ value_output_res ]

  (* Cache management.

     These constants cross the [Run.EXTERN] boundary, whose wire type is
     hardcoded to [Value.t] by the compiled-spec template regardless of
     mode (see FINDINGS.md §4). Building them through [V.Make] and handing
     them across with [V.to_value] — the same [SAFE]/[UNSAFE] boundary
     [interface.ml]'s [call_builtin]/[boot_value] already cross — makes
     the functor produce a genuinely [V]-shaped result instead of always
     an interpreted [Value.t] that [ML_mode] compiled code then
     [Obj.magic]s into its native poly-variant type and misreads. *)

  let cache_find_func (values_input : Value.t list) : Value.t =
    let _value_id, _value_values_input =
      match values_input with
      | [ value_id; value_values_input ] -> (value_id, value_values_input)
      | _ -> error_no_region "unexpected number of arguments to cache_find_func"
    in
    V.Make.("NONE" <| [] <<| typ_funccache) |> V.to_value

  let cache_add_func_maybe (values_input : Value.t list) : Value.t =
    let _value_seff, _value_id, _value_values_input, _value_valres =
      match values_input with
      | [ value_seff; value_id; value_values_input; value_valres ] ->
          (value_seff, value_id, value_values_input, value_valres)
      | _ ->
          error_no_region
            "unexpected number of arguments to cache_add_func_maybe"
    in
    V.Make.bool true |> V.to_value

  let cache_find_rel (values_input : Value.t list) : Value.t =
    let _value_id, _value_values_input =
      match values_input with
      | [ value_id; value_values_input ] -> (value_id, value_values_input)
      | _ -> error_no_region "unexpected number of arguments to cache_find_rel"
    in
    V.Make.("NONE" <| [] <<| typ_relcache) |> V.to_value

  let cache_checkpoint (values_input : Value.t list) : Value.t =
    (match values_input with
    | [] -> ()
    | _ -> error_no_region "unexpected number of arguments to cache_checkpoint");
    V.Make.extern (Typ.Make.var ("cachepoint" $ no_region) []) (`Int 42)
    |> V.to_value

  let cache_add_rel_maybe (values_input : Value.t list) : Value.t =
    let _value_seff, _value_id, _value_values_input, _value_valsres =
      match values_input with
      | [ value_seff; value_id; value_values_input; value_valsres ] ->
          (value_seff, value_id, value_values_input, value_valsres)
      | _ ->
          error_no_region
            "unexpected number of arguments to cache_add_rel_maybe"
    in
    V.Make.bool true |> V.to_value

  let cache_seff (values_input : Value.t list) : Value.t =
    let _value_cachepoint_before, _value_cachepoint_after =
      match values_input with
      | [ value_cachepoint_before; value_cachepoint_after ] ->
          (value_cachepoint_before, value_cachepoint_after)
      | _ -> error_no_region "unexpected number of arguments to cache_seff"
    in
    V.Make.bool false |> V.to_value

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
    (V : Runtime.Valrep.VAL)
    (Runner : Run.RUNNER)
    (Interface_SpecTec : INTERFACE_SPECTEC with type vt = V.t)
    () : Run.EXTERN = struct
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

  (* Mode initialization. Called once per level, right after this level's
     own [Interp_<mode>.init] has already applied the [-no-cache] flag's
     initial on/off state (see [runner/make.ml]'s [init]) - so this runs
     last and wins. Under [ML_mode] the meta-cache's key/value marshal
     (see [marshal_val_star] below) costs more than just redoing the call:
     every hit still walks the full native argument tree into a fresh real
     [Value.t] to hash it, on every single meta-function/meta-relation
     call the compiled interpreter makes. IL/SL modes pay nothing for that
     marshal (identity under [V_value]), so they keep the cache. *)

  let init_mode (mode : Run.mode) : unit =
    match mode with Run.ML_mode -> Cache.cache_off () | _ -> ()

  (* Threading extern calls to the runner.

     [Call_builtin_func] resolves a *builtin* (a `builtin dec` in the
     target script — FINDINGS.md §2a), not a spec-defined function — it
     doesn't belong on [Runner.Interp.eval_func] (that's for *defined*
     functions/relations, calling *up* the tower).

     Always goes through [Runner.Interface.call_builtin] — [Runner_above],
     the adjacent level below — never this level's own [Interface_SpecTec].
     This works today because: (a) the generic builtins spec-meta's own
     interpreter needs internally (`$add_map`/`$find_map`/etc., used e.g.
     by `$load`) are part of the base [Funcs] table in every level's
     [Builtins], regardless of [Ext] (FINDINGS.md §2a); and (b) a builtin
     declared by a script loaded *as data* into this level (spec's own
     `$print_<X>`) is only ever implemented as an interface extension on
     the *target* level (P4's `print_` — [Interface.P4]'s
     [Builtin_P4_Ext]), which [Runner_above] reaches directly in a
     two-level tower.

     Known gap: this is one hop to [Runner_above], not a transitive relay
     down the whole tower — [Interface.call_builtin] has no fallback of
     its own. For a tower with 2+ meta-levels above target, an extension
     that only exists on target (not on the level immediately below the
     caller) is still unreachable. Only correct today for towers where
     [Runner_above] is the target directly. Not otherwise fixed — needs a
     real multi-hop design, not a patch here.

     [Runner.Interface.call_builtin]'s args are already real [Value.t]
     (from [unboot_values]); the result shell ("valres") is built through
     [V.Make] over a [boot_value]-converted, genuinely [vt]-shaped
     payload — not [Value.Make] over the raw result — since compiled code
     [Obj.magic]s this shell straight into its native `` `OK_X `` /
     `` `FAIL `` with no dispatch (FINDINGS.md §2c; see
     [Make_null.call_builtin_func]'s longer comment). *)

  let call_builtin_func (values_input : Value.t list) : Value.t list =
    let value_id, value_typs, value_values =
      match values_input with
      | [ value_id; value_typs; value_values ] ->
          (value_id, value_typs, value_values)
      | _ ->
          error_no_region "unexpected number of arguments to call_builtin_func"
    in
    Interface_SpecTec.push_cache cache.interface;
    let id = value_id |> V.of_value |> Interface_SpecTec.unboot_id in
    let typs = value_typs |> V.of_value |> Interface_SpecTec.unboot_typs in
    let values =
      value_values |> V.of_value |> Interface_SpecTec.unboot_values
    in
    let value_output =
      Runner.Interface.call_builtin (fun _ -> ()) id typs values
    in
    let vt_output = Interface_SpecTec.boot_value value_output in
    let value_output_res =
      V.Make.("OK val" <| [ vt_output ] <<| typ_valres) |> V.to_value
    in
    Interface_SpecTec.pop_cache ();
    [ value_output_res ]

  let call_extern_func (values_input : Value.t list) : Value.t list =
    let value_id, value_typs, value_values =
      match values_input with
      | [ value_id; value_typs; value_values ] ->
          (value_id, value_typs, value_values)
      | _ -> error_no_region "unexpected number of arguments to call_extern_rel"
    in
    Interface_SpecTec.push_cache cache.interface;
    let id = value_id |> V.of_value |> Interface_SpecTec.unboot_id in
    let typs = value_typs |> V.of_value |> Interface_SpecTec.unboot_typs in
    let values =
      value_values |> V.of_value |> Interface_SpecTec.unboot_values
    in
    let value_output =
      match Runner.Interp.eval_func id.it typs values with
      | Pass value_output -> value_output
      | Fail (at, msg) -> error at msg
    in
    let vt_output = Interface_SpecTec.boot_value value_output in
    let value_value_output_res =
      V.Make.("OK val" <| [ vt_output ] <<| typ_valsres) |> V.to_value
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
    let id = value_id |> V.of_value |> Interface_SpecTec.unboot_id in
    let values =
      value_values |> V.of_value |> Interface_SpecTec.unboot_values
    in
    let values_output =
      match Runner.Interp.eval_rel id.it values with
      | Pass values_output -> values_output
      | Fail (at, msg) -> error at msg
    in
    let vt_output = Interface_SpecTec.boot_values values_output in
    let value_values_output_res =
      V.Make.("OK val*" <| [ vt_output ] <<| typ_valsres) |> V.to_value
    in
    Interface_SpecTec.pop_cache ();
    [ value_values_output_res ]

  (* Meta-cache management.

     Wrapper shells ([funccache]/[relcache]/[bool]/[extern cachepoint]) are
     built through [V.Make]/[V.to_value] so [ML_mode] gets a genuinely
     native-shaped result instead of an interpreted [Value.t] that compiled
     code then misreads (see [Make_null] above and FINDINGS.md §4).

     The [value_valres]/[value_valsres] payload *extraction* below is a
     different, still-open problem: [valres]/[valsres] are the parametric
     type [res<X>] instantiated at [val]/[val*], and [V_native]'s generated
     [case_of_typed] has no entry for parametric heads outside
     [set;pair;map] (FINDINGS.md §2c — extending that is a shared-codegen
     change, not something to improvise here). Under [V_native] the
     [V.Get.( |>>? )] below therefore raises; since this cache is a pure
     optimization (a miss just means recomputation, not a wrong answer),
     that failure is caught and treated as "nothing to cache" rather than
     propagated. *)

  (* [CCache] always stores genuine [Value.t] (it's shared, mode-agnostic
     state, hashed/compared structurally) — but [value_values_input] arrives
     here having already crossed the [Run.EXTERN] wire, which under
     [ML_mode]/[V_native] is [Obj.magic], not a real conversion (compiled
     code casts its native [val*] straight across, see [part_001.ml]'s
     [f__cache_find_func]). Using it as a [Hashtbl] key/value without first
     marshaling it treats a native block as if it had [Value.t]'s layout,
     and [Hashtbl.hash] walks off into garbage.

     [val*] is a list type, and [V.marshal]/[V.unmarshal] only dispatch on
     named types (the generated [marshal_typed] has no [IterT] arm), so the
     whole-list shell has to be built/read by hand around a per-element
     [V.marshal typ_val] — identity under [V_value], a real fix only where
     [V_native] needs it. *)

  let marshal_val_star (v : V.t) : Value.t =
    v |> V.Get.list |> List.map (V.marshal typ_val) |> Value.Make.list typ_val

  let unmarshal_val_star (v : Value.t) : V.t =
    v |> Value.Get.list |> List.map (V.unmarshal typ_val) |> V.Make.list typ_val

  let cache_find_func (values_input : Value.t list) : Value.t =
    if not cache.meta.enabled then
      V.Make.("NONE" <| [] <<| typ_funccache) |> V.to_value
    else
      let value_id, value_values_input =
        match values_input with
        | [ value_id; value_values_input ] -> (value_id, value_values_input)
        | _ ->
            error_no_region "unexpected number of arguments to cache_find_func"
      in
      let id = value_id |> V.of_value |> Interface_SpecTec.unboot_id in
      let value_values_input = value_values_input |> V.of_value |> marshal_val_star in
      let cache_result =
        CCache.find cache.meta.func (id.it, [ value_values_input ])
      in
      (match cache_result with
      | Some value_value_output ->
          V.Make.(
            "OK val"
            <| [ value_value_output |> V.unmarshal typ_val ]
            <<| typ_funccache)
      | None -> V.Make.("NONE" <| [] <<| typ_funccache))
      |> V.to_value

  let cache_add_func_maybe (values_input : Value.t list) : Value.t =
    if not cache.meta.enabled then V.Make.bool true |> V.to_value
    else
      let value_seff, value_id, value_values_input, value_valres =
        match values_input with
        | [ value_seff; value_id; value_values_input; value_valres ] ->
            (value_seff, value_id, value_values_input, value_valres)
        | _ ->
            error_no_region
              "unexpected number of arguments to cache_add_func_maybe"
      in
      let seff = value_seff |> V.of_value |> V.Get.bool in
      (if not seff then
         try
           match
             V.Get.(value_valres |> V.of_value |>>? (mixop_ok_val, typ_valres))
           with
           | Some [ value_value_output ] ->
               let id = value_id |> V.of_value |> Interface_SpecTec.unboot_id in
               let value_values_input =
                 value_values_input |> V.of_value |> marshal_val_star
               in
               CCache.add cache.meta.func
                 (id.it, [ value_values_input ])
                 (value_value_output |> V.marshal typ_val)
           | _ -> ()
         with Failure _ -> ());
      V.Make.bool true |> V.to_value

  let cache_find_rel (values_input : Value.t list) : Value.t =
    if not cache.meta.enabled then
      V.Make.("NONE" <| [] <<| typ_relcache) |> V.to_value
    else
      let value_id, value_values_input =
        match values_input with
        | [ value_id; value_values_input ] -> (value_id, value_values_input)
        | _ ->
            error_no_region "unexpected number of arguments to cache_find_rel"
      in
      let id = value_id |> V.of_value |> Interface_SpecTec.unboot_id in
      let value_values_input = value_values_input |> V.of_value |> marshal_val_star in
      let cache_result =
        CCache.find cache.meta.rel (id.it, [ value_values_input ])
      in
      (match cache_result with
      | Some value_values_output ->
          V.Make.(
            "OK val*"
            <| [ value_values_output |> unmarshal_val_star ]
            <<| typ_relcache)
      | None -> V.Make.("NONE" <| [] <<| typ_relcache))
      |> V.to_value

  let cache_add_rel_maybe (values_input : Value.t list) : Value.t =
    if not cache.meta.enabled then V.Make.bool true |> V.to_value
    else
      let value_seff, value_id, value_values_input, value_valsres =
        match values_input with
        | [ value_seff; value_id; value_values_input; value_valsres ] ->
            (value_seff, value_id, value_values_input, value_valsres)
        | _ ->
            error_no_region
              "unexpected number of arguments to cache_add_rel_maybe"
      in
      let seff = value_seff |> V.of_value |> V.Get.bool in
      (if not seff then
         try
           match
             V.Get.(
               value_valsres |> V.of_value |>>? (mixop_ok_vals, typ_valsres))
           with
           | Some [ value_values_output ] ->
               let id = value_id |> V.of_value |> Interface_SpecTec.unboot_id in
               let value_values_input =
                 value_values_input |> V.of_value |> marshal_val_star
               in
               CCache.add cache.meta.rel
                 (id.it, [ value_values_input ])
                 (value_values_output |> marshal_val_star)
           | _ -> ()
         with Failure _ -> ());
      V.Make.bool true |> V.to_value

  let cache_checkpoint (values_input : Value.t list) : Value.t =
    (match values_input with
    | [] -> ()
    | _ -> error_no_region "unexpected number of arguments to cache_checkpoint");
    let checkpoint = Runner.Interface.checkpoint () in
    V.Make.extern (Typ.Make.var ("cachepoint" $ no_region) []) (`Int checkpoint)
    |> V.to_value

  let cache_seff (values_input : Value.t list) : Value.t =
    let value_cachepoint_before, value_cachepoint_after =
      match values_input with
      | [ value_cachepoint_before; value_cachepoint_after ] ->
          (value_cachepoint_before, value_cachepoint_after)
      | _ -> error_no_region "unexpected number of arguments to cache_seff"
    in
    let cachepoint_before =
      value_cachepoint_before |> V.of_value |> V.Get.extern |> function
      | `Int i -> i
      | _ -> error_no_region "unexpected type for cachepoint_before"
    in
    let cachepoint_after =
      value_cachepoint_after |> V.of_value |> V.Get.extern |> function
      | `Int i -> i
      | _ -> error_no_region "unexpected type for cachepoint_after"
    in
    let seff = Runner.Interface.seff cachepoint_before cachepoint_after in
    V.Make.bool seff |> V.to_value

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
