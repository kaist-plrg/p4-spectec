module Typ = Runtime.Type.Typ
open Error
open Util.Source

module Make (V : Valrep.VAL) (Spec_Func : Spec.Func.S with type vt = V.t) =
struct
  module Unpack = Spec.Unpack.Make (V)
  open Unpack
  (* Check a predicate @check in the parser; if the predicate is true do nothing,
     otherwise set the parser error to @toSignal, and transition to the `reject` state.

     extern void verify(in bool check, in error toSignal); *)

  let verify (value_ctx : V.t) (value_arch : V.t) : V.t * V.t * V.t =
    (* Get "check" in context *)
    let value_check = Spec_Func.find_var_e_local value_ctx "check" in
    (* Get "toSignal" in context *)
    let value_toSignal = Spec_Func.find_var_e_local value_ctx "toSignal" in
    (* If check, return and otherwise reject *)
    let check = value_check |> unpack_p4_bool in
    (* verify's result is a [callResult] (= abortResult | returnResult); name that
       type, not a leaf. The typename only matters under [V_typed], where
       [make_case_typed] builds the typed variant from it; [V_value] ignores it
       (mixop-driven). "rejectResult" was a stale name that never existed as a
       type — harmless under [V_value], a hard failure under [V_typed]. *)
    let value_callResult =
      if check then
        let typ = Typ.Make.var ("value" $ no_region) [] |> Typ.Make.opt in
        let value_eps = V.Make.opt typ None in
        V.Make.("RETURN value?" <| [ value_eps ] <<| "callResult")
      else V.Make.("REJECT errorValue" <| [ value_toSignal ] <<| "callResult")
    in
    (value_ctx, value_arch, value_callResult)

  (* Static assert evaluates a boolean expression
     at compilation time.  If the expression evaluates to
     false, compilation is stopped and the corresponding message is printed.
     The function returns a boolean, so that it can be used
     as a global constant value in a program, e.g.:
       const bool _check = static_assert(V1MODEL_VERSION > 20180000, "Expected a v1 model version >= 20180000");

     extern bool static_assert(bool check, string message);

     Like the above but using a default message.

     extern bool static_assert(bool check); *)

  let static_assert ~(message : bool) (value_ctx : V.t) : V.t =
    (* Get "check" in context *)
    let value_check = Spec_Func.find_var_value_t_local value_ctx "check" in
    (* Get "message" in context if present *)
    let value_message =
      if message then
        Some (Spec_Func.find_var_value_t_local value_ctx "message")
      else None
    in
    (* If check, return true and otherwise fail *)
    let check = value_check |> unpack_p4_bool in
    if check then value_check
    else
      let message =
        match value_message with
        | Some v -> unpack_p4_string v
        | None -> "static_assert failed"
      in
      error_no_region message
end
