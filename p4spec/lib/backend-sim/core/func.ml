module Value = Runtime.Sim.Value
open Interface.Wrap
open Interface.Unpack
open Error

(* Check a predicate @check in the parser; if the predicate is true do nothing,
   otherwise set the parser error to @toSignal, and transition to the `reject` state.

   extern void verify(in bool check, in error toSignal); *)

let verify (value_ctx : Value.t) (value_arch : Value.t) :
    Value.t * Value.t * Value.t =
  (* Get "check" in context *)
  let value_check = Spec.Func.find_var_e_local value_ctx "check" in
  (* Get "toSignal" in context *)
  let value_toSignal = Spec.Func.find_var_e_local value_ctx "toSignal" in
  (* If check, return and otherwise reject *)
  let check = value_check |> unpack_p4_bool in
  let value_callResult =
    if check then
      let value_eps = wrap_opt_v "value" None in
      [ Term "RETURN"; NT value_eps ] #@ "returnResult"
    else [ Term "REJECT"; NT value_toSignal ] #@ "rejectTransitionResult"
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

let static_assert ~(message : bool) (value_ctx : Value.t) : Value.t =
  (* Get "check" in context *)
  let value_check = Spec.Func.find_var_value_t_local value_ctx "check" in
  (* Get "message" in context if present *)
  let value_message =
    if message then Some (Spec.Func.find_var_value_t_local value_ctx "message")
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
