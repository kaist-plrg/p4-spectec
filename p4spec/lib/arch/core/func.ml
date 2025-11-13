module Value = Runtime_dynamic.Value
open Interface.Wrap
open Interface.Unpack

(* Check a predicate @check in the parser; if the predicate is true do nothing,
   otherwise set the parser error to @toSignal, and transition to the `reject` state.

   extern void verify(in bool check, in error toSignal); *)
let verify (value_ctx : Value.t) (value_sto : Value.t) :
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
  (value_ctx, value_sto, value_callResult)
