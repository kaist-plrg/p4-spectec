open Lang
open Interface.Wrap
open Interface.Unwrap
module Value = Runtime.Sim.Value
module IO = Runtime.Sim.Io
module Sim = Runtime.Sim.Simulator
open Error

module Make (Interp_IL : Sim.INTERP_IL) (Interp_SL : Sim.INTERP_SL) : Sim.ARCH =
struct
  (* Specification *)

  let spec : Sim.spec ref = ref (Sim.Empty : Sim.spec)
  let init_spec (spec_ : Sim.spec) : unit = spec := spec_

  (* Call entry points *)

  let call_rel (relname : string) (values_input : Value.t list) : Value.t list =
    match !spec with
    | IL spec_il -> (
        let rel_result_il = Interp_IL.eval_rel spec_il relname values_input in
        match rel_result_il with
        | Pass values_output -> values_output
        | Fail (at, msg) -> error at msg)
    | SL spec_sl -> (
        let rel_result_sl = Interp_SL.eval_rel spec_sl relname values_input in
        match rel_result_sl with
        | Pass values_output -> values_output
        | Fail (at, msg) -> error at msg)
    | Empty -> assert false

  let init_call_rel () = Spec.Rel.register call_rel

  let call_func (funcname : string) (typs_input : Sl.typ list)
      (values_input : Value.t list) : Value.t =
    match !spec with
    | IL spec_il -> (
        let func_result_il =
          Interp_IL.eval_func spec_il funcname typs_input values_input
        in
        match func_result_il with
        | Pass value_output -> value_output
        | Fail (at, msg) -> error at msg)
    | SL spec_sl -> (
        let func_result_sl =
          Interp_SL.eval_func spec_sl funcname typs_input values_input
        in
        match func_result_sl with
        | Pass value_output -> value_output
        | Fail (at, msg) -> error at msg)
    | Empty -> assert false

  let init_call_func () = Spec.Func.register call_func

  (* Extern calls *)

  let eval_extern_init (_values_input : Value.t list) : Value.t =
    wrap_extern_v "externState" `Null

  let eval_extern_func_lctk_call (values_input : Value.t list) : Value.t list =
    let value_ctx, value_name_func, value_names_param =
      match values_input with
      | [ value_ctx; value_name_func; value_names_param ] ->
          (value_ctx, value_name_func, value_names_param)
      | _ ->
          error_no_region
            "unexpected number of arguments to local compile-time known extern \
             function call"
    in
    let name_func = unwrap_text_v value_name_func in
    let names_param =
      value_names_param |> unwrap_list_v |> List.map unwrap_text_v
    in
    match (name_func, names_param) with
    | "static_assert", [ "check"; "message" ] ->
        [ Core.Func.static_assert ~message:true value_ctx ]
    | "static_assert", [ "check" ] ->
        [ Core.Func.static_assert ~message:false value_ctx ]
    | _ ->
        error_no_region
          ("unsupported local compile-time known extern function call: "
         ^ name_func ^ "("
          ^ String.concat ", " names_param
          ^ ")")

  let eval_extern_func_call (_values_input : Value.t list) : Value.t list =
    error_no_region
      "eval_extern_func_call not implemented for the placeholder simulator"

  let eval_extern_method_call (_values_input : Value.t list) : Value.t list =
    error_no_region
      "eval_extern_method_call not implemented for the placeholder simulator"

  (* Match-action table interface *)

  let table_add_entry (_value_sto : Value.t) (_value_tableName : Value.t)
      (_value_tableEntryPriorityInterface : Value.t)
      (_value_tableKeysetInterface : Value.t)
      (_value_tableActionInterface : Value.t) : Value.t =
    error_no_region
      "table_add_entry not implemented for the placeholder simulator"

  (* Initializer *)

  let init (spec_ : Sim.spec) : unit =
    init_spec spec_;
    init_call_rel ();
    init_call_func ()

  (* Pipeline initializer *)

  let init_pipe (_spec : Sim.spec) (_includes_p4 : string list)
      (_filename_p4 : string) : Value.t * Value.t =
    error_no_region "init_pipe not implemented for the placeholder simulator"

  (* Pipeline driver *)

  let drive_pipe (_value_ctx : Value.t) (_value_sto : Value.t) (_rx : IO.rx) :
      Value.t * Value.t * IO.tx option =
    error_no_region "drive_pipe not implemented for the placeholder simulator"
end
