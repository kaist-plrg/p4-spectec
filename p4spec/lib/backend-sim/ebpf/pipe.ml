open Lang
open Interface.Wrap
open Interface.Unwrap
open Interface.Unpack
open Interface.Flatten
module Value = Runtime.Sim.Value
module IO = Runtime.Sim.Io
module Sim = Runtime.Sim.Simulator
open Error

module Make (Interp_IL : Sim.INTERP_IL) (Interp_SL : Sim.INTERP_SL) : Sim.ARCH =
struct
  (* STF AST transformation *)

  let transform_stf_stmt (stmt : Stf.Ast.stmt) : Stf.Ast.stmt =
    let transform_name name =
      Stf.Transform.Name.(
        name
        |> replace_substring ~substrings:[ "pipe_c1_" ]
             ~replacement:"main.filt.c1."
        |> replace_substring ~substrings:[ "pipe_" ] ~replacement:"main.filt."
        |> replace_substring ~substrings:[ "pipe" ] ~replacement:"main.filt")
    in
    let transform_action (name, args) =
      Stf.Transform.Action.(
        let name =
          name
          |> replace_substring ~substrings:[ "pipe_c1_" ]
               ~replacement:"main.filt.c1."
          |> replace_substring ~substrings:[ "pipe_" ] ~replacement:"main.filt."
          |> replace_substring ~substrings:[ "_NoAction" ]
               ~replacement:"NoAction"
        in
        into_unqualified (name, args))
    in
    match stmt with
    | Add (name, priority_opt, mtches, action, id_opt) ->
        let name = transform_name name in
        let action = transform_action action in
        Add (name, priority_opt, mtches, action, id_opt)
    | SetDefault (name, action) ->
        let name = transform_name name in
        let action = transform_action action in
        SetDefault (name, action)
    | _ -> stmt

  (* Mode *)

  let mode : Sim.mode ref = ref (Sim.Empty_mode : Sim.mode)
  let init_mode (mode_ : Sim.mode) : unit = mode := mode_

  (* Call entry points *)

  let call_rel (relname : string) (values_input : Value.t list) : Value.t list =
    match !mode with
    | IL_mode -> (
        let rel_result_il = Interp_IL.eval_rel relname values_input in
        match rel_result_il with
        | Pass values_output -> values_output
        | Fail (at, msg) -> error at msg)
    | SL_mode -> (
        let rel_result_sl = Interp_SL.eval_rel relname values_input in
        match rel_result_sl with
        | Pass values_output -> values_output
        | Fail (at, msg) -> error at msg)
    | Empty_mode -> assert false

  let init_call_rel () = Spec.Rel.register call_rel

  let call_func (funcname : string) (typs_input : Sl.typ list)
      (values_input : Value.t list) : Value.t =
    match !mode with
    | IL_mode -> (
        let func_result_il =
          Interp_IL.eval_func funcname typs_input values_input
        in
        match func_result_il with
        | Pass value_output -> value_output
        | Fail (at, msg) -> error at msg)
    | SL_mode -> (
        let func_result_sl =
          Interp_SL.eval_func funcname typs_input values_input
        in
        match func_result_sl with
        | Pass value_output -> value_output
        | Fail (at, msg) -> error at msg)
    | Empty_mode -> assert false

  let init_call_func () = Spec.Func.register call_func

  (* Extern objects *)

  type arch_state = unit [@@deriving yojson]

  let init_arch_state = () |> arch_state_to_yojson |> wrap_extern_v "archState"

  type extern =
    | PacketIn of Core.Object.PacketIn.t
    | CounterArray of Object.CounterArray.t
  [@@deriving yojson]

  let get_extern (value_arch : Value.t) (value_oid : Value.t) : extern =
    Spec.Func.find_objectState_e value_arch value_oid
    |> unwrap_extern_v |> extern_of_yojson |> Result.get_ok

  (* Extern calls *)

  let eval_extern_init (values_input : Value.t list) : Value.t =
    let value_name_extern, value_type_args, value_args =
      match values_input with
      | [ value_name; value_type_args; value_args ] ->
          (value_name, value_type_args, value_args)
      | _ -> error_no_region "unexpected number of arguments to extern init"
    in
    let name_extern = unwrap_text_v value_name_extern in
    match name_extern with
    | "CounterArray" ->
        let counter_array =
          Object.CounterArray.init value_type_args value_args
        in
        let counter_array = CounterArray counter_array in
        counter_array |> extern_to_yojson |> wrap_extern_v "objectState"
    | _ -> wrap_extern_v "objectState" `Null

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

  let eval_extern_func_call (values_input : Value.t list) : Value.t list =
    let value_ctx, value_arch, value_name_func, value_names_param =
      match values_input with
      | [ value_ctx; value_arch; value_name_func; value_names_param ] ->
          (value_ctx, value_arch, value_name_func, value_names_param)
      | _ ->
          error_no_region
            "unexpected number of arguments to extern function call"
    in
    let name_func = unwrap_text_v value_name_func in
    let names_param =
      value_names_param |> unwrap_list_v |> List.map unwrap_text_v
    in
    let value_ctx, value_arch, value_callResult =
      match (name_func, names_param) with
      | "verify", [ "check"; "toSignal" ] ->
          Core.Func.verify value_ctx value_arch
      | _ ->
          error_no_region
            ("unsupported extern function call: " ^ name_func ^ "("
            ^ String.concat ", " names_param
            ^ ")")
    in
    [ value_ctx; value_arch; value_callResult ]

  let eval_extern_method_call (values_input : Value.t list) : Value.t list =
    let value_ctx, value_arch, value_oid, value_name_method, value_names_param =
      match values_input with
      | [
       value_ctx; value_arch; value_oid; value_name_method; value_names_param;
      ] ->
          ( value_ctx,
            value_arch,
            value_oid,
            value_name_method,
            value_names_param )
      | _ ->
          error_no_region "unexpected number of arguments to extern method call"
    in
    let extern = get_extern value_arch value_oid in
    let name_method = unwrap_text_v value_name_method in
    let names_param =
      value_names_param |> unwrap_list_v |> List.map unwrap_text_v
    in
    let extern, value_ctx, value_arch, value_callResult =
      match (extern, name_method, names_param) with
      | PacketIn packet_in, "extract", [ "hdr" ] ->
          let packet_in, value_ctx, value_arch, value_callResult =
            Core.Object.PacketIn.extract value_ctx value_arch packet_in
          in
          let packet_in = PacketIn packet_in in
          (packet_in, value_ctx, value_arch, value_callResult)
      | ( PacketIn packet_in,
          "extract",
          [ "variableSizeHeader"; "variableFieldSizeInBits" ] ) ->
          let packet_in, value_ctx, value_arch, value_callResult =
            Core.Object.PacketIn.extract_varsize value_ctx value_arch packet_in
          in
          let packet_in = PacketIn packet_in in
          (packet_in, value_ctx, value_arch, value_callResult)
      | PacketIn packet_in, "lookahead", [] ->
          let packet_in, value_ctx, value_arch, value_callResult =
            Core.Object.PacketIn.lookahead value_ctx value_arch packet_in
          in
          let packet_in = PacketIn packet_in in
          (packet_in, value_ctx, value_arch, value_callResult)
      | PacketIn packet_in, "advance", [ "sizeInBits" ] ->
          let packet_in, value_ctx, value_arch, value_callResult =
            Core.Object.PacketIn.advance value_ctx value_arch packet_in
          in
          let packet_in = PacketIn packet_in in
          (packet_in, value_ctx, value_arch, value_callResult)
      | PacketIn packet_in, "length", [] ->
          let packet_in, value_ctx, value_arch, value_callResult =
            Core.Object.PacketIn.length value_ctx value_arch packet_in
          in
          let packet_in = PacketIn packet_in in
          (packet_in, value_ctx, value_arch, value_callResult)
      | CounterArray counter_array, "increment", [ "index" ] ->
          let counter_array, value_ctx, value_arch, value_callResult =
            Object.CounterArray.increment value_ctx value_arch counter_array
          in
          let counter_array = CounterArray counter_array in
          (counter_array, value_ctx, value_arch, value_callResult)
      | CounterArray counter_array, "add", [ "index"; "value" ] ->
          let counter_array, value_ctx, value_arch, value_callResult =
            Object.CounterArray.add value_ctx value_arch counter_array
          in
          let counter_array = CounterArray counter_array in
          (counter_array, value_ctx, value_arch, value_callResult)
      | _ ->
          let oid =
            value_oid |> unwrap_list_v |> List.map unwrap_text_v
            |> String.concat "."
          in
          error_no_region
            ("unsupported extern method call: " ^ oid ^ "." ^ name_method ^ "("
            ^ String.concat ", " names_param
            ^ ")")
    in
    let value_extern =
      extern |> extern_to_yojson |> wrap_extern_v "objectState"
    in
    let value_arch =
      Spec.Func.update_objectState_e value_arch value_oid value_extern
    in
    [ value_ctx; value_arch; value_callResult ]

  (* Match-action table interface *)

  let find_table (value_arch : Value.t) (value_tableName : Value.t) : Value.t =
    let find_table_unqualified table_name_unqualified =
      let value_tableName_unqualified = wrap_text_v table_name_unqualified in
      Spec.Func.find_object_unqualified_e value_arch value_tableName_unqualified
      |> Option.get
    in
    let table_name = unwrap_text_v value_tableName in
    match String.split_on_char '.' table_name with
    | [] -> assert false
    | [ table_name_unqualified ] ->
        find_table_unqualified table_name_unqualified
    | names -> (
        let values_name = List.map wrap_text_v names in
        let value_objectId = wrap_list_v "nameIR" values_name in
        match Spec.Func.find_object_qualified_e value_arch value_objectId with
        | Some value_table -> value_table
        | None ->
            let table_name_unqualified = names |> List.rev |> List.hd in
            find_table_unqualified table_name_unqualified)

  let update_table (value_arch : Value.t) (value_tableName : Value.t)
      (value_tableObject : Value.t) : Value.t =
    let update_table_unqualified table_name_unqualified =
      let value_tableName_unqualified = wrap_text_v table_name_unqualified in
      Spec.Func.update_object_unqualified_e value_arch
        value_tableName_unqualified value_tableObject
    in
    let table_name = unwrap_text_v value_tableName in
    match String.split_on_char '.' table_name with
    | [] -> assert false
    | [ table_name_unqualified ] ->
        update_table_unqualified table_name_unqualified
    | names ->
        let values_name = List.map wrap_text_v names in
        let value_objectId = wrap_list_v "nameIR" values_name in
        if
          Spec.Func.find_object_qualified_e value_arch value_objectId
          |> Option.is_some
        then
          Spec.Func.update_object_qualified_e value_arch value_objectId
            value_tableObject
        else
          let table_name_unqualified = names |> List.rev |> List.hd in
          update_table_unqualified table_name_unqualified

  let table_add_entry (value_ctx : Value.t) (value_arch : Value.t)
      (value_tableName : Value.t) (value_tableEntryPriorityInterface : Value.t)
      (value_tableKeysetInterface : Value.t)
      (value_tableActionInterface : Value.t) : Value.t =
    (* Lookup table object *)
    let value_tableObject = find_table value_arch value_tableName in
    (* Add entry to table object *)
    let value_tableObject =
      Spec.Func.tableObject_add_entry value_ctx value_tableObject
        value_tableEntryPriorityInterface value_tableKeysetInterface
        value_tableActionInterface
    in
    (* Update arch with modified table object *)
    update_table value_arch value_tableName value_tableObject

  let table_add_default_action (value_ctx : Value.t) (value_arch : Value.t)
      (value_tableName : Value.t) (value_tableActionInterface : Value.t) :
      Value.t =
    (* Lookup table object *)
    let value_tableObject = find_table value_arch value_tableName in
    (* Add entry to table object *)
    let value_tableObject =
      Spec.Func.tableObject_add_default_action value_ctx value_tableObject
        value_tableActionInterface
    in
    (* Update arch with modified table object *)
    update_table value_arch value_tableName value_tableObject

  (* Mirror session interface *)

  let add_mirror_session _session _port =
    error_no_region
      "add_mirror_session is not implemented for the ebpf simulator"

  let add_mirror_session_mc _session _multicast_group =
    error_no_region
      "add_mirror_session_mc is not implemented for the ebpf simulator"

  let mc_mgrp_create (_value_arch : Value.t) (_mgid : int) : Value.t =
    error_no_region "mc_mgrp_create is not implemented for the ebpf simulator"

  let mc_node_create (_value_arch : Value.t) (_rid : int) (_port : int list) :
      Value.t =
    error_no_region "mc_node_create is not implemented for the ebpf simulator"

  let mc_node_associate (_value_arch : Value.t) (_mgid : int) (_handle : int) :
      Value.t =
    error_no_region
      "mc_node_associate is not implemented for the ebpf simulator"

  (* Register interface *)

  let register_read (_value_arch : Value.t) (_reg_name : string) (_index : int)
      : Value.t =
    error_no_region "register_read is not implemented for the ebpf simulator"

  let register_write (_value_arch : Value.t) (_reg_name : string) (_index : int)
      (_value : int) : Value.t =
    error_no_region "register_write is not implemented for the ebpf simulator"

  let register_reset (_value_arch : Value.t) (_reg_name : string) : Value.t =
    error_no_region "register_reset is not implemented for the ebpf simulator"

  (* Pipeline initializer *)

  let init_pipe (includes_p4 : string list) (filename_p4 : string) :
      Value.t * Value.t =
    let program_result =
      match !mode with
      | IL_mode -> Interp_IL.eval_program "EBPF_init" includes_p4 filename_p4
      | SL_mode -> Interp_SL.eval_program "EBPF_init" includes_p4 filename_p4
      | Empty_mode -> assert false
    in
    match program_result with
    | Pass [ value_ctx; value_arch ] -> (value_ctx, value_arch)
    | Pass _ -> error_no_region "unexpected return from EBPF_init"
    | Fail (`Syntax (at, msg)) | Fail (`Runtime (at, msg)) -> error at msg

  (* Pipeline driver *)

  let setup_rx (value_ctx : Value.t) (value_arch : Value.t) (rx : IO.rx) :
      Value.t * Value.t =
    let _, packet_in = rx in
    (* Setup packet_in extern *)
    let packet_in = PacketIn (Core.Object.PacketIn.init packet_in) in
    let packet_in_state = extern_to_yojson packet_in in
    let value_packet_in_state = wrap_extern_v "objectState" packet_in_state in
    let value_ctx, value_arch =
      Spec.Rel.ebpf_init_packet_in value_ctx value_arch value_packet_in_state
    in
    (* Setup global variables *)
    let value_ctx = Spec.Rel.ebpf_init_globals value_ctx value_arch in
    (value_ctx, value_arch)

  let drive_prs (value_ctx : Value.t) (value_arch : Value.t) :
      Value.t * Value.t * bool =
    let value_ctx, value_arch, value_parse_result =
      Spec.Rel.ebpf_parse value_ctx value_arch
    in
    let drop =
      match flatten_case_v_opt value_parse_result with
      | Some (_, [ [ "REJECT" ]; [] ], [ _ ]) -> true
      | Some _ -> false
      | None -> assert false
    in
    (value_ctx, value_arch, drop)

  let drive_filt (value_ctx : Value.t) (value_arch : Value.t) :
      Value.t * Value.t * Value.t =
    Spec.Rel.ebpf_filter value_ctx value_arch

  let drive_pipe (value_ctx : Value.t) (value_arch : Value.t) (rx : IO.rx) :
      Value.t * Value.t * IO.tx list =
    (* Setup packet *)
    let value_ctx, value_arch = setup_rx value_ctx value_arch rx in
    (* Parse block *)
    let value_ctx, value_arch, drop = drive_prs value_ctx value_arch in
    if drop then (value_ctx, value_arch, [])
    else
      (* Filter block *)
      let value_ctx, value_arch, _value_filter_result =
        drive_filt value_ctx value_arch
      in
      (* Check if packet is accepted *)
      let value_accept =
        Spec.Rel.lvalue_read_var_global value_ctx value_arch "accept"
      in
      let accept = unpack_p4_bool value_accept in
      if accept then (value_ctx, value_arch, [ rx ])
      else (value_ctx, value_arch, [])

  (* Initializer *)

  let init (mode_ : Sim.mode) : unit =
    init_mode mode_;
    init_call_rel ();
    init_call_func ()
end
