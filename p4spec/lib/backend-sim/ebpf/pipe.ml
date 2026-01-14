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

  (* Extern objects *)

  type extern =
    | PacketIn of Core.Object.PacketIn.t
    | CounterArray of Object.CounterArray.t
  [@@deriving yojson]

  let get_extern (value_sto : Value.t) (value_oid : Value.t) : extern =
    Spec.Func.find_store_externState value_sto value_oid
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
        counter_array |> extern_to_yojson |> wrap_extern_v "externState"
    | _ -> wrap_extern_v "externState" `Null

  let eval_extern_func_lctk_call (_values_input : Value.t list) : Value.t list =
    error_no_region
      "eval_extern_func_lctk_call not implemented for the placeholder simulator"

  let eval_extern_func_call (values_input : Value.t list) : Value.t list =
    let value_ctx, value_sto, value_name_func, value_names_param =
      match values_input with
      | [ value_ctx; value_sto; value_name_func; value_names_param ] ->
          (value_ctx, value_sto, value_name_func, value_names_param)
      | _ ->
          error_no_region
            "unexpected number of arguments to extern function call"
    in
    let name_func = unwrap_text_v value_name_func in
    let names_param =
      value_names_param |> unwrap_list_v |> List.map unwrap_text_v
    in
    let value_ctx, value_sto, value_callResult =
      match (name_func, names_param) with
      | "verify", [ "check"; "toSignal" ] ->
          Core.Func.verify value_ctx value_sto
      | _ ->
          error_no_region
            ("unsupported extern function call: " ^ name_func ^ "("
            ^ String.concat ", " names_param
            ^ ")")
    in
    [ value_ctx; value_sto; value_callResult ]

  let eval_extern_method_call (values_input : Value.t list) : Value.t list =
    let value_ctx, value_sto, value_oid, value_name_method, value_names_param =
      match values_input with
      | [
       value_ctx; value_sto; value_oid; value_name_method; value_names_param;
      ] ->
          (value_ctx, value_sto, value_oid, value_name_method, value_names_param)
      | _ ->
          error_no_region "unexpected number of arguments to extern method call"
    in
    let extern = get_extern value_sto value_oid in
    let name_method = unwrap_text_v value_name_method in
    let names_param =
      value_names_param |> unwrap_list_v |> List.map unwrap_text_v
    in
    let extern, value_ctx, value_sto, value_callResult =
      match (extern, name_method, names_param) with
      | PacketIn packet_in, "extract", [ "hdr" ] ->
          let packet_in, value_ctx, value_sto, value_callResult =
            Core.Object.PacketIn.extract value_ctx value_sto packet_in
          in
          let packet_in = PacketIn packet_in in
          (packet_in, value_ctx, value_sto, value_callResult)
      | ( PacketIn packet_in,
          "extract",
          [ "variableSizeHeader"; "variableFieldSizeInBits" ] ) ->
          let packet_in, value_ctx, value_sto, value_callResult =
            Core.Object.PacketIn.extract_varsize value_ctx value_sto packet_in
          in
          let packet_in = PacketIn packet_in in
          (packet_in, value_ctx, value_sto, value_callResult)
      | PacketIn packet_in, "lookahead", [] ->
          let packet_in, value_ctx, value_sto, value_callResult =
            Core.Object.PacketIn.lookahead value_ctx value_sto packet_in
          in
          let packet_in = PacketIn packet_in in
          (packet_in, value_ctx, value_sto, value_callResult)
      | PacketIn packet_in, "advance", [ "sizeInBits" ] ->
          let packet_in, value_ctx, value_sto, value_callResult =
            Core.Object.PacketIn.advance value_ctx value_sto packet_in
          in
          let packet_in = PacketIn packet_in in
          (packet_in, value_ctx, value_sto, value_callResult)
      | PacketIn packet_in, "length", [] ->
          let packet_in, value_ctx, value_sto, value_callResult =
            Core.Object.PacketIn.length value_ctx value_sto packet_in
          in
          let packet_in = PacketIn packet_in in
          (packet_in, value_ctx, value_sto, value_callResult)
      | CounterArray counter_array, "increment", [ "index" ] ->
          let counter_array, value_ctx, value_sto, value_callResult =
            Object.CounterArray.increment value_ctx value_sto counter_array
          in
          let counter_array = CounterArray counter_array in
          (counter_array, value_ctx, value_sto, value_callResult)
      | CounterArray counter_array, "add", [ "index"; "value" ] ->
          let counter_array, value_ctx, value_sto, value_callResult =
            Object.CounterArray.add value_ctx value_sto counter_array
          in
          let counter_array = CounterArray counter_array in
          (counter_array, value_ctx, value_sto, value_callResult)
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
      extern |> extern_to_yojson |> wrap_extern_v "externState"
    in
    let value_sto =
      Spec.Func.update_store_externState value_sto value_oid value_extern
    in
    [ value_ctx; value_sto; value_callResult ]

  (* Match-action table interface *)

  let find_table (value_sto : Value.t) (value_tableName : Value.t) : Value.t =
    let table_name = unwrap_text_v value_tableName in
    match String.split_on_char '.' table_name with
    | [] -> assert false
    | [ table_name_unqualified ] ->
        let value_tableName_unqualified = wrap_text_v table_name_unqualified in
        Spec.Func.find_store_unqualified value_sto value_tableName_unqualified
    | names ->
        let values_name = List.map wrap_text_v names in
        let value_objectId = wrap_list_v "nameIR" values_name in
        Spec.Func.find_store_qualified value_sto value_objectId

  let update_table (value_sto : Value.t) (value_tableName : Value.t)
      (value_tableObject : Value.t) : Value.t =
    let table_name = unwrap_text_v value_tableName in
    match String.split_on_char '.' table_name with
    | [] -> assert false
    | [ table_name_unqualified ] ->
        let value_tableName_unqualified = wrap_text_v table_name_unqualified in
        Spec.Func.update_store_unqualified value_sto value_tableName_unqualified
          value_tableObject
    | names ->
        let values_name = List.map wrap_text_v names in
        let value_objectId = wrap_list_v "nameIR" values_name in
        Spec.Func.update_store_qualified value_sto value_objectId
          value_tableObject

  let table_add_entry (value_sto : Value.t) (value_tableName : Value.t)
      (value_tableEntryPriorityInterface : Value.t)
      (value_tableKeysetInterface : Value.t)
      (value_tableActionInterface : Value.t) : Value.t =
    (* Lookup table object *)
    let value_tableObject = find_table value_sto value_tableName in
    (* Add entry to table object *)
    let value_tableObject =
      Spec.Func.tableObject_add_entry value_tableObject
        value_tableEntryPriorityInterface value_tableKeysetInterface
        value_tableActionInterface
    in
    (* Update store with modified table object *)
    update_table value_sto value_tableName value_tableObject

  (* Initializer *)

  let init (spec_ : Sim.spec) : unit =
    init_spec spec_;
    init_call_rel ();
    init_call_func ()

  (* Pipeline initializer *)

  let init_pipe (spec_ : Sim.spec) (includes_p4 : string list)
      (filename_p4 : string) : Value.t * Value.t =
    init spec_;
    let program_result =
      match !spec with
      | IL spec_il ->
          Interp_IL.eval_program spec_il "EBPF_init" includes_p4 filename_p4
      | SL spec_sl ->
          Interp_SL.eval_program spec_sl "EBPF_init" includes_p4 filename_p4
      | Empty -> assert false
    in
    match program_result with
    | Pass [ value_ctx; value_sto ] -> (value_ctx, value_sto)
    | Pass _ -> error_no_region "unexpected return from EBPF_init"
    | Fail (`Syntax (at, msg)) | Fail (`Runtime (at, msg)) -> error at msg

  (* Pipeline driver *)

  let setup_rx (value_ctx : Value.t) (value_sto : Value.t) (rx : IO.rx) :
      Value.t * Value.t =
    let _, packet_in = rx in
    (* Setup packet_in extern *)
    let packet_in = PacketIn (Core.Object.PacketIn.init packet_in) in
    let packet_in_state = extern_to_yojson packet_in in
    let value_packet_in_state = wrap_extern_v "externState" packet_in_state in
    let value_ctx, value_sto =
      Spec.Rel.ebpf_init_packet_in value_ctx value_sto value_packet_in_state
    in
    (* Setup global variables *)
    let value_ctx = Spec.Rel.ebpf_init_globals value_ctx value_sto in
    (value_ctx, value_sto)

  let drive_prs (value_ctx : Value.t) (value_sto : Value.t) :
      Value.t * Value.t * bool =
    let value_ctx, value_sto, value_parse_result =
      Spec.Rel.ebpf_parse value_ctx value_sto
    in
    let drop =
      match flatten_case_v_opt value_parse_result with
      | Some (_, [ [ "REJECT" ]; [] ], [ _ ]) -> true
      | Some _ -> false
      | None -> assert false
    in
    (value_ctx, value_sto, drop)

  let drive_filt (value_ctx : Value.t) (value_sto : Value.t) :
      Value.t * Value.t * Value.t =
    Spec.Rel.ebpf_filter value_ctx value_sto

  let drive_pipe (value_ctx : Value.t) (value_sto : Value.t) (rx : IO.rx) :
      Value.t * Value.t * IO.tx option =
    (* Setup packet *)
    let value_ctx, value_sto = setup_rx value_ctx value_sto rx in
    (* Parse block *)
    let value_ctx, value_sto, drop = drive_prs value_ctx value_sto in
    if drop then (value_ctx, value_sto, None)
    else
      (* Filter block *)
      let value_ctx, value_sto, _value_filter_result =
        drive_filt value_ctx value_sto
      in
      (* Check if packet is accepted *)
      let value_accept =
        Spec.Rel.lvalue_read_var_global value_ctx value_sto "accept"
      in
      let accept = unpack_p4_bool value_accept in
      if accept then (value_ctx, value_sto, Some rx)
      else (value_ctx, value_sto, None)
end
