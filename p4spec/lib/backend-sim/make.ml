module Typ = Runtime.Type.Typ
module Value = Runtime.Value
open Runtime.Sim.Io
open Runtime.Sim.Signature
open Error
open Util.Source

(* Functor to create a SIM from ARCH and INTERP implementations *)

module Make
    (Interface : INTERFACE)
    (MakeArch : functor (Spec : Spec.S) -> ARCH with type vt = Spec.V.t)
    (MakeInterp_IL : functor
      (Interface : INTERFACE)
      (Extern : EXTERN)
      ()
      -> INTERP_IL)
    (MakeInterp_SL : functor
      (Interface : INTERFACE)
      (Extern : EXTERN)
      ()
      -> INTERP_SL) : SIM = struct
  (* Instantiations — two parallel extern stacks (C5 typed bypass).

     The IL/SL interpreters evaluate on [Value.t] natively, so they drive the
     [V_value] stack ([vt = Value.t]). The compiled ML interpreter passes typed
     [Obj.t] values smuggled through the [Value.t] interfaces, so it drives a
     second stack instantiated at [V_typed] ([vt = Obj.t]); its generated
     dispatch + extern wrappers [Obj.magic]-cast instead of marshalling. One
     simulator instance runs in exactly one mode, so [init_mode] registers only
     the matching stack's trampolines.

     [Spec_*] hold the trampolines that let [Arch_*]/[Table_*] call back into the
     interpreters. *)

  module Spec_v = Spec.Make (Val.V_value)
  module Arch_v = MakeArch (Spec_v)
  module Table_v = Table.Make (Val.V_value) (Spec_v.Func)

  module Spec_t = Spec.Make (Val_typed.V_typed)
  module Arch_t = MakeArch (Spec_t)
  module Table_t = Table.Make (Val_typed.V_typed) (Spec_t.Func)

  (* Active mode, set by [MakeExtern.init_mode] (the runner calls it from [init]);
     read by the extern dispatch and the STF runner to pick the stack. *)
  let sim_mode : mode ref = ref Empty_mode

  module MakeExtern
      (Interp_IL : INTERP_IL)
      (Interp_SL : INTERP_SL)
      (Interp_ML : INTERP_ML) : EXTERN = struct
    (* IL/SL trampolines: [Value.t] native, route to the matching interpreter. *)
    let call_func_v name typs values =
      (match !sim_mode with
      | IL_mode -> Interp_IL.eval_func name typs values
      | SL_mode -> Interp_SL.eval_func name typs values
      | _ -> assert false)
      |> function
      | Pass value -> value
      | Fail (at, msg) -> error at msg

    let call_rel_v name values =
      (match !sim_mode with
      | IL_mode -> Interp_IL.eval_rel name values
      | SL_mode -> Interp_SL.eval_rel name values
      | _ -> assert false)
      |> function
      | Pass values -> values
      | Fail (at, msg) -> error at msg

    (* ML trampolines: typed [Obj.t] smuggled through the [Value.t] dispatch. *)
    let call_func_t name typs (values : Obj.t list) : Obj.t =
      (match
         Interp_ML.eval_func name typs (Obj.magic values : Value.t list)
       with
      | Pass value -> value
      | Fail (at, msg) -> error at msg)
      |> Obj.magic

    let call_rel_t name (values : Obj.t list) : Obj.t list =
      (match Interp_ML.eval_rel name (Obj.magic values : Value.t list) with
      | Pass values -> values
      | Fail (at, msg) -> error at msg)
      |> Obj.magic

    (* Program init returns [Value.t * Value.t] for every mode (cold entry, not
       the hot path); the ML pair is typed [Obj.t] smuggled as [Value.t]. *)
    let call_pgm relname includes path =
      (match !sim_mode with
      | IL_mode -> Interp_IL.eval_program relname includes path
      | SL_mode -> Interp_SL.eval_program relname includes path
      | ML_mode -> Interp_ML.eval_program relname includes path
      | Empty_mode -> assert false)
      |> function
      | Pass [ value_ctx; value_arch ] -> (value_ctx, value_arch)
      | Pass _ -> error no_region "unexpected number of return values"
      | Fail (`Syntax (at, msg) | `Runtime (at, msg)) -> error at msg

    let init_mode mode_ =
      sim_mode := mode_;
      match mode_ with
      | ML_mode ->
          Spec_t.Func.register call_func_t;
          Spec_t.Rel.register call_rel_t;
          Spec_t.Pgm.register call_pgm
      | IL_mode | SL_mode ->
          Spec_v.Func.register call_func_v;
          Spec_v.Rel.register call_rel_v;
          Spec_v.Pgm.register call_pgm
      | Empty_mode -> assert false

    let checkpoint () : int = 0
    let seff (before : int) (after : int) : bool = before <> after
    let clear () = ()

    module Cache = struct
      let cache_on () = ()
      let cache_off () = ()
    end

    (* Extern relation/function evaluation crosses to the active stack's [Arch];
       for ML the [Value.t] args/results are typed [Obj.t] smuggled (the [Arch_t]
       externs [Obj.obj]-project them). *)
    let eval_extern_rel name args =
      match !sim_mode with
      | ML_mode -> Arch_t.eval_extern_rel name args
      | _ -> Arch_v.eval_extern_rel name args

    let eval_extern_func name typs args =
      match !sim_mode with
      | ML_mode -> Arch_t.eval_extern_func name typs args
      | _ -> Arch_v.eval_extern_func name typs args
  end

  include (
    Runner.Make.Make_rec (Interface) (MakeExtern) (MakeInterp_IL)
      (MakeInterp_SL)
      (Backend_ocaml.Spec_compiled.Make) :
        RUNNER)

  (* Logger *)

  let verbose = ref true
  let log (msg : string) : unit = if !verbose then print_endline msg

  (* STF test runner *)

  (* Find the first expect element that has the same output port,
     then compare packet output.
     Return matched element and the rest of the list, preserving order. *)

  let extract_matching_expect (tx : IO.tx) (expect_queue : IO.expect list) :
      (IO.expect * IO.expect list) option =
    let tx_port, _ = tx in
    let rec extract_matching_expect expects = function
      | [] -> None
      | expect_h :: expect_t ->
          let (expect_port, expect_packet), exact = expect_h in
          if expect_port = tx_port then
            if compare_tx ~exact tx (expect_port, expect_packet) then
              Some (expect_h, List.rev_append expects expect_t)
            else
              error_stf
                (Format.asprintf "expected %s but got %s"
                   (string_of_tx (expect_port, expect_packet))
                   (string_of_tx tx))
          else extract_matching_expect (expect_h :: expects) expect_t
    in
    extract_matching_expect [] expect_queue

  let on_tx_output (txs : IO.tx list) (tx_output_queue : IO.tx list)
      (expect_queue : IO.expect list) : IO.tx list * IO.expect list =
    match txs with
    (* Packet was dropped *)
    | [] -> (tx_output_queue, expect_queue)
    (* Packet was transmitted *)
    | tx_h :: tx_t -> (
        match extract_matching_expect tx_h expect_queue with
        | None ->
            (* No expected packet (yet) *)
            let tx_output_queue = tx_output_queue @ txs in
            (tx_output_queue, expect_queue)
        | Some (expect, expect_queue) ->
            let tx, _ = expect in
            Format.asprintf "[PASS] Transmitted %s" (string_of_tx tx) |> log;
            (tx_output_queue @ tx_t, expect_queue))

  let extract_matching_output (expect : IO.expect)
      (tx_output_queue : IO.tx list) : (IO.tx * IO.tx list) option =
    let (expect_port, expect_packet), exact = expect in
    let rec extract_matching_output txs = function
      | [] -> None
      | tx_h :: tx_t ->
          let tx_port, _ = tx_h in
          if expect_port = tx_port then
            if compare_tx ~exact tx_h (expect_port, expect_packet) then
              Some (tx_h, List.rev_append txs tx_t)
            else
              error_stf
                (Format.asprintf "expected %s but got %s"
                   (string_of_tx (expect_port, expect_packet))
                   (string_of_tx tx_h))
          else extract_matching_output (tx_h :: txs) tx_t
    in
    extract_matching_output [] tx_output_queue

  let on_tx_expect (expect : IO.expect) (tx_output_queue : IO.tx list)
      (expect_queue : IO.expect list) : IO.tx list * expect list =
    match extract_matching_output expect tx_output_queue with
    | None ->
        (* No output packet (yet) *)
        let expect_queue = expect_queue @ [ expect ] in
        (tx_output_queue, expect_queue)
    | Some (tx_output, tx_output_queue) ->
        Format.asprintf "[PASS] Transmitted %s" (string_of_tx tx_output) |> log;
        (tx_output_queue, expect_queue)

  (* STF test runner, factored over the value representation so it drives the
     [V_value] stack for IL/SL and the [V_typed] stack for ML. The table encoding
     uses [V.Make.*] (typed [make_case_typed] under [V_typed]); the packet/expect
     queue bookkeeping above is representation-independent and stays shared. *)
  module RunStf
      (V : Val.VAL)
      (A : ARCH with type vt = V.t)
      (T : sig
        val add_entry : V.t -> V.t -> V.t -> V.t -> V.t -> V.t -> V.t
        val add_default_action : V.t -> V.t -> V.t -> V.t -> V.t
      end) =
  struct
  let run_stf_stmt (value_ctx : V.t) (value_arch : V.t)
      (tx_output_queue : IO.tx list) (expect_queue : IO.expect list)
      (stmt_stf : Stf.Ast.stmt) :
      V.t * V.t * IO.tx list * IO.expect list =
    (* Apply architecture-specific STF transformation *)
    let stmt_stf = A.transform_stf_stmt stmt_stf in
    match stmt_stf with
    (* Packet I/O *)
    | Stf.Ast.Packet (port_in, packet_in) ->
        let port_in = int_of_string port_in in
        let packet_in = String.uppercase_ascii packet_in in
        let rx = (port_in, packet_in) in
        let value_ctx, value_arch, tx_outputs =
          A.drive_pipe value_ctx value_arch rx
        in
        let tx_output_queue, expect_queue =
          on_tx_output tx_outputs tx_output_queue expect_queue
        in
        (value_ctx, value_arch, tx_output_queue, expect_queue)
    | Stf.Ast.Expect (port_expect, packet_expect_opt, exact) ->
        let port_expect = int_of_string port_expect in
        let packet_expect = Option.value packet_expect_opt ~default:"" in
        let packet_expect = String.uppercase_ascii packet_expect in
        let expect = ((port_expect, packet_expect), exact) in
        let tx_output_queue, expect_queue =
          on_tx_expect expect tx_output_queue expect_queue
        in
        (value_ctx, value_arch, tx_output_queue, expect_queue)
    (* Match-action table updates *)
    | Stf.Ast.Add
        ( table_name,
          table_entry_priority_opt,
          table_entry_keys,
          table_entry_action,
          _ ) ->
        (* Encode name *)
        let value_tableName = table_name |> String.escaped |> V.Make.text in
        (* Encode priority *)
        let value_tableEntryPriorityInterface =
          table_entry_priority_opt
          |> Option.map (fun table_entry_priority ->
                 table_entry_priority |> Bigint.of_int |> V.Make.int)
          |> V.Make.opt (Typ.Make.opt Typ.Make.int)
        in
        (* Encode keys *)
        let typ_tableKeyInterface =
          Typ.Make.var ("tableKeyInterface" $ no_region) []
        in
        let typ_tableKeysetInterface = Typ.Make.list typ_tableKeyInterface in
        let value_tableKeysetInterface =
          table_entry_keys
          |> List.map (fun (table_entry_key : Stf.Ast.mtch) ->
                 let table_key_name, table_key_value = table_entry_key in
                 let table_key_name =
                   Stf.Print.convert_dollar_to_brackets table_key_name
                 in
                 let value_table_key_name = V.Make.text table_key_name in
                 let value_table_key_value =
                   match table_key_value with
                   | Num number ->
                       if String.starts_with ~prefix:"0x" number then
                         let number_base_len = String.length number - 2 in
                         let number_base =
                           String.sub number 2 number_base_len
                         in
                         V.Make.(
                           "`HEX text"
                           <| [ text number_base ]
                           <<| "tableKeyValueInterface")
                       else if String.starts_with ~prefix:"0b" number then
                         let number_base_len = String.length number - 2 in
                         let number_base =
                           String.sub number 2 number_base_len
                         in
                         V.Make.(
                           "`BIN text"
                           <| [ text number_base ]
                           <<| "tableKeyValueInterface")
                       else
                         V.Make.(
                           "`DEC text"
                           <| [ text number ]
                           <<| "tableKeyValueInterface")
                   | Slash (prefix, mask) ->
                       let value_prefix = V.Make.text prefix in
                       let mask = Bigint.of_int (int_of_string mask) in
                       let value_mask = V.Make.nat mask in
                       V.Make.(
                         "text `SLASH nat"
                         <| [ value_prefix; value_mask ]
                         <<| "tableKeyValueInterface")
                 in
                 V.Make.tuple typ_tableKeyInterface
                   [ value_table_key_name; value_table_key_value ])
          |> V.Make.list typ_tableKeysetInterface
        in
        (* Encode action *)
        let value_tableActionInterface =
          let table_action_name, table_action_args = table_entry_action in
          let typ_tableActionInterface =
            Typ.Make.var ("tableActionInterface" $ no_region) []
          in
          let typ_tableActionArgumentInterface =
            Typ.Make.var ("tableActionArgumentInterface" $ no_region) []
          in
          let typ_tableActionArgumentInterfaceList =
            Typ.Make.list typ_tableActionArgumentInterface
          in
          let value_table_action_name = V.Make.text table_action_name in
          let value_tableActionArgumentInterfaces =
            table_action_args
            |> List.map (fun (name, number) ->
                   let value_name = V.Make.text name in
                   let value_number =
                     number |> int_of_string |> Bigint.of_int |> V.Make.int
                   in
                   V.Make.tuple typ_tableActionArgumentInterface
                     [ value_name; value_number ])
            |> V.Make.list typ_tableActionArgumentInterfaceList
          in
          V.Make.tuple typ_tableActionInterface
            [ value_table_action_name; value_tableActionArgumentInterfaces ]
        in
        let value_arch =
          T.add_entry value_ctx value_arch value_tableName
            value_tableEntryPriorityInterface value_tableKeysetInterface
            value_tableActionInterface
        in
        (value_ctx, value_arch, tx_output_queue, expect_queue)
    | Stf.Ast.SetDefault (table_name, table_entry_action) ->
        (* Encode name *)
        let value_tableName = V.Make.text table_name in
        (* Encode action *)
        let value_tableActionInterface =
          let table_action_name, table_action_args = table_entry_action in
          let typ_tableActionInterface =
            Typ.Make.var ("tableActionInterface" $ no_region) []
          in
          let typ_tableActionArgumentInterface =
            Typ.Make.var ("tableActionArgumentInterface" $ no_region) []
          in
          let typ_tableActionArgumentInterfaceList =
            Typ.Make.list typ_tableActionArgumentInterface
          in
          let value_table_action_name = V.Make.text table_action_name in
          let value_tableActionArgumentInterfaces =
            table_action_args
            |> List.map (fun (name, number) ->
                   let value_name = V.Make.text name in
                   let value_number =
                     number |> int_of_string |> Bigint.of_int |> V.Make.int
                   in
                   V.Make.tuple typ_tableActionArgumentInterface
                     [ value_name; value_number ])
            |> V.Make.list typ_tableActionArgumentInterfaceList
          in
          V.Make.tuple typ_tableActionInterface
            [ value_table_action_name; value_tableActionArgumentInterfaces ]
        in
        let value_arch =
          T.add_default_action value_ctx value_arch value_tableName
            value_tableActionInterface
        in
        (value_ctx, value_arch, tx_output_queue, expect_queue)
    (* Mirror session updates *)
    | Stf.Ast.MirroringAdd (session, port) ->
        let session = int_of_string session in
        let port = int_of_string port in
        let value_arch = A.add_mirror_session value_arch session port in
        (value_ctx, value_arch, tx_output_queue, expect_queue)
    | Stf.Ast.MirroringAddMc (session, id) ->
        let session = int_of_string session in
        let id = int_of_string id in
        let value_arch = A.add_mirror_session_mc value_arch session id in
        (value_ctx, value_arch, tx_output_queue, expect_queue)
    | Stf.Ast.MirroringGet _session ->
        (value_ctx, value_arch, tx_output_queue, expect_queue)
    (* Multicast group updates *)
    | Stf.Ast.McGroupCreate mgid ->
        let mgid = int_of_string mgid in
        let value_arch = A.mc_mgrp_create value_arch mgid in
        (value_ctx, value_arch, tx_output_queue, expect_queue)
    | Stf.Ast.McNodeCreate (rid, ports) ->
        let rid = int_of_string rid in
        let ports = List.map int_of_string ports in
        let value_arch = A.mc_node_create value_arch rid ports in
        (value_ctx, value_arch, tx_output_queue, expect_queue)
    | Stf.Ast.McNodeAssociate (mgid, handle) ->
        let mgid = int_of_string mgid in
        let handle = int_of_string handle in
        let value_arch = A.mc_node_associate value_arch mgid handle in
        (value_ctx, value_arch, tx_output_queue, expect_queue)
    (* Register updates *)
    | Stf.Ast.RegisterRead (reg_name, index) ->
        let index = int_of_string index in
        let value_arch = A.register_read value_arch reg_name index in
        (value_ctx, value_arch, tx_output_queue, expect_queue)
    | Stf.Ast.RegisterWrite (reg_name, index, value) ->
        let index = int_of_string index in
        let value = int_of_string value in
        let value_arch = A.register_write value_arch reg_name index value in
        (value_ctx, value_arch, tx_output_queue, expect_queue)
    | Stf.Ast.RegisterReset reg_name ->
        let value_arch = A.register_reset value_arch reg_name in
        (value_ctx, value_arch, tx_output_queue, expect_queue)
    (* Async *)
    | Stf.Ast.Wait -> (value_ctx, value_arch, tx_output_queue, expect_queue)
    | _ ->
        error_stf
          (Format.asprintf "not yet supported: %a" Stf.Print.print_stmt stmt_stf)

  let run_stf_stmts (value_ctx : V.t) (value_arch : V.t)
      (stmts_stf : Stf.Ast.stmt list) : unit =
    let _, _, tx_output_queue, expect_queue =
      List.fold_left
        (fun (value_ctx, value_arch, tx_output_queue, expect_queue) stmt_stf ->
          run_stf_stmt value_ctx value_arch tx_output_queue expect_queue
            stmt_stf)
        (value_ctx, value_arch, [], [])
        stmts_stf
    in
    match (tx_output_queue, expect_queue) with
    | [], [] -> ()
    | tx_output_queue, expect_queue ->
        let msg_output =
          if tx_output_queue <> [] then
            Format.asprintf "[FAIL] Remaining packets to be matched:\n%s"
              (tx_output_queue |> List.map string_of_tx |> String.concat "\n")
          else ""
        in
        let msg_expect =
          if expect_queue <> [] then
            Format.asprintf "[FAIL] Expected packets to be output:\n%s"
              (expect_queue
              |> List.map (fun (tx, _) -> string_of_tx tx)
              |> String.concat "\n")
          else ""
        in
        error_stf (msg_output ^ msg_expect)

  let run_stf_test (includes_p4 : string list) (path_p4 : string)
      (path_stf : string) : stf_result =
    try
      let value_ctx, value_arch = A.init_pipe includes_p4 path_p4 in
      let stf_stmts = Stf.Parse.parse_file path_stf in
      run_stf_stmts value_ctx value_arch stf_stmts;
      Pass
    with
    | Util.Error.ParseError (at, msg) -> Fail (`Syntax (at, msg))
    | Util.Error.InterpError (at, msg) | Util.Error.ExternError (at, msg) ->
        Fail (`Runtime (at, msg))
    | Util.Error.StfError msg -> Fail (`Runtime (no_region, msg))
  end

  module RunStf_v = RunStf (Val.V_value) (Arch_v) (Table_v)
  module RunStf_t = RunStf (Val_typed.V_typed) (Arch_t) (Table_t)

  (* Dispatch to the stack matching the active mode (ML drives the typed stack). *)
  let run_stf_test (includes_p4 : string list) (path_p4 : string)
      (path_stf : string) : stf_result =
    match !sim_mode with
    | ML_mode -> RunStf_t.run_stf_test includes_p4 path_p4 path_stf
    | _ -> RunStf_v.run_stf_test includes_p4 path_p4 path_stf
end
